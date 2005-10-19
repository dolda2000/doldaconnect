/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2004 Fredrik Tolf (fredrik@dolda2000.com)
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
/* XXX: Implement SOCKS proxyability */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/poll.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/signal.h>
#include <printf.h>
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif
#include <errno.h>
#include <net/if.h>

#include "conf.h"
#include "net.h"
#include "module.h"
#include "log.h"
#include "utils.h"
#include "sysevents.h"

static struct configvar myvars[] =
{
    /* 0 = Direct mode, 1 = Passive mode, 2 = SOCKS proxy */
    {CONF_VAR_INT, "mode", {.num = 0}},
    {CONF_VAR_BOOL, "reuseaddr", {.num = 0}},
    /* Only for direct mode */
    {CONF_VAR_IPV4, "visibleipv4", {.ipv4 = {0}}},
    {CONF_VAR_STRING, "publicif", {.str = L""}},
    {CONF_VAR_END}
};

static struct socket *sockets = NULL;
int numsocks = 0;

/* XXX: Get autoconf for all this... */
int getpublicaddr(int af, struct sockaddr **addr, socklen_t *lenbuf)
{
    struct sockaddr_in *ipv4;
    struct configvar *var;
    void *bufend;
    int sock;
    struct ifconf conf;
    struct ifreq *ifr, req;
    char *pif;
    
    if(af == AF_INET)
    {
	var = confgetvar("net", "visibleipv4");
	if(var->val.ipv4.s_addr != 0)
	{
	    ipv4 = smalloc(sizeof(*ipv4));
	    ipv4->sin_family = AF_INET;
	    ipv4->sin_addr.s_addr = var->val.ipv4.s_addr;
	    *addr = (struct sockaddr *)ipv4;
	    *lenbuf = sizeof(*ipv4);
	    return(0);
	}
	if((pif = icwcstombs(confgetstr("net", "publicif"), NULL)) == NULL)
	{
	    flog(LOG_ERR, "could not convert net.publicif into local charset: %s", strerror(errno));
	    return(-1);
	}
	if((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
	    return(-1);
	conf.ifc_buf = smalloc(conf.ifc_len = 65536);
	if(ioctl(sock, SIOCGIFCONF, &conf) < 0)
	{
	    free(conf.ifc_buf);
	    close(sock);
	    return(-1);
	}
	bufend = ((char *)conf.ifc_buf) + conf.ifc_len;
	ipv4 = NULL;
	for(ifr = conf.ifc_ifcu.ifcu_req; (void *)ifr < bufend; ifr++)
	{
	    memset(&req, 0, sizeof(req));
	    memcpy(req.ifr_name, ifr->ifr_name, sizeof(ifr->ifr_name));
	    if(ioctl(sock, SIOCGIFFLAGS, &req) < 0)
	    {
		free(conf.ifc_buf);
		close(sock);
		return(-1);
	    }
	    if(!(req.ifr_flags & IFF_UP))
		continue;
	    if(ifr->ifr_addr.sa_family == AF_INET)
	    {
		if(ntohl(((struct sockaddr_in *)&ifr->ifr_addr)->sin_addr.s_addr) == 0x7f000001)
		    continue;
		if(ipv4 == NULL)
		{
		    ipv4 = smalloc(sizeof(*ipv4));
		    memcpy(ipv4, &ifr->ifr_addr, sizeof(ifr->ifr_addr));
		} else {
		    free(ipv4);
		    flog(LOG_WARNING, "could not locate an unambiguous interface for determining your public IP address - set net.publicif");
		    errno = ENFILE; /* XXX: There's no appropriate one for this... */
		    return(-1);
		}
	    }
	}
	close(sock);
	if(ipv4 != NULL)
	{
	    *addr = (struct sockaddr *)ipv4;
	    *lenbuf = sizeof(*ipv4);
	    return(0);
	}
	errno = ENETDOWN;
	return(-1);
    }
    errno = EPFNOSUPPORT;
    return(-1);
}

static struct socket *newsock(int type)
{
    struct socket *new;
    
    new = smalloc(sizeof(*new));
    new->refcount = 2;
    new->fd = -1;
    new->isrealsocket = 1;
    new->family = -1;
    new->tos = 0;
    new->type = type;
    new->state = -1;
    new->ignread = 0;
    new->close = 0;
    new->remote = NULL;
    new->remotelen = 0;
    switch(type)
    {
    case SOCK_STREAM:
	new->outbuf.s.buf = NULL;
	new->outbuf.s.bufsize = 0;
	new->outbuf.s.datasize = 0;
	new->inbuf.s.buf = NULL;
	new->inbuf.s.bufsize = 0;
	new->inbuf.s.datasize = 0;
	break;
    case SOCK_DGRAM:
	new->outbuf.d.f = new->outbuf.d.l = NULL;
	new->inbuf.d.f = new->inbuf.d.l = NULL;
	break;
    }
    new->conncb = NULL;
    new->errcb = NULL;
    new->readcb = NULL;
    new->writecb = NULL;
    new->acceptcb = NULL;
    new->next = sockets;
    new->prev = NULL;
    if(sockets != NULL)
	sockets->prev = new;
    sockets = new;
    numsocks++;
    return(new);
}

static struct socket *mksock(int domain, int type)
{
    int fd;
    struct socket *new;
    
    if((fd = socket(domain, type, 0)) < 0)
    {
	flog(LOG_CRIT, "could not create socket: %s", strerror(errno));
	return(NULL);
    }
    new = newsock(type);
    new->fd = fd;
    new->family = domain;
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    return(new);
}

struct socket *wrapsock(int fd)
{
    struct socket *new;
    
    new = newsock(SOCK_STREAM);
    new->fd = fd;
    new->state = SOCK_EST;
    new->isrealsocket = 0;
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    return(new);
}

static void unlinksock(struct socket *sk)
{
    if(sk->prev != NULL)
	sk->prev->next = sk->next;
    if(sk->next != NULL)
	sk->next->prev = sk->prev;
    if(sk == sockets)
	sockets = sk->next;
    putsock(sk);
    numsocks--;
}

void getsock(struct socket *sk)
{
    sk->refcount++;
}

void putsock(struct socket *sk)
{
    struct dgrambuf *buf;
    
    if(--(sk->refcount) == 0)
    {
	switch(sk->type)
	{
	case SOCK_STREAM:
	    if(sk->outbuf.s.buf != NULL)
		free(sk->outbuf.s.buf);
	    if(sk->inbuf.s.buf != NULL)
		free(sk->inbuf.s.buf);
	    break;
	case SOCK_DGRAM:
	    while((buf = sk->outbuf.d.f) != NULL)
	    {
		sk->outbuf.d.f = buf->next;
		free(buf->data);
		free(buf);
	    }
	    while((buf = sk->inbuf.d.f) != NULL)
	    {
		sk->inbuf.d.f = buf->next;
		free(buf->data);
		free(buf);
	    }
	    break;
	}
	if(sk->fd >= 0)
	    close(sk->fd);
	if(sk->remote != NULL)
	    free(sk->remote);
	free(sk);
    }
}

void *sockgetinbuf(struct socket *sk, size_t *size)
{
    void *buf;
    struct dgrambuf *dbuf;
    
    switch(sk->type)
    {
    case SOCK_STREAM:
	if((sk->inbuf.s.buf == NULL) || (sk->inbuf.s.datasize == 0))
	{
	    *size = 0;
	    return(NULL);
	}
	buf = sk->inbuf.s.buf;
	*size = sk->inbuf.s.datasize;
	sk->inbuf.s.buf = NULL;
	sk->inbuf.s.bufsize = sk->inbuf.s.datasize = 0;
	return(buf);
    case SOCK_DGRAM:
	if((dbuf = sk->inbuf.d.f) == NULL)
	    return(NULL);
	sk->inbuf.d.f = dbuf->next;
	if(dbuf->next == NULL)
	    sk->inbuf.d.l = NULL;
	buf = dbuf->data;
	*size = dbuf->size;
	free(dbuf->addr);
	free(dbuf);
	return(buf);
    }
    return(NULL);
}

static void sockrecv(struct socket *sk)
{
    int ret, inq;
    struct dgrambuf *dbuf;
    
    switch(sk->type)
    {
    case SOCK_STREAM:
#if defined(HAVE_LINUX_SOCKIOS_H) && defined(SIOCINQ)
	/* SIOCINQ is Linux-specific AFAIK, but I really have no idea
	 * how to read the inqueue size on other OSs */
	if(ioctl(sk->fd, SIOCINQ, &inq))
	{
	    /* I don't really know what could go wrong here, so let's
	     * assume it's transient. */
	    flog(LOG_WARNING, "SIOCINQ return %s on socket %i, falling back to 2048 bytes", strerror(errno), sk->fd);
	    inq = 2048;
	}
#else
	inq = 2048;
#endif
	if(inq > 65536)
	    inq = 65536;
	sizebuf(&sk->inbuf.s.buf, &sk->inbuf.s.bufsize, sk->inbuf.s.datasize + inq, 1, 1);
	ret = read(sk->fd, sk->inbuf.s.buf + sk->inbuf.s.datasize, inq);
	if(ret < 0)
	{
	    if((errno == EINTR) || (errno == EAGAIN))
		return;
	    if(sk->errcb != NULL)
		sk->errcb(sk, errno, sk->data);
	    closesock(sk);
	    return;
	}
	if(ret == 0)
	{
	    if(sk->errcb != NULL)
		sk->errcb(sk, 0, sk->data);
	    closesock(sk);
	    return;
	}
	sk->inbuf.s.datasize += ret;
	if(sk->readcb != NULL)
	    sk->readcb(sk, sk->data);
	break;
    case SOCK_DGRAM:
	if(ioctl(sk->fd, SIOCINQ, &inq))
	{
	    /* I don't really know what could go wrong here, so let's
	     * assume it's transient. */
	    flog(LOG_WARNING, "SIOCINQ return %s on socket %i", strerror(errno), sk->fd);
	    return;
	}
	dbuf = smalloc(sizeof(*dbuf));
	dbuf->data = smalloc(inq);
	dbuf->addr = smalloc(dbuf->addrlen = sizeof(struct sockaddr_storage));
	ret = recvfrom(sk->fd, dbuf->data, inq, 0, dbuf->addr, &dbuf->addrlen);
	if(ret < 0)
	{
	    free(dbuf->addr);
	    free(dbuf->data);
	    free(dbuf);
	    if((errno == EINTR) || (errno == EAGAIN))
		return;
	    if(sk->errcb != NULL)
		sk->errcb(sk, errno, sk->data);
	    closesock(sk);
	    return;
	}
	/* On UDP/IPv[46], ret == 0 doesn't mean EOF (since UDP can't
	 * have EOF), but rather an empty packet. I don't know if any
	 * other potential DGRAM protocols might have an EOF
	 * condition, so let's play safe. */
	if(ret == 0)
	{
	    free(dbuf->addr);
	    free(dbuf->data);
	    free(dbuf);
	    if(!((sk->family == AF_INET) || (sk->family == AF_INET6)))
	    {
		if(sk->errcb != NULL)
		    sk->errcb(sk, 0, sk->data);
		closesock(sk);
	    }
	    return;
	}
	dbuf->addr = srealloc(dbuf->addr, dbuf->addrlen);
	dbuf->data = srealloc(dbuf->data, dbuf->size = ret);
	dbuf->next = NULL;
	if(sk->inbuf.d.l != NULL)
	    sk->inbuf.d.l->next = dbuf;
	else
	    sk->inbuf.d.f = dbuf;
	sk->inbuf.d.l = dbuf;
	if(sk->readcb != NULL)
	    sk->readcb(sk, sk->data);
	break;
    }
}

static void sockflush(struct socket *sk)
{
    int ret;
    struct dgrambuf *dbuf;
    
    switch(sk->type)
    {
    case SOCK_STREAM:
	if(sk->isrealsocket)
	    ret = send(sk->fd, sk->outbuf.s.buf, sk->outbuf.s.datasize, MSG_DONTWAIT | MSG_NOSIGNAL);
	else
	    ret = write(sk->fd, sk->outbuf.s.buf, sk->outbuf.s.datasize);
	if(ret < 0)
	{
	    /* For now, assume transient error, since
	     * the socket is polled for errors */
	    break;
	}
	if(ret > 0)
	{
	    memmove(sk->outbuf.s.buf, ((char *)sk->outbuf.s.buf) + ret, sk->outbuf.s.datasize -= ret);
	    if(sk->writecb != NULL)
		sk->writecb(sk, sk->data);
	}
	break;
    case SOCK_DGRAM:
	dbuf = sk->outbuf.d.f;
	if((sk->outbuf.d.f = dbuf->next) == NULL)
	    sk->outbuf.d.l = NULL;
	sendto(sk->fd, dbuf->data, dbuf->size, MSG_DONTWAIT | MSG_NOSIGNAL, dbuf->addr, dbuf->addrlen);
	free(dbuf->data);
	free(dbuf->addr);
	free(dbuf);
	if(sk->writecb != NULL)
	    sk->writecb(sk, sk->data);
	break;
    }
}

void closesock(struct socket *sk)
{
    sk->state = SOCK_STL;
    close(sk->fd);
    sk->fd = -1;
    sk->close = 0;
}

void sockqueue(struct socket *sk, void *data, size_t size)
{
    struct dgrambuf *new;
    
    if(sk->state == SOCK_STL)
	return;
    switch(sk->type)
    {
    case SOCK_STREAM:
	sizebuf(&(sk->outbuf.s.buf), &(sk->outbuf.s.bufsize), sk->outbuf.s.datasize + size, 1, 1);
	memcpy(sk->outbuf.s.buf + sk->outbuf.s.datasize, data, size);
	sk->outbuf.s.datasize += size;
	break;
    case SOCK_DGRAM:
	if(sk->remote == NULL)
	    return;
	new = smalloc(sizeof(*new));
	new->next = NULL;
	memcpy(new->data = smalloc(size), data, new->size = size);
	memcpy(new->addr = smalloc(sk->remotelen), sk->remote, new->addrlen = sk->remotelen);
	if(sk->outbuf.d.l == NULL)
	{
	    sk->outbuf.d.l = sk->outbuf.d.f = new;
	} else {
	    sk->outbuf.d.l->next = new;
	    sk->outbuf.d.l = new;
	}
	break;
    }
}

size_t sockgetdatalen(struct socket *sk)
{
    struct dgrambuf *b;
    size_t ret;
    
    switch(sk->type)
    {
    case SOCK_STREAM:
	ret = sk->inbuf.s.datasize;
	break;
    case SOCK_DGRAM:
	ret = 0;
	for(b = sk->inbuf.d.f; b != NULL; b = b->next)
	    ret += b->size;
	break;
    }
    return(ret);
}

size_t sockqueuesize(struct socket *sk)
{
    struct dgrambuf *b;
    size_t ret;
    
    switch(sk->type)
    {
    case SOCK_STREAM:
	ret = sk->outbuf.s.datasize;
	break;
    case SOCK_DGRAM:
	ret = 0;
	for(b = sk->outbuf.d.f; b != NULL; b = b->next)
	    ret += b->size;
	break;
    }
    return(ret);
}

struct socket *netcslisten(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct socket *, struct socket *, void *), void *data)
{
    struct socket *sk;
    int intbuf;
    
    if(confgetint("net", "mode") == 1)
    {
	errno = EOPNOTSUPP;
	return(NULL);
    }
    /* I don't know if this is actually correct (it probably isn't),
     * but since, at on least Linux systems, PF_* are specifically
     * #define'd to their AF_* counterparts, it allows for a severely
     * smoother implementation. If it breaks something on your
     * platform, please tell me so.
     */
    if(confgetint("net", "mode") == 0)
    {
	if((sk = mksock(name->sa_family, type)) == NULL)
	    return(NULL);
	sk->state = SOCK_LST;
	if(confgetint("net", "reuseaddr"))
	{
	    intbuf = 1;
	    setsockopt(sk->fd, SOL_SOCKET, SO_REUSEADDR, &intbuf, sizeof(intbuf));
	}
	if(bind(sk->fd, name, namelen) < 0)
	{
	    putsock(sk);
	    return(NULL);
	}
	if(listen(sk->fd, 16) < 0)
	{
	    putsock(sk);
	    return(NULL);
	}
	sk->acceptcb = func;
	sk->data = data;
	return(sk);
    }
    errno = EOPNOTSUPP;
    return(NULL);
}

/*
 * The difference between netcslisten() and netcslistenlocal() is that
 * netcslistenlocal() always listens on the local host, instead of
 * following proxy/passive mode directions. It is suitable for eg. the
 * UI channel, while the file sharing networks should, naturally, use
 * netcslisten() instead.
*/

struct socket *netcslistenlocal(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct socket *, struct socket *, void *), void *data)
{
    struct socket *sk;
    int intbuf;
    
    /* I don't know if this is actually correct (it probably isn't),
     * but since, at on least Linux systems, PF_* are specifically
     * #define'd to their AF_* counterparts, it allows for a severely
     * smoother implementation. If it breaks something on your
     * platform, please tell me so.
     */
    if((sk = mksock(name->sa_family, type)) == NULL)
	return(NULL);
    sk->state = SOCK_LST;
    if(confgetint("net", "reuseaddr"))
    {
	intbuf = 1;
	setsockopt(sk->fd, SOL_SOCKET, SO_REUSEADDR, &intbuf, sizeof(intbuf));
    }
    if(bind(sk->fd, name, namelen) < 0)
    {
	putsock(sk);
	return(NULL);
    }
    if(listen(sk->fd, 16) < 0)
    {
	putsock(sk);
	return(NULL);
    }
    sk->acceptcb = func;
    sk->data = data;
    return(sk);
}

struct socket *netcsdgram(struct sockaddr *name, socklen_t namelen)
{
    struct socket *sk;
    int mode;
    
    mode = confgetint("net", "mode");
    if((mode == 0) || (mode == 1))
    {
	if((sk = mksock(name->sa_family, SOCK_DGRAM)) == NULL)
	    return(NULL);
	if(bind(sk->fd, name, namelen) < 0)
	{
	    putsock(sk);
	    return(NULL);
	}
	sk->state = SOCK_EST;
	return(sk);
    }
    errno = EOPNOTSUPP;
    return(NULL);
}

struct socket *netdupsock(struct socket *sk)
{
    struct socket *newsk;
    
    newsk = newsock(sk->type);
    if((newsk->fd = dup(sk->fd)) < 0)
    {
	flog(LOG_WARNING, "could not dup() socket: %s", strerror(errno));
	putsock(newsk);
	return(NULL);
    }
    newsk->state = sk->state;
    newsk->ignread = sk->ignread;
    if(sk->remote != NULL)
	memcpy(newsk->remote = smalloc(sk->remotelen), sk->remote, newsk->remotelen = sk->remotelen);
    return(newsk);
}

void netdgramconn(struct socket *sk, struct sockaddr *addr, socklen_t addrlen)
{
    if(sk->remote != NULL)
	free(sk->remote);
    memcpy(sk->remote = smalloc(addrlen), addr, sk->remotelen = addrlen);
    sk->ignread = 1;
}

struct socket *netcsconn(struct sockaddr *addr, socklen_t addrlen, void (*func)(struct socket *, int, void *), void *data)
{
    struct socket *sk;
    int mode;
    
    mode = confgetint("net", "mode");
    if((mode == 0) || (mode == 1))
    {
	if((sk = mksock(addr->sa_family, SOCK_STREAM)) == NULL)
	    return(NULL);
	memcpy(sk->remote = smalloc(addrlen), addr, sk->remotelen = addrlen);
	if(!connect(sk->fd, addr, addrlen))
	{
	    sk->state = SOCK_EST;
	    func(sk, 0, data);
	    return(sk);
	}
	if(errno == EINPROGRESS)
	{
	    sk->state = SOCK_SYN;
	    sk->conncb = func;
	    sk->data = data;
	    return(sk);
	}
	putsock(sk);
	return(NULL);
    }
    errno = EOPNOTSUPP;
    return(NULL);
}

int pollsocks(int timeout)
{
    int i, num, ret, retlen;
    int newfd;
    struct pollfd *pfds;
    struct socket *sk, *next, *newsk;
    struct sockaddr_storage ss;
    socklen_t sslen;
    
    pfds = smalloc(sizeof(*pfds) * (num = numsocks));
    for(i = 0, sk = sockets; i < num; sk = sk->next)
    {
	if(sk->state == SOCK_STL)
	{
	    num--;
	    continue;
	}
	pfds[i].fd = sk->fd;
	pfds[i].events = 0;
	if(!sk->ignread)
	    pfds[i].events |= POLLIN;
	if((sk->state == SOCK_SYN) || (sockqueuesize(sk) > 0))
	    pfds[i].events |= POLLOUT;
	pfds[i].revents = 0;
	i++;
    }
    ret = poll(pfds, num, timeout);
    if(ret < 0)
    {
	if(errno != EINTR)
	{
	    flog(LOG_CRIT, "pollsocks: poll errored out: %s", strerror(errno));
	    /* To avoid CPU hogging in case it's bad, which it
	     * probably is. */
	    sleep(1);
	}
	free(pfds);
	return(1);
    }
    for(sk = sockets; sk != NULL; sk = next)
    {
	next = sk->next;
	for(i = 0; i < num; i++)
	{
	    if(pfds[i].fd == sk->fd)
		break;
	}
	if(i == num)
	    continue;
	switch(sk->state)
	{
	case SOCK_LST:
	    if(pfds[i].revents & POLLIN)
	    {
		sslen = sizeof(ss);
		if((newfd = accept(sk->fd, (struct sockaddr *)&ss, &sslen)) < 0)
		{
		    if(sk->errcb != NULL)
			sk->errcb(sk, errno, sk->data);
		}
		newsk = newsock(sk->type);
		newsk->fd = newfd;
		newsk->family = sk->family;
		newsk->state = SOCK_EST;
		memcpy(newsk->remote = smalloc(sslen), &ss, sslen);
		newsk->remotelen = sslen;
		putsock(newsk);
		if(sk->acceptcb != NULL)
		    sk->acceptcb(sk, newsk, sk->data);
	    }
	    if(pfds[i].revents & POLLERR)
	    {
		retlen = sizeof(ret);
		getsockopt(sk->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->errcb != NULL)
		    sk->errcb(sk, ret, sk->data);
		continue;
	    }
	    break;
	case SOCK_SYN:
	    if(pfds[i].revents & POLLERR)
	    {
		retlen = sizeof(ret);
		getsockopt(sk->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->conncb != NULL)
		    sk->conncb(sk, ret, sk->data);
		closesock(sk);
		continue;
	    }
	    if(pfds[i].revents & (POLLIN | POLLOUT))
	    {
		sk->state = SOCK_EST;
		if(sk->conncb != NULL)
		    sk->conncb(sk, 0, sk->data);
	    }
	    break;
	case SOCK_EST:
	    if(pfds[i].revents & POLLERR)
	    {
		retlen = sizeof(ret);
		getsockopt(sk->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->errcb != NULL)
		    sk->errcb(sk, ret, sk->data);
		closesock(sk);
		continue;
	    }
	    if(pfds[i].revents & POLLIN)
		sockrecv(sk);
	    if(pfds[i].revents & POLLOUT)
	    {
		if(sockqueuesize(sk) > 0)
		    sockflush(sk);
	    }
	    break;
	}
	if(pfds[i].revents & POLLNVAL)
	{
	    flog(LOG_CRIT, "BUG: stale socket struct on fd %i", sk->fd);
	    sk->state = SOCK_STL;
	    unlinksock(sk);
	    continue;
	}
	if(pfds[i].revents & POLLHUP)
	{
	    if(sk->errcb != NULL)
		sk->errcb(sk, 0, sk->data);
	    closesock(sk);
	    unlinksock(sk);
	    continue;
	}
    }
    free(pfds);
    for(sk = sockets; sk != NULL; sk = next)
    {
	next = sk->next;
	if(sk->refcount == 1 && (sockqueuesize(sk) == 0))
	{
	    unlinksock(sk);
	    continue;
	}
	if(sk->close && (sockqueuesize(sk) == 0))
	    closesock(sk);
	if(sk->state == SOCK_STL)
	{
	    unlinksock(sk);
	    continue;
	}
    }
    return(1);
}

int socksettos(struct socket *sk, int tos)
{
    if(sk->family == AF_INET)
    {
	if(setsockopt(sk->fd, SOL_IP, IP_TOS, &tos, sizeof(tos)) < 0)
	{
	    flog(LOG_WARNING, "could not set sock TOS to %i: %s", tos, strerror(errno));
	    return(-1);
	}
	return(0);
    }
    /* XXX: How does the IPv6 traffic class work? */
    flog(LOG_WARNING, "could not set TOS on sock of family %i", sk->family);
    return(1);
}

struct resolvedata
{
    int fd;
    void (*callback)(struct sockaddr *addr, int addrlen, void *data);
    void *data;
    struct sockaddr_storage addr;
    int addrlen;
};

static void resolvecb(pid_t pid, int status, struct resolvedata *data)
{
    static char buf[80];
    int ret;
    struct sockaddr_in *ipv4;
    
    if(!status)
    {
	if((ret = read(data->fd, buf, sizeof(buf))) != 4)
	{
	    errno = ENONET;
	    data->callback(NULL, 0, data->data);
	} else {
	    ipv4 = (struct sockaddr_in *)&data->addr;
	    memcpy(&ipv4->sin_addr, buf, 4);
	    data->callback((struct sockaddr *)ipv4, sizeof(*ipv4), data->data);
	}
    } else {
	errno = ENONET;
	data->callback(NULL, 0, data->data);
    }
    close(data->fd);
    free(data);
}

int netresolve(char *addr, void (*callback)(struct sockaddr *addr, int addrlen, void *data), void *data)
{
    int i;
    char *p;
    int port;
    int pfd[2];
    pid_t child;
    struct resolvedata *rdata;
    struct sockaddr_in ipv4;
    struct hostent *he;
    sigset_t sigset;
    
    /* IPv4 */
    port = -1;
    if((p = strchr(addr, ':')) != NULL)
    {
	*p = 0;
	port = atoi(p + 1);
    }
    ipv4.sin_family = AF_INET;
    ipv4.sin_port = htons(port);
    if(inet_aton(addr, &ipv4.sin_addr))
    {
	callback((struct sockaddr *)&ipv4, sizeof(ipv4), data);
    } else {
	sigemptyset(&sigset);
	sigaddset(&sigset, SIGCHLD);
	sigprocmask(SIG_BLOCK, &sigset, NULL);
	if((pipe(pfd) < 0) || ((child = fork()) < 0))
	{
	    sigprocmask(SIG_UNBLOCK, &sigset, NULL);
	    return(-1);
	}
	if(child == 0)
	{
	    sigprocmask(SIG_UNBLOCK, &sigset, NULL);
	    for(i = 3; i < FD_SETSIZE; i++)
	    {
		if(i != pfd[1])
		    close(i);
	    }
	    signal(SIGALRM, SIG_DFL);
	    alarm(30);
	    if((he = gethostbyname(addr)) == NULL)
		exit(1);
	    write(pfd[1], he->h_addr_list[0], 4);
	    exit(0);
	} else {
	    close(pfd[1]);
	    fcntl(pfd[0], F_SETFL, fcntl(pfd[0], F_GETFL) | O_NONBLOCK);
	    rdata = smalloc(sizeof(*rdata));
	    rdata->fd = pfd[0];
	    rdata->callback = callback;
	    rdata->data = data;
	    memcpy(&rdata->addr, &ipv4, rdata->addrlen = sizeof(ipv4));
	    childcallback(child, (void (*)(pid_t, int, void *))resolvecb, rdata);
	    sigprocmask(SIG_UNBLOCK, &sigset, NULL);
	    return(1);
	}
    }
    return(0);
}

int sockgetlocalname(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    socklen_t len;
    struct sockaddr_storage name;
    
    *namebuf = NULL;
    if((sk->state == SOCK_STL) || (sk->fd < 0))
	return(-1);
    len = sizeof(name);
    if(getsockname(sk->fd, (struct sockaddr *)&name, &len) < 0)
    {
	flog(LOG_ERR, "BUG: alive socket with dead fd in sockgetlocalname");
	return(-1);
    }
    *namebuf = memcpy(smalloc(len), &name, len);
    *lenbuf = len;
    return(0);
}

int sockgetremotename(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    socklen_t len;
    struct sockaddr_storage name;
    struct sockaddr_in *ipv4;
    struct sockaddr *pname;
    socklen_t pnamelen;
    
    switch(confgetint("net", "mode"))
    {
    case 0:
	*namebuf = NULL;
	if((sk->state == SOCK_STL) || (sk->fd < 0))
	    return(-1);
	len = sizeof(name);
	if(getsockname(sk->fd, (struct sockaddr *)&name, &len) < 0)
	{
	    flog(LOG_ERR, "BUG: alive socket with dead fd in sockgetremotename");
	    return(-1);
	}
	if(name.ss_family == AF_INET)
	{
	    ipv4 = (struct sockaddr_in *)&name;
	    if(getpublicaddr(AF_INET, &pname, &pnamelen) < 0)
	    {
		flog(LOG_WARNING, "could not determine public IP address - strange things may happen");
		return(-1);
	    }
	    ipv4->sin_addr.s_addr = ((struct sockaddr_in *)pname)->sin_addr.s_addr;
	    free(pname);
	}
	*namebuf = memcpy(smalloc(len), &name, len);
	*lenbuf = len;
	return(0);
    case 1:
	errno = EOPNOTSUPP;
	return(-1);
    default:
	flog(LOG_CRIT, "unknown net mode %i active", confgetint("net", "mode"));
	errno = EOPNOTSUPP;
	return(-1);
    }
}

char *formataddress(struct sockaddr *arg, socklen_t arglen)
{
    struct sockaddr_un *UNIX; /* Some wise guy has #defined unix with
			       * lowercase letters to 1, so I do this
			       * instead. */
    struct sockaddr_in *ipv4;
#ifdef HAVE_IPV6
    struct sockaddr_in6 *ipv6;
#endif
    static char *ret = NULL;
    char buf[1024];
    
    if(ret != NULL)
	free(ret);
    ret = NULL;
    switch(arg->sa_family)
    {
    case AF_UNIX:
	UNIX = (struct sockaddr_un *)arg;
	ret = sprintf2("%s", UNIX->sun_path);
	break;
    case AF_INET:
	ipv4 = (struct sockaddr_in *)arg;
	if(inet_ntop(AF_INET, &ipv4->sin_addr, buf, sizeof(buf)) == NULL)
	    return(NULL);
	ret = sprintf2("%s:%i", buf, (int)ntohs(ipv4->sin_port));
	break;
#ifdef HAVE_IPV6
    case AF_INET6:
	ipv6 = (struct sockaddr_in6 *)arg;
	if(inet_ntop(AF_INET6, &ipv6->sin6_addr, buf, sizeof(buf)) == NULL)
	    return(NULL);
	ret = sprintf2("%s:%i", buf, (int)ntohs(ipv6->sin6_port));
	break;
#endif
    default:
	errno = EPFNOSUPPORT;
	break;
    }
    return(ret);
}

#if 0

/* 
 * It was very nice to use this, but it seems
 * to mess things up, so I guess it has to go... :-(
 */

static int formataddress(FILE *stream, const struct printf_info *info, const void *const *args)
{
    struct sockaddr *arg;
    socklen_t arglen;
    struct sockaddr_un *UNIX; /* Some wise guy has #defined unix with
			       * lowercase letters to 1, so I do this
			       * instead. */
    struct sockaddr_in *ipv4;
    int ret;
    
    arg = *(struct sockaddr **)(args[0]);
    arglen = *(socklen_t *)(args[1]);
    switch(arg->sa_family)
    {
    case AF_UNIX:
	UNIX = (struct sockaddr_un *)arg;
	ret = fprintf(stream, "%s", UNIX->sun_path);
	break;
    case AF_INET:
	ipv4 = (struct sockaddr_in *)arg;
	ret = fprintf(stream, "%s:%i", inet_ntoa(ipv4->sin_addr), (int)ntohs(ipv4->sin_port));
	break;
    default:
	ret = -1;
	errno = EPFNOSUPPORT;
	break;
    }
    return(ret);
}

static int formataddress_arginfo(const struct printf_info *info, size_t n, int *argtypes)
{
    if(n > 0)
	argtypes[0] = PA_POINTER;
    if(n > 1)
	argtypes[1] = PA_INT; /* Sources tell me that socklen_t _must_
			       * be an int, so I guess this should be
			       * safe. */
    return(2);
}
#endif

static int init(int hup)
{
    if(!hup)
    {
	/*
	if(register_printf_function('N', formataddress, formataddress_arginfo))
	{
	    flog(LOG_CRIT, "could not register printf handler %%N: %s", strerror(errno));
	    return(1);
	}
	*/
    }
    return(0);
}

static void terminate(void)
{
    while(sockets != NULL)
	unlinksock(sockets);
}

static struct module me =
{
    .name = "net",
    .conf =
    {
	.vars = myvars
    },
    .init = init,
    .terminate = terminate
};

MODULE(me)
