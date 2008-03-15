/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2004 Fredrik Tolf <fredrik@dolda2000.com>
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
#include <sys/select.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/signal.h>
#include <sys/stat.h>       /* For rebindunix() */
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
    /** The network mode to use. Currently supported values are 0 for
     * active mode and 1 for passive mode. In the future, SOCKS5 proxy
     * support may be added. */
    {CONF_VAR_INT, "mode", {.num = 0}},
    /** Set the SO_REUSEADDR socket option on listening sockets, so
     * that dead TCP connections waiting for timeout are ignored. */
    {CONF_VAR_BOOL, "reuseaddr", {.num = 0}},
    /** Overrides the IPv4 address reported to other clients in active
     * mode. Useful for servers behind NAT routers. If both this and
     * net.publicif are unspecified the address of the hub connection
     * is used. */
    {CONF_VAR_IPV4, "visibleipv4", {.ipv4 = {0}}},
    /** Specifies an interface name from which to fetch the IPv4
     * address reported to other clients in active mode. If both this
     * and net.visibleipv4 are unspecified the address of the hub
     * connection is used. */
    {CONF_VAR_STRING, "publicif", {.str = L""}},
    /* Diffserv should be supported on IPv4, too, but I don't know the
     * API to do that. */
    /** The Diffserv value to use on IPv6 connections when the
     * minimize cost TOS value is used (see the TOS VALUES
     * section). */
    {CONF_VAR_INT, "diffserv-mincost", {.num = 0}},
    /** The Diffserv value to use on IPv6 connections when the
     * maximize reliability TOS value is used (see the TOS VALUES
     * section). */
    {CONF_VAR_INT, "diffserv-maxrel", {.num = 0}},
    /** The Diffserv value to use on IPv6 connections when the
     * maximize throughput TOS value is used (see the TOS VALUES
     * section). */
    {CONF_VAR_INT, "diffserv-maxtp", {.num = 0}},
    /** The Diffserv value to use on IPv6 connections when the
     * minimize delay TOS value is used (see the TOS VALUES
     * section). */
    {CONF_VAR_INT, "diffserv-mindelay", {.num = 0}},
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
	if((pif = icswcstombs(confgetstr("net", "publicif"), NULL, NULL)) == NULL)
	{
	    flog(LOG_ERR, "could not convert net.publicif into local charset: %s", strerror(errno));
	    return(-1);
	}
	if(!strcmp(pif, ""))
	    return(1);
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
	    if(strcmp(ifr->ifr_name, pif))
		continue;
	    memset(&req, 0, sizeof(req));
	    memcpy(req.ifr_name, ifr->ifr_name, sizeof(ifr->ifr_name));
	    if(ioctl(sock, SIOCGIFFLAGS, &req) < 0)
		break;
	    if(!(req.ifr_flags & IFF_UP))
	    {
		flog(LOG_WARNING, "public interface is down");
		break;
	    }
	    if(ifr->ifr_addr.sa_family != AF_INET)
	    {
		flog(LOG_WARNING, "address of the public interface is not AF_INET");
		break;
	    }
	    ipv4 = smalloc(sizeof(*ipv4));
	    memcpy(ipv4, &ifr->ifr_addr, sizeof(ifr->ifr_addr));
	    break;
	}
	free(conf.ifc_buf);
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
    return(1);
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
    new->ucred.uid = -1;
    new->ucred.gid = -1;
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
		free(buf->addr);
		free(buf);
	    }
	    while((buf = sk->inbuf.d.f) != NULL)
	    {
		sk->inbuf.d.f = buf->next;
		free(buf->data);
		free(buf->addr);
		free(buf);
	    }
	    break;
	}
	closesock(sk);
	if(sk->remote != NULL)
	    free(sk->remote);
	free(sk);
    }
}

void sockpushdata(struct socket *sk, void *buf, size_t size)
{
    switch(sk->type)
    {
    case SOCK_STREAM:
	sizebuf(&sk->inbuf.s.buf, &sk->inbuf.s.bufsize, sk->inbuf.s.datasize + size, 1, 1);
	memmove(sk->inbuf.s.buf + size, sk->inbuf.s.buf, sk->inbuf.s.datasize);
	memcpy(sk->inbuf.s.buf, buf, size);
	sk->inbuf.s.datasize += size;
	break;
    case SOCK_DGRAM:
	/* XXX */
	break;
    }
    return;
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

static void recvcmsg(struct socket *sk, struct msghdr *msg)
{
    struct cmsghdr *cmsg;
    
    for(cmsg = CMSG_FIRSTHDR(msg); cmsg != NULL; cmsg = CMSG_NXTHDR(msg, cmsg))
    {
#if UNIX_AUTH_STYLE == 1
	if((cmsg->cmsg_level == SOL_SOCKET) && (cmsg->cmsg_type == SCM_CREDENTIALS))
	{
	    struct ucred *cred;
	    if(sk->ucred.uid == -1)
	    {
		cred = (struct ucred *)CMSG_DATA(cmsg);
		sk->ucred.uid = cred->uid;
		sk->ucred.gid = cred->gid;
	    }
	}
#endif
    }
}

static void sockrecv(struct socket *sk)
{
    int ret, inq;
    struct dgrambuf *dbuf;
    struct msghdr msg;
    char cbuf[65536];
    struct iovec bufvec;
    
    memset(&msg, 0, sizeof(msg));
    msg.msg_iov = &bufvec;
    msg.msg_iovlen = 1;
    msg.msg_control = cbuf;
    msg.msg_controllen = sizeof(cbuf);
    switch(sk->type)
    {
    case SOCK_STREAM:
#if defined(HAVE_LINUX_SOCKIOS_H) && defined(SIOCINQ)
	/* SIOCINQ is Linux-specific AFAIK, but I really have no idea
	 * how to read the inqueue size on other OSs */
	if(sk->isrealsocket) {
	    if(ioctl(sk->fd, SIOCINQ, &inq))
	    {
		/* I don't really know what could go wrong here, so let's
		 * assume it's transient. */
		flog(LOG_WARNING, "SIOCINQ return %s on socket %i, falling back to 2048 bytes", strerror(errno), sk->fd);
		inq = 2048;
	    }
	} else {
	    /* There are perils when trying to use SIOCINQ on files >2GiB... */
	    inq = 65536;
	}
#else
	inq = 2048;
#endif
	if(inq > 65536)
	    inq = 65536;
	sizebuf(&sk->inbuf.s.buf, &sk->inbuf.s.bufsize, sk->inbuf.s.datasize + inq, 1, 1);
	if(sk->isrealsocket)
	{
	    bufvec.iov_base = sk->inbuf.s.buf + sk->inbuf.s.datasize;
	    bufvec.iov_len = inq;
	    ret = recvmsg(sk->fd, &msg, 0);
	} else {
	    ret = read(sk->fd, sk->inbuf.s.buf + sk->inbuf.s.datasize, inq);
	    msg.msg_controllen = 0;
	    msg.msg_flags = 0;
	}
	if(ret < 0)
	{
	    if((errno == EINTR) || (errno == EAGAIN))
		return;
	    if(sk->errcb != NULL)
		sk->errcb(sk, errno, sk->data);
	    closesock(sk);
	    return;
	}
	if(msg.msg_flags & MSG_CTRUNC)
	    flog(LOG_DEBUG, "ancillary data was truncated");
	else
	    recvcmsg(sk, &msg);
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
#if defined(HAVE_LINUX_SOCKIOS_H) && defined(SIOCINQ)
	if(ioctl(sk->fd, SIOCINQ, &inq))
	{
	    /* I don't really know what could go wrong here, so let's
	     * assume it's transient. */
	    flog(LOG_WARNING, "SIOCINQ return %s on socket %i", strerror(errno), sk->fd);
	    return;
	}
#else
	inq = 65536;
#endif
	dbuf = smalloc(sizeof(*dbuf));
	dbuf->data = smalloc(inq);
	dbuf->addr = smalloc(dbuf->addrlen = sizeof(struct sockaddr_storage));
	/*
	ret = recvfrom(sk->fd, dbuf->data, inq, 0, dbuf->addr, &dbuf->addrlen);
	*/
	msg.msg_name = dbuf->addr;
	msg.msg_namelen = dbuf->addrlen;
	bufvec.iov_base = dbuf->data;
	bufvec.iov_len = inq;
	ret = recvmsg(sk->fd, &msg, 0);
	dbuf->addrlen = msg.msg_namelen;
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
	if(msg.msg_flags & MSG_CTRUNC)
	    flog(LOG_DEBUG, "ancillary data was truncated");
	else
	    recvcmsg(sk, &msg);
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
    struct sockaddr_un *un;
    
    if((sk->family == AF_UNIX) && !sockgetlocalname(sk, (struct sockaddr **)(void *)&un, NULL) && (un->sun_family == PF_UNIX))
    {
	if((sk->state == SOCK_LST) && strchr(un->sun_path, '/'))
	{
	    if(unlink(un->sun_path))
		flog(LOG_WARNING, "could not unlink Unix socket %s: %s", un->sun_path, strerror(errno));
	}
    }
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

/*
 * Seriously, I don't know if it's naughty or not to remove
 * pre-existing Unix sockets.
 */
static int rebindunix(struct socket *sk, struct sockaddr *name, socklen_t namelen)
{
    struct sockaddr_un *un;
    struct stat sb;
    
    if((sk->family != AF_UNIX) || (name->sa_family != PF_UNIX))
	return(-1);
    un = (struct sockaddr_un *)name;
    if(stat(un->sun_path, &sb))
	return(-1);
    if(!S_ISSOCK(sb.st_mode))
	return(-1);
    if(unlink(un->sun_path))
	return(-1);
    if(bind(sk->fd, name, namelen) < 0)
	return(-1);
    return(0);
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
    if((bind(sk->fd, name, namelen) < 0) && ((errno != EADDRINUSE) || (rebindunix(sk, name, namelen) < 0)))
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

struct socket *netcslisten(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct socket *, struct socket *, void *), void *data)
{
    if(confgetint("net", "mode") == 1)
    {
	errno = EOPNOTSUPP;
	return(NULL);
    }
    if(confgetint("net", "mode") == 0)
	return(netcslistenlocal(type, name, namelen, func, data));
    errno = EOPNOTSUPP;
    return(NULL);
}

struct socket *netcstcplisten(int port, int local, void (*func)(struct socket *, struct socket *, void *), void *data)
{
    struct sockaddr_in addr;
#ifdef HAVE_IPV6
    struct sockaddr_in6 addr6;
#endif
    struct socket *(*csfunc)(int, struct sockaddr *, socklen_t, void (*)(struct socket *, struct socket *, void *), void *);
    struct socket *ret;
    
    if(local)
	csfunc = netcslistenlocal;
    else
	csfunc = netcslisten;
#ifdef HAVE_IPV6
    memset(&addr6, 0, sizeof(addr6));
    addr6.sin6_family = AF_INET6;
    addr6.sin6_port = htons(port);
    addr6.sin6_addr = in6addr_any;
    if((ret = csfunc(SOCK_STREAM, (struct sockaddr *)&addr6, sizeof(addr6), func, data)) != NULL)
	return(ret);
    if((ret == NULL) && (errno != EAFNOSUPPORT))
	return(NULL);
#endif
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    return(csfunc(SOCK_STREAM, (struct sockaddr *)&addr, sizeof(addr), func, data));
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

static void acceptunix(struct socket *sk)
{
    int buf;
    
    buf = 1;
#if UNIX_AUTH_STYLE == 1
    if(setsockopt(sk->fd, SOL_SOCKET, SO_PASSCRED, &buf, sizeof(buf)) < 0)
	flog(LOG_WARNING, "could not enable SO_PASSCRED on Unix socket %i: %s", sk->fd, strerror(errno));
#elif UNIX_AUTH_STYLE == 2
    if(getpeereid(sk->fd, &sk->ucred.uid, &sk->ucred.gid) < 0)
    {
	flog(LOG_WARNING, "could not get peer creds on Unix socket %i: %s", sk->fd, strerror(errno));
	sk->ucred.uid = -1;
	sk->ucred.gid = -1;
    }
#endif
}

int pollsocks(int timeout)
{
    int ret, fd;
    socklen_t retlen;
    int newfd, maxfd;
    fd_set rfds, wfds, efds;
    struct socket *sk, *next, *newsk;
    struct sockaddr_storage ss;
    socklen_t sslen;
    struct timeval tv;
    
    FD_ZERO(&rfds);
    FD_ZERO(&wfds);
    FD_ZERO(&efds);
    for(maxfd = 0, sk = sockets; sk != NULL; sk = sk->next)
    {
	if((sk->state == SOCK_STL) || (sk->fd < 0))
	    continue;
	if(!sk->ignread)
	    FD_SET(sk->fd, &rfds);
	if((sk->state == SOCK_SYN) || (sockqueuesize(sk) > 0))
	    FD_SET(sk->fd, &wfds);
	FD_SET(sk->fd, &efds);
	if(sk->fd > maxfd)
	    maxfd = sk->fd;
    }
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = (timeout % 1000) * 1000;
    ret = select(maxfd + 1, &rfds, &wfds, &efds, (timeout < 0)?NULL:&tv);
    if(ret < 0)
    {
	if(errno != EINTR)
	{
	    flog(LOG_CRIT, "pollsocks: select errored out: %s", strerror(errno));
	    /* To avoid CPU hogging in case it's bad, which it
	     * probably is. */
	    sleep(1);
	}
	return(1);
    }
    for(sk = sockets; sk != NULL; sk = next)
    {
	next = sk->next;
	fd = sk->fd;
	switch(sk->state)
	{
	case SOCK_LST:
	    if(FD_ISSET(fd, &rfds))
	    {
		sslen = sizeof(ss);
		if((newfd = accept(fd, (struct sockaddr *)&ss, &sslen)) < 0)
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
		if(ss.ss_family == PF_UNIX)
		    acceptunix(newsk);
		if(sk->acceptcb != NULL)
		    sk->acceptcb(sk, newsk, sk->data);
		putsock(newsk);
	    }
	    if(FD_ISSET(fd, &efds))
	    {
		retlen = sizeof(ret);
		getsockopt(fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->errcb != NULL)
		    sk->errcb(sk, ret, sk->data);
		continue;
	    }
	    break;
	case SOCK_SYN:
	    if(FD_ISSET(fd, &efds))
	    {
		retlen = sizeof(ret);
		getsockopt(fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->conncb != NULL)
		    sk->conncb(sk, ret, sk->data);
		closesock(sk);
		continue;
	    }
	    if(FD_ISSET(fd, &rfds) || FD_ISSET(fd, &wfds))
	    {
		sk->state = SOCK_EST;
		if(sk->conncb != NULL)
		    sk->conncb(sk, 0, sk->data);
	    }
	    break;
	case SOCK_EST:
	    if(FD_ISSET(fd, &efds))
	    {
		retlen = sizeof(ret);
		getsockopt(fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(sk->errcb != NULL)
		    sk->errcb(sk, ret, sk->data);
		closesock(sk);
		continue;
	    }
	    if(FD_ISSET(fd, &rfds))
		sockrecv(sk);
	    if(FD_ISSET(fd, &wfds))
	    {
		if(sockqueuesize(sk) > 0)
		    sockflush(sk);
	    }
	    break;
	}
    }
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
    int buf;
    
    if(sk->family == AF_UNIX)
	return(0); /* Unix sockets are always perfect. :) */
    if(sk->family == AF_INET)
    {
	switch(tos)
	{
	case 0:
	    buf = 0;
	    break;
	case SOCK_TOS_MINCOST:
	    buf = 0x02;
	    break;
	case SOCK_TOS_MAXREL:
	    buf = 0x04;
	    break;
	case SOCK_TOS_MAXTP:
	    buf = 0x08;
	    break;
	case SOCK_TOS_MINDELAY:
	    buf = 0x10;
	    break;
	default:
	    flog(LOG_WARNING, "attempted to set unknown TOS value %i to IPv4 sock", tos);
	    return(-1);
	}
	if(setsockopt(sk->fd, IPPROTO_IP, IP_TOS, &buf, sizeof(buf)) < 0)
	{
	    flog(LOG_WARNING, "could not set sock TOS to %i: %s", tos, strerror(errno));
	    return(-1);
	}
	return(0);
    }
    if(sk->family == AF_INET6)
    {
	switch(tos)
	{
	case 0:
	    buf = 0;
	case SOCK_TOS_MINCOST:
	    buf = confgetint("net", "diffserv-mincost");
	    break;
	case SOCK_TOS_MAXREL:
	    buf = confgetint("net", "diffserv-maxrel");
	    break;
	case SOCK_TOS_MAXTP:
	    buf = confgetint("net", "diffserv-maxtp");
	    break;
	case SOCK_TOS_MINDELAY:
	    buf = confgetint("net", "diffserv-mindelay");
	    break;
	default:
	    flog(LOG_WARNING, "attempted to set unknown TOS value %i to IPv4 sock", tos);
	    return(-1);
	}
	/*
	  On Linux, the API IPv6 flow label management doesn't seem to
	  be entirely complete, so I guess this will have to wait.
	  
	if(setsockopt(...) < 0)
	{
	    flog(LOG_WARNING, "could not set sock traffic class to %i: %s", tos, strerror(errno));
	    return(-1);
	}
	*/
	return(0);
    }
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
	    errno = ENOENT;
	    data->callback(NULL, 0, data->data);
	} else {
	    ipv4 = (struct sockaddr_in *)&data->addr;
	    memcpy(&ipv4->sin_addr, buf, 4);
	    data->callback((struct sockaddr *)ipv4, sizeof(*ipv4), data->data);
	}
    } else {
	errno = ENOENT;
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
	flog(LOG_ERR, "BUG: alive socket with dead fd in sockgetlocalname (%s)", strerror(errno));
	return(-1);
    }
    *namebuf = memcpy(smalloc(len), &name, len);
    if(lenbuf != NULL)
	*lenbuf = len;
    return(0);
}

static void sethostaddr(struct sockaddr *dst, struct sockaddr *src)
{
    if(dst->sa_family != src->sa_family)
    {
	flog(LOG_ERR, "BUG: non-matching socket families in sethostaddr (%i -> %i)", src->sa_family, dst->sa_family);
	return;
    }
    switch(src->sa_family)
    {
    case AF_INET:
	((struct sockaddr_in *)dst)->sin_addr = ((struct sockaddr_in *)src)->sin_addr;
	break;
    case AF_INET6:
	((struct sockaddr_in6 *)dst)->sin6_addr = ((struct sockaddr_in6 *)src)->sin6_addr;
	break;
    default:
	flog(LOG_WARNING, "sethostaddr unimplemented for family %i", src->sa_family);
	break;
    }
}

static int makepublic(struct sockaddr *addr)
{
    int ret;
    socklen_t plen;
    struct sockaddr *pname;
    
    if((ret = getpublicaddr(addr->sa_family, &pname, &plen)) < 0)
    {
	flog(LOG_ERR, "could not get public address: %s", strerror(errno));
	return(-1);
    }
    if(ret)
	return(0);
    sethostaddr(addr, pname);
    free(pname);
    return(0);
}

int sockgetremotename(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    socklen_t len;
    struct sockaddr *name;
    
    switch(confgetint("net", "mode"))
    {
    case 0:
	*namebuf = NULL;
	if((sk->state == SOCK_STL) || (sk->fd < 0))
	{
	    errno = EBADF;
	    return(-1);
	}
	if(!sockgetlocalname(sk, &name, &len))
	{
	    *namebuf = name;
	    *lenbuf = len;
	    makepublic(name);
	    return(0);
	}
	flog(LOG_ERR, "could not get remotely accessible name by any means");
	return(-1);
    case 1:
	errno = EOPNOTSUPP;
	return(-1);
    default:
	flog(LOG_CRIT, "unknown net mode %i active", confgetint("net", "mode"));
	errno = EOPNOTSUPP;
	return(-1);
    }
}

int sockgetremotename2(struct socket *sk, struct socket *sk2, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct sockaddr *name1, *name2;
    socklen_t len1, len2;
    
    if(sk->family != sk2->family)
    {
	flog(LOG_ERR, "using sockgetremotename2 with sockets of differing family: %i %i", sk->family, sk2->family);
	return(-1);
    }
    if(sockgetremotename(sk, &name1, &len1))
	return(-1);
    if(sockgetremotename(sk2, &name2, &len2)) {
	free(name1);
	return(-1);
    }
    sethostaddr(name1, name2);
    free(name2);
    *namebuf = name1;
    *lenbuf = len1;
    return(0);
}

int addreq(struct sockaddr *x, struct sockaddr *y)
{
    struct sockaddr_un *u1, *u2;
    struct sockaddr_in *n1, *n2;
#ifdef HAVE_IPV6
    struct sockaddr_in6 *s1, *s2;
#endif
    
    if(x->sa_family != y->sa_family)
	return(0);
    switch(x->sa_family) {
    case AF_UNIX:
	u1 = (struct sockaddr_un *)x; u2 = (struct sockaddr_un *)y;
	if(strncmp(u1->sun_path, u2->sun_path, sizeof(u1->sun_path)))
	    return(0);
	break;
    case AF_INET:
	n1 = (struct sockaddr_in *)x; n2 = (struct sockaddr_in *)y;
	if(n1->sin_port != n2->sin_port)
	    return(0);
	if(n1->sin_addr.s_addr != n2->sin_addr.s_addr)
	    return(0);
	break;
#ifdef HAVE_IPV6
    case AF_INET6:
	s1 = (struct sockaddr_in6 *)x; s2 = (struct sockaddr_in6 *)y;
	if(s1->sin6_port != s2->sin6_port)
	    return(0);
	if(memcmp(s1->sin6_addr.s6_addr, s2->sin6_addr.s6_addr, sizeof(s1->sin6_addr.s6_addr)))
	    return(0);
	break;
#endif
    }
    return(1);
}

char *formataddress(struct sockaddr *arg, socklen_t arglen)
{
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
	ret = sstrdup("Unix socket");
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
	ret = sprintf2("[%s]:%i", buf, (int)ntohs(ipv6->sin6_port));
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
