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

#define UFD_SOCK 0
#define UFD_PIPE 1
#define UFD_LISTEN 2

struct scons {
    struct scons *n, *p;
    struct socket *s;
};

struct ufd {
    struct ufd *next, *prev;
    int fd;
    int type;
    int ignread;
    struct socket *sk;
    union {
	struct {
	    int family;
	    int type;
	    struct sockaddr *remote;
	    socklen_t remotelen;
	    struct {
		uid_t uid;
		gid_t gid;
	    } ucred;
	} s;
	struct {
	    struct lport *lp;
	    int family;
	} l;
    } d;
};

static int getlocalname(int fd, struct sockaddr **namebuf, socklen_t *lenbuf);

static struct ufd *ufds = NULL;
static struct scons *rbatch, *wbatch, *cbatch;
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

static struct socket *newsock1(int dgram)
{
    struct socket *new;
    
    new = memset(smalloc(sizeof(*new)), 0, sizeof(*new));
    new->refcount = 1;
    new->state = -1;
    new->dgram = dgram;
    numsocks++;
    return(new);
}

static struct socket *sockpair(int dgram)
{
    struct socket *s1, *s2;
    
    s1 = newsock1(dgram);
    s2 = newsock1(dgram);
    s1->back = s2;
    s2->back = s1;
    putsock(s2);
    return(s1);
}

static void sksetstate(struct socket *sk, int state)
{
    sk->state = state;
    sk->back->state = state;
}

static void closeufd(struct ufd *ufd)
{
    if(ufd->fd != -1)
	close(ufd->fd);
    ufd->fd = -1;
}

static void freeufd(struct ufd *ufd)
{
    if(ufd->next != NULL)
	ufd->next->prev = ufd->prev;
    if(ufd->prev != NULL)
	ufd->prev->next = ufd->next;
    if(ufd == ufds)
	ufds = ufd->next;
    closeufd(ufd);
    if(ufd->sk != NULL)
	putsock(ufd->sk);
    if(ufd->type == UFD_SOCK) {
	if(ufd->d.s.remote != NULL)
	    free(ufd->d.s.remote);
    }
    free(ufd);
}

static struct ufd *mkufd(int fd, int type, struct socket *sk)
{
    struct ufd *ufd;
    
    ufd = memset(smalloc(sizeof(*ufd)), 0, sizeof(*ufd));
    ufd->fd = fd;
    ufd->type = type;
    if(sk != NULL) {
	getsock(ufd->sk = sk);
	sk->ufd = ufd;
    }
    if(type == UFD_SOCK) {
	ufd->d.s.ucred.uid = -1;
	ufd->d.s.ucred.gid = -1;
    }
    ufd->next = ufds;
    if(ufds)
	ufds->prev = ufd;
    ufds = ufd;
    return(ufd);
}

static struct ufd *dupufd(struct ufd *ufd)
{
    struct ufd *nufd;
    struct socket *nsk;
    
    if(ufd->sk != NULL)
	nsk = sockpair(ufd->sk->dgram);
    else
	nsk = NULL;
    nufd = mkufd(ufd->fd, ufd->type, nsk);
    if(nsk != NULL)
	putsock(nsk);
    if((nufd->fd = dup(ufd->fd)) < 0)
    {
	flog(LOG_WARNING, "could not dup() fd: %s", strerror(errno));
	freeufd(nufd);
	return(NULL);
    }
    sksetstate(nsk, SOCK_EST);
    if(ufd->type == UFD_SOCK) {
	nufd->d.s.family = ufd->d.s.family;
	nufd->d.s.type = ufd->d.s.type;
	nufd->d.s.ucred.uid = ufd->d.s.ucred.uid;
	nufd->d.s.ucred.gid = ufd->d.s.ucred.gid;
	if(ufd->d.s.remote != NULL)
	    nufd->d.s.remote = memcpy(smalloc(ufd->d.s.remotelen), ufd->d.s.remote, nufd->d.s.remotelen = ufd->d.s.remotelen);
    } else if(ufd->type == UFD_LISTEN) {
	nufd->d.l.family = ufd->d.l.family;
    }
    return(nufd);
}

static struct socket *mksock(int domain, int type)
{
    int fd;
    struct socket *sk;
    struct ufd *ufd;
    
    if((fd = socket(domain, type, 0)) < 0)
    {
	flog(LOG_CRIT, "could not create socket: %s", strerror(errno));
	return(NULL);
    }
    sk = sockpair(type == SOCK_DGRAM);
    ufd = mkufd(fd, UFD_SOCK, sk);
    ufd->d.s.family = domain;
    ufd->d.s.type = type;
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    return(sk);
}

struct socket *wrapsock(int fd)
{
    struct socket *sk;
    struct ufd *ufd;
    
    sk = sockpair(0);
    ufd = mkufd(fd, UFD_PIPE, sk->back);
    sksetstate(sk, SOCK_EST);
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
    return(sk);
}

void getsock(struct socket *sk)
{
    sk->refcount++;
}

static void freesock(struct socket *sk)
{
    struct dgrambuf *buf;
    
    if(sk->dgram) {
	while((buf = sk->buf.d.f) != NULL) {
	    sk->buf.d.f = buf->next;
	    freedgbuf(buf);
	}
    } else {
	if(sk->buf.s.buf != NULL)
	    free(sk->buf.s.buf);
    }
    free(sk);
    numsocks--;
}

void putsock(struct socket *sk)
{
    struct socket *back;
    
    if(--(sk->refcount) < 0) {
	flog(LOG_CRIT, "BUG: socket refcount < 0");
	abort();
    }
    if((sk->refcount == 0) && (sk->back->refcount == 0)) {
	back = sk->back;
	freesock(sk);
	freesock(back);
    }
}

static void linksock(struct scons **list, struct socket *sk)
{
    struct scons *sc;
    
    for(sc = *list; sc != NULL; sc = sc->n) {
	if(sc->s == sk)
	    return;
    }
    sc = smalloc(sizeof(*sc));
    getsock(sc->s = sk);
    sc->n = *list;
    sc->p = NULL;
    if(*list)
	(*list)->p = sc;
    *list = sc;
}

void sockpushdata(struct socket *sk, void *buf, size_t size)
{
    if(size == 0)
	return;
    if(sk->dgram) {
	/* XXX */
    } else {
	sizebuf(&sk->buf.s.buf, &sk->buf.s.bufsize, sk->buf.s.datasize + size, 1, 1);
	memmove(sk->buf.s.buf + size, sk->buf.s.buf, sk->buf.s.datasize);
	memcpy(sk->buf.s.buf, buf, size);
	sk->buf.s.datasize += size;
	linksock(&rbatch, sk);
    }
}

/* Read as the preterite of `read' */
void sockread(struct socket *sk)
{
    if((sockgetdatalen(sk) == 0) && (sk->eos == 1))
	linksock(&rbatch, sk);
    linksock(&wbatch, sk->back);
}

void freedgbuf(struct dgrambuf *dg)
{
    if(dg->data != NULL)
	free(dg->data);
    if(dg->addr != NULL)
	free(dg->addr);
    free(dg);
}

struct dgrambuf *sockgetdgbuf(struct socket *sk)
{
    struct dgrambuf *dbuf;
    
    if((dbuf = sk->buf.d.f) == NULL)
	return(NULL);
    sk->buf.d.f = dbuf->next;
    if(dbuf->next == NULL)
	sk->buf.d.l = NULL;
    dbuf->next = NULL;
    sockread(sk);
    return(dbuf);
}

void *sockgetinbuf(struct socket *sk, size_t *size)
{
    void *buf;
    struct dgrambuf *dbuf;
    
    if(sk->dgram) {
	dbuf = sockgetdgbuf(sk);
	buf = dbuf->data;
	*size = dbuf->size;
	free(dbuf->addr);
	free(dbuf);
    } else {
	if((sk->buf.s.buf == NULL) || (sk->buf.s.datasize == 0))
	{
	    *size = 0;
	    return(NULL);
	}
	buf = sk->buf.s.buf;
	*size = sk->buf.s.datasize;
	sk->buf.s.buf = NULL;
	sk->buf.s.bufsize = sk->buf.s.datasize = 0;
	sockread(sk);
    }
    return(buf);
}

void sockqueue(struct socket *sk, void *data, size_t size)
{
    struct dgrambuf *new;
    struct sockaddr *remote;
    socklen_t remotelen;
    
    if(size == 0)
	return;
    if(sk->state == SOCK_STL)
	return;
    if(sk->dgram) {
	if(sockpeeraddr(sk, &remote, &remotelen))
	    return;
	new = smalloc(sizeof(*new));
	new->next = NULL;
	memcpy(new->data = smalloc(size), data, new->size = size);
	new->addr = remote;
	new->addrlen = remotelen;
	if(sk->back->buf.d.l == NULL)
	{
	    sk->back->buf.d.l = sk->back->buf.d.f = new;
	} else {
	    sk->back->buf.d.l->next = new;
	    sk->back->buf.d.l = new;
	}
    } else {
	sizebuf(&(sk->back->buf.s.buf), &(sk->back->buf.s.bufsize), sk->back->buf.s.datasize + size, 1, 1);
	memcpy(sk->back->buf.s.buf + sk->back->buf.s.datasize, data, size);
	sk->back->buf.s.datasize += size;
    }
    linksock(&rbatch, sk->back);
}

void sockqueuedg(struct socket *sk, struct dgrambuf *dg)
{
    if(sk->state == SOCK_STL) {
	freedgbuf(dg);
	return;
    }
    if(!sk->dgram) {
	flog(LOG_ERR, "BUG: sockqueuedg called on non-dgram socket");
	freedgbuf(dg);
	return;
    }
    dg->next = NULL;
    if(sk->back->buf.d.l == NULL)
    {
	sk->back->buf.d.l = sk->back->buf.d.f = dg;
    } else {
	sk->back->buf.d.l->next = dg;
	sk->back->buf.d.l = dg;
    }
    linksock(&rbatch, sk->back);
}

void sockerror(struct socket *sk, int en)
{
    sksetstate(sk, SOCK_STL);
    if(sk->back->errcb != NULL)
	sk->back->errcb(sk->back, en, sk->back->data);
}

static void recvcmsg(struct ufd *ufd, struct msghdr *msg)
{
    struct cmsghdr *cmsg;
    
    for(cmsg = CMSG_FIRSTHDR(msg); cmsg != NULL; cmsg = CMSG_NXTHDR(msg, cmsg))
    {
#if UNIX_AUTH_STYLE == 1
	if((cmsg->cmsg_level == SOL_SOCKET) && (cmsg->cmsg_type == SCM_CREDENTIALS))
	{
	    struct ucred *cred;
	    if(ufd->d.s.ucred.uid == -1)
	    {
		cred = (struct ucred *)CMSG_DATA(cmsg);
		ufd->d.s.ucred.uid = cred->uid;
		ufd->d.s.ucred.gid = cred->gid;
	    }
	}
#endif
    }
}

static int ufddgram(struct ufd *ufd)
{
    int dgram;

    if(ufd->type == UFD_SOCK) {
	dgram = ufd->d.s.type == SOCK_DGRAM;
    } else if(ufd->type == UFD_PIPE) {
	dgram = 0;
    } else {
	flog(LOG_ERR, "BUG: calling ufddgram on ufd of bad type %i", ufd->type);
	return(-1);
    }
    if(ufd->sk == NULL) {
	flog(LOG_ERR, "BUG: calling ufddgram on socketless ufd (type %i)", ufd->type);
	return(-1);
    }
    if(dgram != ufd->sk->dgram) {
	flog(LOG_ERR, "BUG: ufd/socket dgram value mismatch");
	return(-1);
    }
    return(dgram);
}

static void sockrecv(struct ufd *ufd)
{
    int ret, inq;
    int dgram;
    struct dgrambuf *dbuf;
    struct msghdr msg;
    char cbuf[65536];
    struct iovec bufvec;
    void *buf;
    
    memset(&msg, 0, sizeof(msg));
    msg.msg_iov = &bufvec;
    msg.msg_iovlen = 1;
    msg.msg_control = cbuf;
    msg.msg_controllen = sizeof(cbuf);
    if((dgram = ufddgram(ufd)) < 0)
	return;
    if(dgram) {
#if defined(HAVE_LINUX_SOCKIOS_H) && defined(SIOCINQ)
	if(ioctl(ufd->fd, SIOCINQ, &inq))
	{
	    /* I don't really know what could go wrong here, so let's
	     * assume it's transient. */
	    flog(LOG_WARNING, "SIOCINQ return %s on socket %i", strerror(errno), ufd->fd);
	    return;
	}
#else
	inq = 65536;
#endif
	dbuf = smalloc(sizeof(*dbuf));
	dbuf->data = smalloc(inq);
	dbuf->addr = smalloc(dbuf->addrlen = sizeof(struct sockaddr_storage));
	msg.msg_name = dbuf->addr;
	msg.msg_namelen = dbuf->addrlen;
	bufvec.iov_base = dbuf->data;
	bufvec.iov_len = inq;
	ret = recvmsg(ufd->fd, &msg, 0);
	dbuf->addrlen = msg.msg_namelen;
	if(ret < 0)
	{
	    freedgbuf(dbuf);
	    if((errno == EINTR) || (errno == EAGAIN))
		return;
	    closeufd(ufd);
	    sockerror(ufd->sk, errno);
	    return;
	}
	if(msg.msg_flags & MSG_CTRUNC)
	    flog(LOG_DEBUG, "ancillary data was truncated");
	else
	    recvcmsg(ufd, &msg);
	/* On UDP/IPv[46], ret == 0 doesn't mean EOF (since UDP can't
	 * have EOF), but rather an empty packet. I don't know if any
	 * other potential DGRAM protocols might have an EOF
	 * condition, so let's play safe. */
	if(ret == 0)
	{
	    freedgbuf(dbuf);
	    if((ufd->type != UFD_SOCK) || !((ufd->d.s.family == AF_INET) || (ufd->d.s.family == AF_INET6)))
	    {
		closesock(ufd->sk);
		closeufd(ufd);
	    }
	    return;
	}
	dbuf->addr = srealloc(dbuf->addr, dbuf->addrlen);
	dbuf->data = srealloc(dbuf->data, dbuf->size = ret);
	dbuf->next = NULL;
	sockqueuedg(ufd->sk, dbuf);
    } else {
#if defined(HAVE_LINUX_SOCKIOS_H) && defined(SIOCINQ)
	/* SIOCINQ is Linux-specific AFAIK, but I really have no idea
	 * how to read the inqueue size on other OSs */
	if(ufd->type == UFD_SOCK) {
	    if(ioctl(ufd->fd, SIOCINQ, &inq))
	    {
		/* I don't really know what could go wrong here, so let's
		 * assume it's transient. */
		flog(LOG_WARNING, "SIOCINQ return %s on socket %i, falling back to 2048 bytes", strerror(errno), ufd->fd);
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
	/* This part could be optimized by telling the kernel to read
	 * directly into ufd->sk->back->buf, but that would be uglier
	 * by not using the socket function interface. */
	buf = smalloc(inq);
	if(ufd->type == UFD_SOCK)
	{
	    bufvec.iov_base = buf;
	    bufvec.iov_len = inq;
	    ret = recvmsg(ufd->fd, &msg, 0);
	} else {
	    ret = read(ufd->fd, buf, inq);
	    msg.msg_controllen = 0;
	    msg.msg_flags = 0;
	}
	if(ret < 0)
	{
	    free(buf);
	    if((errno == EINTR) || (errno == EAGAIN))
		return;
	    closeufd(ufd);
	    sockerror(ufd->sk, errno);
	    return;
	}
	if(msg.msg_flags & MSG_CTRUNC)
	    flog(LOG_DEBUG, "ancillary data was truncated");
	else
	    recvcmsg(ufd, &msg);
	if(ret == 0)
	{
	    free(buf);
	    closeufd(ufd);
	    closesock(ufd->sk);
	    return;
	}
	sockqueue(ufd->sk, buf, ret);
	free(buf);
    }
}

static void sockflush(struct ufd *ufd)
{
    int ret;
    struct dgrambuf *dbuf;
    int dgram;
    
    if((dgram = ufddgram(ufd)) < 0)
	return;
    if(dgram) {
	dbuf = sockgetdgbuf(ufd->sk);
	sendto(ufd->fd, dbuf->data, dbuf->size, MSG_DONTWAIT | MSG_NOSIGNAL, dbuf->addr, dbuf->addrlen);
	freedgbuf(dbuf);
    } else {
	if(ufd->type == UFD_SOCK)
	    ret = send(ufd->fd, ufd->sk->buf.s.buf, ufd->sk->buf.s.datasize, MSG_DONTWAIT | MSG_NOSIGNAL);
	else
	    ret = write(ufd->fd, ufd->sk->buf.s.buf, ufd->sk->buf.s.datasize);
	if(ret < 0) {
	    /* For now, assume transient error, since
	     * the socket is polled for errors */
	    return;
	}
	if(ret > 0) {
	    memmove(ufd->sk->buf.s.buf, ((char *)ufd->sk->buf.s.buf) + ret, ufd->sk->buf.s.datasize -= ret);
	    sockread(ufd->sk);
	}
    }
}

void closesock(struct socket *sk)
{
    sksetstate(sk, SOCK_STL);
    if(sk->back->eos == 0)
	sk->back->eos = 1;
    linksock(&rbatch, sk->back);
}

size_t sockgetdatalen(struct socket *sk)
{
    struct dgrambuf *b;
    size_t ret;
    
    if(sk->dgram) {
	ret = 0;
	for(b = sk->buf.d.f; b != NULL; b = b->next)
	    ret += b->size;
    } else {
	ret = sk->buf.s.datasize;
    }
    return(ret);
}

size_t sockqueuesize(struct socket *sk)
{
    return(sockgetdatalen(sk->back));
}

/*
 * Seriously, I don't know if it's naughty or not to remove
 * pre-existing Unix sockets.
 */
static int rebindunix(struct ufd *ufd, struct sockaddr *name, socklen_t namelen)
{
    struct sockaddr_un *un;
    struct stat sb;
    
    if((ufd->d.l.family != AF_UNIX) || (name->sa_family != PF_UNIX))
	return(-1);
    un = (struct sockaddr_un *)name;
    if(stat(un->sun_path, &sb))
	return(-1);
    if(!S_ISSOCK(sb.st_mode))
	return(-1);
    if(unlink(un->sun_path))
	return(-1);
    if(bind(ufd->fd, name, namelen) < 0)
	return(-1);
    return(0);
}

void closelport(struct lport *lp)
{
    struct ufd *ufd;
    struct sockaddr_un *un;
    
    ufd = lp->ufd;
    if((ufd->d.l.family == AF_UNIX) && !getlocalname(ufd->fd, (struct sockaddr **)(void *)&un, NULL) && (un->sun_family == PF_UNIX) && strchr(un->sun_path, '/')) {
	if(unlink(un->sun_path))
	    flog(LOG_WARNING, "could not unlink Unix socket %s: %s", un->sun_path, strerror(errno));
    }
    freeufd(lp->ufd);
}

/*
 * The difference between netcslisten() and netcslistenlocal() is that
 * netcslistenlocal() always listens on the local host, instead of
 * following proxy/passive mode directions. It is suitable for eg. the
 * UI channel, while the file sharing networks should, naturally, use
 * netcslisten() instead.
*/

struct lport *netcslistenlocal(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct lport *, struct socket *, void *), void *data)
{
    struct lport *lp;
    struct ufd *ufd;
    int fd;
    int intbuf;
    
    /* I don't know if this is actually correct (it probably isn't),
     * but since, at on least Linux systems, PF_* are specifically
     * #define'd to their AF_* counterparts, it allows for a severely
     * smoother implementation. If it breaks something on your
     * platform, please tell me so.
     */
    if((fd = socket(name->sa_family, type, 0)) < 0)
	return(NULL);
    if(confgetint("net", "reuseaddr")) {
	intbuf = 1;
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &intbuf, sizeof(intbuf));
    }
    ufd = mkufd(fd, UFD_LISTEN, NULL);
    ufd->d.l.family = name->sa_family;
    lp = memset(smalloc(sizeof(*lp)), 0, sizeof(*lp));
    lp->ufd = ufd;
    ufd->d.l.lp = lp;
    if((bind(fd, name, namelen) < 0) && ((errno != EADDRINUSE) || (rebindunix(ufd, name, namelen) < 0))) {
	freeufd(ufd);
	return(NULL);
    }
    if(listen(fd, 16) < 0)
    {
	freeufd(ufd);
	return(NULL);
    }
    lp->acceptcb = func;
    lp->data = data;
    return(lp);
}

struct lport *netcslisten(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct lport *, struct socket *, void *), void *data)
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

struct lport *netcstcplisten(int port, int local, void (*func)(struct lport *, struct socket *, void *), void *data)
{
    struct sockaddr_in addr;
#ifdef HAVE_IPV6
    struct sockaddr_in6 addr6;
#endif
    struct lport *(*csfunc)(int, struct sockaddr *, socklen_t, void (*)(struct lport *, struct socket *, void *), void *);
    struct lport *ret;
    
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
	if(bind(sk->ufd->fd, name, namelen) < 0)
	{
	    putsock(sk);
	    return(NULL);
	}
	sksetstate(sk, SOCK_EST);
	return(sk->back);
    }
    errno = EOPNOTSUPP;
    return(NULL);
}

struct socket *netdgramconn(struct socket *sk, struct sockaddr *addr, socklen_t addrlen)
{
    struct ufd *nufd;
    
    nufd = dupufd(sk->back->ufd);
    getsock(sk = nufd->sk->back);
    memcpy(nufd->d.s.remote = smalloc(addrlen), addr, nufd->d.s.remotelen = addrlen);
    nufd->ignread = 1;
    return(sk);
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
	memcpy(sk->ufd->d.s.remote = smalloc(addrlen), addr, sk->ufd->d.s.remotelen = addrlen);
	sk->back->conncb = func;
	sk->back->data = data;
	if(!connect(sk->ufd->fd, addr, addrlen))
	{
	    sksetstate(sk, SOCK_EST);
	    linksock(&cbatch, sk->back);
	    return(sk->back);
	}
	if(errno == EINPROGRESS)
	{
	    sksetstate(sk, SOCK_SYN);
	    return(sk->back);
	}
	putsock(sk);
	return(NULL);
    }
    errno = EOPNOTSUPP;
    return(NULL);
}

static void acceptunix(struct ufd *ufd)
{
    int buf;
    
    buf = 1;
#if UNIX_AUTH_STYLE == 1
    if(setsockopt(ufd->fd, SOL_SOCKET, SO_PASSCRED, &buf, sizeof(buf)) < 0)
	flog(LOG_WARNING, "could not enable SO_PASSCRED on Unix socket %i: %s", ufd->fd, strerror(errno));
#elif UNIX_AUTH_STYLE == 2
    if(getpeereid(ufd->fd, &ufd->d.s.ucred.uid, &ufd->d.s.ucred.gid) < 0)
    {
	flog(LOG_WARNING, "could not get peer creds on Unix socket %i: %s", ufd->fd, strerror(errno));
	ufd->d.s.ucred.uid = -1;
	ufd->d.s.ucred.gid = -1;
    }
#endif
}

static void runbatches(void)
{
    struct scons *sc, *nsc;

    for(sc = cbatch, cbatch = NULL; sc; sc = nsc) {
	nsc = sc->n;
	if(sc->s->conncb != NULL)
	    sc->s->conncb(sc->s, 0, sc->s->data);
	free(sc);
    }
    for(sc = rbatch, rbatch = NULL; sc; sc = nsc) {
	nsc = sc->n;
	if(sc->s->readcb != NULL)
	    sc->s->readcb(sc->s, sc->s->data);
	if((sockgetdatalen(sc->s) == 0) && (sc->s->eos == 1)) {
	    if(sc->s->errcb != NULL)
		sc->s->errcb(sc->s, 0, sc->s->data);
	    sc->s->eos = 2;
	}
	free(sc);
    }
    for(sc = wbatch, wbatch = NULL; sc; sc = nsc) {
	nsc = sc->n;
	if(sc->s->writecb != NULL)
	    sc->s->writecb(sc->s, sc->s->data);
	free(sc);
    }
}

static void cleansocks(void)
{
    struct ufd *ufd, *next;
    
    for(ufd = ufds; ufd != NULL; ufd = next) {
	next = ufd->next;
	if(ufd->sk && (sockgetdatalen(ufd->sk) == 0)) {
	    if(ufd->sk->eos == 1) {
		ufd->sk->eos = 2;
		closeufd(ufd);
		closesock(ufd->sk);
	    }
	    if((ufd->sk->refcount == 1) && (ufd->sk->back->refcount == 0)) {
		freeufd(ufd);
		continue;
	    }
	}
    }
}

int pollsocks(int timeout)
{
    int ret;
    socklen_t retlen;
    int newfd, maxfd;
    fd_set rfds, wfds, efds;
    struct ufd *ufd, *nufd;
    struct socket *nsk;
    struct sockaddr_storage ss;
    socklen_t sslen;
    struct timeval tv;
    
    cleansocks();
    FD_ZERO(&rfds);
    FD_ZERO(&wfds);
    FD_ZERO(&efds);
    for(maxfd = 0, ufd = ufds; ufd != NULL; ufd = ufd->next) {
	if(ufd->fd < 0)
	    continue;
	if(!ufd->ignread)
	    FD_SET(ufd->fd, &rfds);
	if(ufd->sk != NULL) {
	    if(sockgetdatalen(ufd->sk) > 0)
		FD_SET(ufd->fd, &wfds);
	    else if(ufd->sk->state == SOCK_SYN)
		FD_SET(ufd->fd, &wfds);
	}
	FD_SET(ufd->fd, &efds);
	if(ufd->fd > maxfd)
	    maxfd = ufd->fd;
    }
    if(rbatch || wbatch || cbatch)
	timeout = 0;
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = (timeout % 1000) * 1000;
    ret = select(maxfd + 1, &rfds, &wfds, &efds, (timeout < 0)?NULL:&tv);
    if(ret < 0) {
	if(errno != EINTR) {
	    flog(LOG_CRIT, "pollsocks: select errored out: %s", strerror(errno));
	    /* To avoid CPU hogging in case it's bad, which it
	     * probably is. */
	    sleep(1);
	}
	return(1);
    }
    for(ufd = ufds; ufd != NULL; ufd = ufd->next) {
	if(ufd->sk < 0)
	    continue;
	if(ufd->type == UFD_LISTEN) {
	    if(FD_ISSET(ufd->fd, &rfds)) {
		sslen = sizeof(ss);
		if((newfd = accept(ufd->fd, (struct sockaddr *)&ss, &sslen)) < 0) {
		    if(ufd->d.l.lp->errcb != NULL)
			ufd->d.l.lp->errcb(ufd->d.l.lp, errno, ufd->d.l.lp->data);
		}
		nsk = sockpair(0);
		nufd = mkufd(newfd, UFD_SOCK, nsk);
		nufd->d.s.family = ufd->d.l.family;
		sksetstate(nsk, SOCK_EST);
		memcpy(nufd->d.s.remote = smalloc(sslen), &ss, sslen);
		nufd->d.s.remotelen = sslen;
		if(ss.ss_family == PF_UNIX)
		    acceptunix(nufd);
		if(ufd->d.l.lp->acceptcb != NULL)
		    ufd->d.l.lp->acceptcb(ufd->d.l.lp, nsk->back, ufd->d.l.lp->data);
		putsock(nsk);
	    }
	    if(FD_ISSET(ufd->fd, &efds)) {
		retlen = sizeof(ret);
		getsockopt(ufd->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		if(ufd->d.l.lp->errcb != NULL)
		    ufd->d.l.lp->errcb(ufd->d.l.lp, ret, ufd->d.l.lp->data);
		continue;
	    }
	} else {
	    if(ufd->sk->state == SOCK_SYN) {
		if(FD_ISSET(ufd->fd, &efds)) {
		    retlen = sizeof(ret);
		    getsockopt(ufd->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		    if(ufd->sk->back->conncb != NULL)
			ufd->sk->back->conncb(ufd->sk->back, ret, ufd->sk->back->data);
		    closeufd(ufd);
		    continue;
		}
		if(FD_ISSET(ufd->fd, &rfds) || FD_ISSET(ufd->fd, &wfds)) {
		    sksetstate(ufd->sk, SOCK_EST);
		    linksock(&cbatch, ufd->sk->back);
		}
	    } else if(ufd->sk->state == SOCK_EST) {
		if(FD_ISSET(ufd->fd, &efds)) {
		    retlen = sizeof(ret);
		    getsockopt(ufd->fd, SOL_SOCKET, SO_ERROR, &ret, &retlen);
		    sockerror(ufd->sk, ret);
		    closeufd(ufd);
		    continue;
		}
		if(FD_ISSET(ufd->fd, &rfds))
		    sockrecv(ufd);
		if(ufd->fd == -1)
		    continue;
		if(FD_ISSET(ufd->fd, &wfds))
		    sockflush(ufd);
	    }
	}
    }
    runbatches();
    cleansocks();
    return(1);
}

static struct ufd *getskufd(struct socket *sk)
{
    while(1) {
	if(sk->back->ufd != NULL)
	    return(sk->back->ufd);
	if((sk = sk->back->pnext) == NULL)
	    break;
    }
    return(NULL);
}

int socksettos(struct socket *sk, int tos)
{
    int buf;
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd->d.s.family == AF_UNIX)
	return(0); /* Unix sockets are always perfect. :) */
    if(ufd->d.s.family == AF_INET)
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
	if(setsockopt(ufd->fd, IPPROTO_IP, IP_TOS, &buf, sizeof(buf)) < 0)
	{
	    flog(LOG_WARNING, "could not set sock TOS to %i: %s", tos, strerror(errno));
	    return(-1);
	}
	return(0);
    }
    if(ufd->d.s.family == AF_INET6)
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
    flog(LOG_WARNING, "could not set TOS on sock of family %i", ufd->d.s.family);
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

static int getlocalname(int fd, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    socklen_t len;
    struct sockaddr_storage name;
    
    *namebuf = NULL;
    if(fd < 0)
	return(-1);
    len = sizeof(name);
    if(getsockname(fd, (struct sockaddr *)&name, &len) < 0)
    {
	flog(LOG_ERR, "BUG: alive socket with dead fd in sockgetlocalname (%s)", strerror(errno));
	return(-1);
    }
    *namebuf = memcpy(smalloc(len), &name, len);
    if(lenbuf != NULL)
	*lenbuf = len;
    return(0);
}

int lstgetlocalname(struct lport *lp, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct ufd *ufd;

    ufd = lp->ufd;
    return(getlocalname(ufd->fd, namebuf, lenbuf));
}

int sockgetlocalname(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct ufd *ufd;

    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    return(getlocalname(ufd->fd, namebuf, lenbuf));
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

static int getremotename(int fd, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    socklen_t len;
    struct sockaddr *name;

    switch(confgetint("net", "mode")) {
    case 0:
	*namebuf = NULL;
	if(!getlocalname(fd, &name, &len)) {
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

int sockgetremotename(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd->fd < 0) {
	errno = EBADF;
	return(-1);
    }
    return(getremotename(ufd->fd, namebuf, lenbuf));
}

int lstgetremotename(struct lport *lp, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct ufd *ufd;
    
    ufd = lp->ufd;
    return(getremotename(ufd->fd, namebuf, lenbuf));
}

int sockgetremotename2(struct socket *sk, struct socket *sk2, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct sockaddr *name1, *name2;
    socklen_t len1, len2;
    struct ufd *ufd1, *ufd2;
    
    ufd1 = getskufd(sk);
    ufd2 = getskufd(sk2);
    if((ufd1->type != UFD_SOCK) || (ufd2->type != UFD_SOCK)) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd1->d.s.family != ufd2->d.s.family)
    {
	flog(LOG_ERR, "using sockgetremotename2 with sockets of differing family: %i %i", ufd1->d.s.family, ufd2->d.s.family);
	return(-1);
    }
    if(getremotename(ufd1->fd, &name1, &len1))
	return(-1);
    if(getremotename(ufd2->fd, &name2, &len2)) {
	free(name1);
	return(-1);
    }
    sethostaddr(name1, name2);
    free(name2);
    *namebuf = name1;
    *lenbuf = len1;
    return(0);
}

int lstgetremotename2(struct lport *lp, struct socket *sk2, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct sockaddr *name1, *name2;
    socklen_t len1, len2;
    struct ufd *ufd1, *ufd2;
    
    ufd1 = lp->ufd;
    ufd2 = getskufd(sk2);
    if(ufd2->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd1->d.l.family != ufd2->d.s.family)
    {
	flog(LOG_ERR, "using lstgetremotename2 with sockets of differing family: %i %i", ufd1->d.l.family, ufd2->d.s.family);
	return(-1);
    }
    if(getremotename(ufd1->fd, &name1, &len1))
	return(-1);
    if(getremotename(ufd2->fd, &name2, &len2)) {
	free(name1);
	return(-1);
    }
    sethostaddr(name1, name2);
    free(name2);
    *namebuf = name1;
    *lenbuf = len1;
    return(0);
}

int getucred(struct socket *sk, uid_t *uid, gid_t *gid)
{
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd->d.s.family != AF_UNIX) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    *uid = ufd->d.s.ucred.uid;
    *gid = ufd->d.s.ucred.gid;
    return(0);
}

void sockblock(struct socket *sk, int block)
{
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    ufd->ignread = block;
}

int sockfamily(struct socket *sk)
{
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    return(ufd->d.s.family);
}

int sockpeeraddr(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf)
{
    struct ufd *ufd;
    
    ufd = getskufd(sk);
    if(ufd->type != UFD_SOCK) {
	errno = EOPNOTSUPP;
	return(-1);
    }
    if(ufd->d.s.remote == NULL)
	return(-1);
    *namebuf = memcpy(smalloc(ufd->d.s.remotelen), ufd->d.s.remote, ufd->d.s.remotelen);
    if(lenbuf != NULL)
	*lenbuf = ufd->d.s.remotelen;
    return(0);
}

char *formatsockpeer(struct socket *sk)
{
    struct sockaddr *name;
    socklen_t nlen;
    char *ret;
    
    if(sockpeeraddr(sk, &name, &nlen))
	return(NULL);
    ret = formataddress(name, nlen);
    free(name);
    return(ret);
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
    /*
    while(ufds != NULL)
	freeufd(ufds);
    */
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
