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
#ifndef _NET_H
#define _NET_H

#include <sys/socket.h>
#include <sys/un.h>

#define SOCK_SYN 0 /* Connecting */
#define SOCK_EST 1 /* Established */
#define SOCK_STL 2 /* Stale, dead */
#define SOCK_TOS_MINDELAY 4
#define SOCK_TOS_MAXTP 3
#define SOCK_TOS_MAXREL 2
#define SOCK_TOS_MINCOST 1

struct dgrambuf
{
    struct dgrambuf *next;
    struct sockaddr *addr;
    socklen_t addrlen;
    void *data;
    size_t size;
};

struct socket
{
    int refcount;
    int state;
    int dgram;
    int eos;
    size_t maxbuf;
    struct socket *back, *pnext;
    union
    {
	struct
	{
	    struct dgrambuf *f, *l;
	} d;
	struct
	{
	    void *buf;
	    size_t bufsize;
	    size_t datasize;
	} s;
    } buf;
    void (*conncb)(struct socket *sk, int err, void *data);
    void (*errcb)(struct socket *sk, int err, void *data);
    void (*readcb)(struct socket *sk, void *data);
    void (*writecb)(struct socket *sk, void *data);
    struct ufd *ufd;
    void *data;
};

struct lport {
    struct ufd *ufd;
    void (*acceptcb)(struct lport *lp, struct socket *newsk, void *data);
    void (*errcb)(struct lport *lp, int err, void *data);
    void *data;
};

void putsock(struct socket *sk);
void getsock(struct socket *sk);
struct socket *netsockpipe(void);
struct lport *netcslisten(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct lport *, struct socket *, void *), void *data);
struct lport *netcslistenlocal(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct lport *, struct socket *, void *), void *data);
struct lport *netcstcplisten(int port, int local, void (*func)(struct lport *, struct socket *, void *), void *data);
struct socket *netcsconn(struct sockaddr *addr, socklen_t addrlen, void (*func)(struct socket *, int, void *), void *data);
int pollsocks(int timeout);
void freedgbuf(struct dgrambuf *dg);
void sockqueue(struct socket *sk, void *data, size_t size);
void sockerror(struct socket *sk, int en);
/* size_t sockqueuesize(struct socket *sk); */
ssize_t sockqueueleft(struct socket *sk);
int netresolve(char *addr, void (*callback)(struct sockaddr *addr, int addrlen, void *data), void *data);
struct socket *netcsdgram(struct sockaddr *name, socklen_t namelen);
struct socket *netdgramconn(struct socket *sk, struct sockaddr *addr, socklen_t addrlen);
int sockgetremotename(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf);
int sockgetremotename2(struct socket *sk, struct socket *sk2, struct sockaddr **namebuf, socklen_t *lenbuf);
int lstgetremotename(struct lport *lp, struct sockaddr **namebuf, socklen_t *lenbuf);
int lstgetremotename2(struct lport *lp, struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf);
void closesock(struct socket *sk);
void closelport(struct lport *lp);
void *sockgetinbuf(struct socket *sk, size_t *size);
struct socket *wrapsock(int fd);
size_t sockgetdatalen(struct socket *sk);
int socksettos(struct socket *sk, int tos);
int addreq(struct sockaddr *x, struct sockaddr *y);
char *formataddress(struct sockaddr *arg, socklen_t arglen);
char *formatsockpeer(struct socket *sk);
void sockpushdata(struct socket *sk, void *buf, size_t size);
/* void sockblock(struct socket *sk, int block); */
int sockpeeraddr(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf);
int getucred(struct socket *sk, uid_t *uid, gid_t *gid);
int sockfamily(struct socket *sk);

#endif
