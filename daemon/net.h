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
#ifndef _NET_H
#define _NET_H

#include <sys/socket.h>
#include <sys/un.h>

#define SOCK_LST 0 /* Listening */
#define SOCK_SYN 1 /* Connecting */
#define SOCK_EST 2 /* Established */
#define SOCK_STL 3 /* Stale, dead */
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
    struct socket *next, *prev;
    int refcount;
    int fd;
    int isrealsocket; /* Bleh... */
    int family;
    int tos;
    int type;
    int state;
    int ignread;
    int events;
    int close;
    struct sockaddr *remote;
    socklen_t remotelen;
    struct ucred ucred;
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
    } outbuf;
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
    } inbuf;
    void (*conncb)(struct socket *sk, int err, void *data);
    void (*errcb)(struct socket *sk, int err, void *data);
    void (*readcb)(struct socket *sk, void *data);
    void (*writecb)(struct socket *sk, void *data);
    void (*acceptcb)(struct socket *sk, struct socket *newsk, void *data);
    void *data;
};

void putsock(struct socket *sk);
void getsock(struct socket *sk);
struct socket *netcslisten(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct socket *, struct socket *, void *), void *data);
struct socket *netcslistenlocal(int type, struct sockaddr *name, socklen_t namelen, void (*func)(struct socket *, struct socket *, void *), void *data);
struct socket *netcstcplisten(int port, int local, void (*func)(struct socket *, struct socket *, void *), void *data);
struct socket *netcsconn(struct sockaddr *addr, socklen_t addrlen, void (*func)(struct socket *, int, void *), void *data);
int pollsocks(int timeout);
void sockqueue(struct socket *sk, void *data, size_t size);
size_t sockqueuesize(struct socket *sk);
int netresolve(char *addr, void (*callback)(struct sockaddr *addr, int addrlen, void *data), void *data);
struct socket *netcsdgram(struct sockaddr *name, socklen_t namelen);
struct socket *netdupsock(struct socket *sk);
void netdgramconn(struct socket *sk, struct sockaddr *addr, socklen_t addrlen);
int sockgetlocalname(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf);
int sockgetremotename(struct socket *sk, struct sockaddr **namebuf, socklen_t *lenbuf);
int sockgetremotename2(struct socket *sk, struct socket *sk2, struct sockaddr **namebuf, socklen_t *lenbuf);
void closesock(struct socket *sk);
void *sockgetinbuf(struct socket *sk, size_t *size);
struct socket *wrapsock(int fd);
size_t sockgetdatalen(struct socket *sk);
int getpublicaddr(int af, struct sockaddr **addr, socklen_t *lenbuf);
int socksettos(struct socket *sk, int tos);
int addreq(struct sockaddr *x, struct sockaddr *y);
char *formataddress(struct sockaddr *arg, socklen_t arglen);
void sockpushdata(struct socket *sk, void *buf, size_t size);

#endif
