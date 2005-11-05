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
#ifndef _FILENET_H
#define _FILENET_H

#include <wchar.h>
#include "net.h"
#include "utils.h"
#include "search.h"

#define FNN_SYN 0
#define FNN_HS 1
#define FNN_EST 2
#define FNN_DEAD 3

#define FNPD_INT 0
#define FNPD_LL 1
#define FNPD_STR 2

struct fnetnode;
struct fnetpeer;

struct fnet
{
    struct fnet *next;
    wchar_t *name;
    void (*connect)(struct fnetnode *fn);
    void (*destroy)(struct fnetnode *fn);
    int (*setnick)(struct fnetnode *fn, wchar_t *newnick);
    int (*reqconn)(struct fnetpeer *peer);
    int (*sendchat)(struct fnetnode *fn, int public, wchar_t *to, wchar_t *string);
    int (*search)(struct fnetnode *fn, struct search *srch, struct srchfnnlist *ln);
    wchar_t *(*filebasename)(wchar_t *filename);
};

struct fnetpeerdatum
{
    struct fnetpeerdatum *next, *prev;
    int refcount;
    wchar_t *id;
    int datatype;
};

struct fnetpeerdi
{
    struct fnetpeerdatum *datum;
    union
    {
	int num;
	long long lnum;
	wchar_t *str;
    } data;
};

struct fnetpeer
{
    struct fnetpeer *next, *prev;
    struct fnetnode *fn;
    wchar_t *id;
    wchar_t *nick;
    union
    {
	struct
	{
	    int delete:1;
	    int op:1;
	} b;
	int w;
    } flags;
    int dinum;
    struct fnetpeerdi *peerdi;
};

struct fnetnode
{
    struct fnetnode *next, *prev;
    int refcount;
    int id;
    int state;
    int linked;
    time_t srchwait, lastsrch;
    wchar_t *name;
    wchar_t *mynick;
    struct fnet *fnet;
    struct socket *sk;
    struct fnetpeerdatum *peerdata;
    struct fnetpeer *peers;
    struct wcspair *args;
    int numpeers;
    void *data;
    CBCHAIN(fnetnode_ac, struct fnetnode *fn, wchar_t *attrib);
    CBCHAIN(fnetnode_chat, struct fnetnode *fn, int public, wchar_t *name, wchar_t *peer, wchar_t *string);
    CBCHAIN(fnetnode_unlink, struct fnetnode *fn);
    CBCHAIN(fnetnode_destroy, struct fnetnode *fn);
    CBCHAIN(fnetpeer_new, struct fnetnode *fn, struct fnetpeer *peer);
    CBCHAIN(fnetpeer_del, struct fnetnode *fn, struct fnetpeer *peer);
    CBCHAIN(fnetpeer_chdi, struct fnetnode *fn, struct fnetpeer *peer, struct fnetpeerdi *di);
};

void regfnet(struct fnet *fnet);
void fnetsetname(struct fnetnode *fn, wchar_t *newname);
void fnetsetstate(struct fnetnode *fn, int newstate);
int fnetsetnick(struct fnetnode *fn, wchar_t *newnick);
struct fnet *findfnet(wchar_t *name);
struct fnetnode *fnetinitconnect(wchar_t *name, char *addr);
void linkfnetnode(struct fnetnode *fn);
void unlinkfnetnode(struct fnetnode *fn);
void getfnetnode(struct fnetnode *fn);
void putfnetnode(struct fnetnode *fn);
void killfnetnode(struct fnetnode *fn);
struct fnetpeer *fnetaddpeer(struct fnetnode *fn, wchar_t *id, wchar_t *nick);
void fnetdelpeer(struct fnetpeer *peer);
struct fnetpeer *fnetfindpeer(struct fnetnode *fn, wchar_t *id);
void fnetpeersetstr(struct fnetpeer *peer, wchar_t *id, wchar_t *value);
void fnetpeersetnum(struct fnetpeer *peer, wchar_t *id, int value);
void fnetpeersetlnum(struct fnetpeer *peer, wchar_t *id, long long value);
void fnetpeerunset(struct fnetpeer *peer, wchar_t *id);
struct fnetnode *findfnetnode(int id);
void fnethandlechat(struct fnetnode *fn, int public, wchar_t *name, wchar_t *peer, wchar_t *chat);
int fnetsendchat(struct fnetnode *fn, int public, wchar_t *to, wchar_t *string);
int fnetsearch(struct fnetnode *fn, struct search *srch, struct srchfnnlist *ln);

extern struct fnetnode *fnetnodes;
EGCBCHAIN(newfncb, struct fnetnode *);

#endif
