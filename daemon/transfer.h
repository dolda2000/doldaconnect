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
#ifndef _TRANSFER_H
#define _TRANSFER_H

#include <wchar.h>
#include <sys/types.h>

#include "net.h"
#include "filenet.h"
#include "utils.h"
#include "auth.h"

#define TRNS_WAITING 0
#define TRNS_HS 1
#define TRNS_MAIN 2
#define TRNS_DONE 3

#define TRNSD_UNKNOWN 0
#define TRNSD_UP 1
#define TRNSD_DOWN 2

#define TRNSE_NOERROR 0
#define TRNSE_NOTFOUND 1
#define TRNSE_NOSLOTS 2

struct transfer;

struct transferiface
{
    void (*detach)(struct transfer *transfer, void *data);
    void (*gotdata)(struct transfer *transfer, void *data);
    void (*endofdata)(struct transfer *transfer, void *data);
    void (*wantdata)(struct transfer *transfer, void *data);
};

struct transfer
{
    struct transfer *next, *prev;
    int id, close;
    union
    {
	int w;
	struct
	{
	    int byop:1;
	    int sgranted:1;
	    int minislot:1;
	} b;
    } flags;
    struct timer *etimer;
    time_t timeout, activity, lastreq;
    wchar_t *actdesc;
    struct fnet *fnet;
    struct transferiface *iface;
    wchar_t *peerid, *peernick;
    wchar_t *path;
    uid_t owner;
    int state, dir, error;
    off_t size, curpos, endpos;
    struct fnetnode *fn;
    void *ifacedata;
    struct socket *localend;
    struct wcspair *args;
    pid_t filter;
    struct authhandle *auth;
    struct socket *filterout;
    char *filterbuf;
    struct hash *hash;
    size_t filterbufsize, filterbufdata;
    wchar_t *exitstatus;
    CBCHAIN(trans_ac, struct transfer *transfer, wchar_t *attrib);
    CBCHAIN(trans_p, struct transfer *transfer);
    CBCHAIN(trans_act, struct transfer *transfer);
    CBCHAIN(trans_destroy, struct transfer *transfer);
    CBCHAIN(trans_filterout, struct transfer *transfer, wchar_t *cmd, wchar_t *arg);
};

void freetransfer(struct transfer *transfer);
struct transfer *newtransfer(void);
void linktransfer(struct transfer *transfer);
int slotsleft(void);
void bumptransfer(struct transfer *transfer);
struct transfer *findtransfer(int id);
struct transfer *hasupload(struct fnet *fnet, wchar_t *peerid);
struct transfer *newupload(struct fnetnode *fn, struct fnet *fnet, wchar_t *nickid, struct transferiface *iface, void *data);
void transfersetnick(struct transfer *transfer, wchar_t *newnick);
void transfersetpath(struct transfer *transfer, wchar_t *newpath);
void transfersetstate(struct transfer *transfer, int newstate);
void transfersetsize(struct transfer *transfer, off_t newsize);
void transferseterror(struct transfer *transfer, int error);
void transfersetactivity(struct transfer *transfer, wchar_t *desc);
void transferattach(struct transfer *transfer, struct transferiface *iface, void *data);
void transferdetach(struct transfer *transfer);
void resettransfer(struct transfer *transfer);
void transfersetlocalend(struct transfer *transfer, struct socket *sk);
void *transfergetdata(struct transfer *transfer, size_t *size);
int forkfilter(struct transfer *transfer);
void transferputdata(struct transfer *transfer, void *buf, size_t size);
size_t transferdatasize(struct transfer *transfer);
void transferendofdata(struct transfer *transfer);
void transferprepul(struct transfer *transfer, off_t size, off_t start, off_t end, struct socket *lesk);
void transferstartul(struct transfer *transfer, struct socket *sk);
void transfersethash(struct transfer *transfer, struct hash *hash);
struct transfer *finddownload(wchar_t *peerid);
void transferstartdl(struct transfer *transfer, struct socket *sk);
void trytransferbypeer(struct fnet *fnet, wchar_t *peerid);

extern struct transfer *transfers;
extern unsigned long long bytesupload;
extern unsigned long long bytesdownload;
EGCBCHAIN(newtransfercb, struct transfer *);

#endif
