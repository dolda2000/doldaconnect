/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2007 Fredrik Tolf <fredrik@dolda2000.com>
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

#ifndef _HTTP_H
#define _HTTP_H

#include <netdb.h>

#include "utils.h"

struct hturlinfo {
    char *host;
    int port;
    char *path;
    char *query;
    char *fragment;
};

struct htconn {
    int state;
    int fd;
    struct addrinfo *ailist, *curai;
    char *outbuf, *inbuf, *databuf;
    size_t outbufsize, outbufdata;
    size_t inbufsize, inbufdata;
    size_t databufsize, databufdata;
    struct hturlinfo *url;
    int rescode;
    char *resstr;
    struct strpair *headers;
    ssize_t tlen, rxd, chl;
};

struct htcookie {
    struct htcookie *next;
    char *name, *val;
};

struct hturlinfo *parseurl(char *url);
void freeurl(struct hturlinfo *ui);
void freehtconn(struct htconn *cn);
struct htconn *htconnect(struct hturlinfo *ui);
int htpollflags(struct htconn *hc);
int htprocess(struct htconn *hc, int pollflags);

#endif
