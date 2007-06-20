/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2007 Fredrik Tolf (fredrik@dolda2000.com)
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

struct hturlinfo {
    char *host;
    int port;
    char *path;
    char *query;
};

struct htconn {
    int fd;
    char *outbuf, *inbuf;
    size_t outbufsize, outbufdata;
    size_t inbufsize, inbufdata;
    struct hturlinfo *url;
};

struct htcookie {
    struct htcookie *next;
    char *name, *val;
};

#endif
