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
#ifndef _CLIENT_H
#define _CLIENT_H

#include <sys/types.h>

#include <utils.h>

#define FILE_REG 0
#define FILE_DIR 1
#define FILE_INT 2

#define HASHHASHSIZE 12

struct sharepoint
{
    struct sharepoint *prev, *next;
    char *path;
    wchar_t *name;
    int delete;
};

struct hash
{
    wchar_t *algo;
    size_t len;
    char *buf;
};

struct hashcache
{
    struct hashcache *next, *prev;
    dev_t dev;
    ino_t inode;
    time_t mtime;
    char tth[24];
};

struct sharecache
{
    struct sharecache *next, *prev, *child, *parent;
    char *path;
    wchar_t *name;
    size_t size;
    time_t mtime;
    dev_t dev;
    ino_t inode;
    char hashtth[24];
    union
    {
	struct
	{
	    int type:3;
	    int hastth:1;
	    int found:1;
	} b;
	int w;
    } f;
    CBCHAIN(share_delete, struct sharecache *);
};

void clientpreinit(void);
int clientinit(void);
int doscan(int quantum);
int opensharecache(struct sharecache *node);
struct sharecache *findcache(struct sharecache *parent, wchar_t *name);
void queuescan(struct sharecache *node);
char *getfspath(struct sharecache *node);
struct sharecache *nextscnode(struct sharecache *node);
struct hash *newhash(wchar_t *algo, size_t len, char *hash);
void freehash(struct hash *hash);
struct hash *duphash(struct hash *hash);
struct hash *parsehash(wchar_t *text);
wchar_t *unparsehash(struct hash *hash);

extern struct sharecache *shareroot;
extern unsigned long long sharesize;
EGCBCHAIN(sharechangecb, unsigned long long);

#endif
