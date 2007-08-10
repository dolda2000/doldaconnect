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
#ifndef _SEARCH_H
#define _SEARCH_H

#include "client.h"
#include "filenet.h"
#include "sysevents.h"
#include <regex.h>
#include <wchar.h>

#define SOP_FALSE 0
#define SOP_TRUE 1
#define SOP_AND 2
#define SOP_OR 3
#define SOP_NOT 4
#define SOP_NAMERE 5
#define SOP_NAMESS 6
#define SOP_LINKRE 7
#define SOP_LINKSS 8
#define SOP_SIZEGT 9
#define SOP_SIZELT 10
#define SOP_SIZEEQ 11
#define SOP_HASHIS 12

#define SRCH_WAIT 0
#define SRCH_RUN 1

struct wcslist
{
    struct wcslist *next, *prev;
    wchar_t *str;
    size_t len;
};

struct sexpr
{
    int refcount;
    int op;
    struct sexpr *l, *r;
    int cost, tcost;
    union
    {
	struct
	{
	    wchar_t *sre;
	    regex_t cre;
	    int inited;
	} re;
	wchar_t *s;
	int n;
	struct hash *hash;
    } d;
};

struct srchfnnlist
{
    struct srchfnnlist *next;
    struct fnetnode *fn;
    void *fnetdata;
    CBCHAIN(searchfnl_destroy, struct srchfnnlist *ln);
};

struct search
{
    struct search *next, *prev;
    int id;
    int state;
    wchar_t *owner;
    int prio;
    time_t eta;
    double committime;
    struct sexpr *sexpr;
    struct srchfnnlist *fnl;
    struct srchres *results;
    int numres;
    struct timer *freetimer;
    CBCHAIN(search_eta, struct search *srch);
    CBCHAIN(search_commit, struct search *srch);
    CBCHAIN(search_result, struct search *srch, struct srchres *sr);
    CBCHAIN(search_destroy, struct search *srch);
};

struct srchres
{
    struct srchres *next, *prev;
    struct search *srch;
    wchar_t *filename;
    struct fnet *fnet;
    wchar_t *peerid, *peernick;
    size_t size;
    int slots;
    struct fnetnode *fn;
    double time;
    struct hash *hash;
};

wchar_t *regexunquotesimple(wchar_t *re);
struct sexpr *parsesexpr(int argc, wchar_t **argv);
void optsexpr(struct sexpr *sexpr);
void getsexpr(struct sexpr *sexpr);
void putsexpr(struct sexpr *sexpr);
struct search *newsearch(wchar_t *owner, struct sexpr *sexpr);
void searchaddfn(struct search *srch, struct fnetnode *fn);
void queuesearch(struct search *srch);
void freesearch(struct search *srch);
struct wcslist *regexfindstrings(wchar_t *re);
void freesl(struct wcslist **list);
void slmergemax(struct wcslist **dest, struct wcslist *src);
struct wcslist *slmergemin(struct wcslist *l1, struct wcslist *l2);
struct wcslist *findsexprstrs(struct sexpr *sexpr);
struct srchres *newsrchres(struct fnet *fnet, wchar_t *filename, wchar_t *peerid);
void freesrchres(struct srchres *sr);
void submitsrchres(struct srchres *sr);
struct search *findsearch(int id);

extern struct search *searches;
EGCBCHAIN(newsrchcb, struct search *);

#endif
