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
#include <stdlib.h>
#include <malloc.h>
#include <wchar.h>
#include <wctype.h>
#include <errno.h>
#include <regex.h>
#include <string.h>
#include <time.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "utils.h"
#include "log.h"
#include "sysevents.h"
#include "filenet.h"
#include "client.h"
#include "search.h"

#define TOK_STR 0
#define TOK_SE 1
#define TOK_OP 2
#define TOK_CP 3

struct srchlist
{
    struct srchlist *next, *prev;
    struct search *srch;
};

struct tok
{
    struct tok *next;
    int type;
    union
    {
	wchar_t *str;
	struct sexpr *se;
    } d;
};

struct reinfo
{
    wchar_t *begstr, *endstr, *onestr;
    struct wcslist *strs;
};

struct bound
{
    int min, max;
};

static void trycommit(void);

struct search *searches = NULL;
static struct srchlist *searchqueue = NULL;
static struct timer *committimer = NULL;
GCBCHAIN(newsrchcb, struct search *);

wchar_t *regexunquotesimple(wchar_t *re)
{
    wchar_t *specials, *buf, *p;
    
    specials = L"\\^$.*+?[{()|";
    buf = smalloc((wcslen(re) + 1) * sizeof(wchar_t));
    p = buf;
    for(; *re != L'\0'; re++)
    {
	if(*re == L'\\')
	{
	    re++;
	    if(!*re)
	    {
		*p = L'\0';
		return(buf);
	    }
	    *(p++) = *re;
	} else {
	    if(wcschr(specials, *re) != NULL)
	    {
		free(buf);
		return(NULL);
	    }
	    *(p++) = *re;
	}
    }
    *p = L'\0';
    return(buf);
}

static void freesln(struct wcslist *ln, struct wcslist **list)
{
    if(ln->prev != NULL)
	ln->prev->next = ln->next;
    if(ln->next != NULL)
	ln->next->prev = ln->prev;
    if(ln == *list)
	*list = ln->next;
    free(ln->str);
    free(ln);
}

void freesl(struct wcslist **list)
{
    while(*list != NULL)
	freesln(*list, list);
}

static struct wcslist *newsl(struct wcslist **list, wchar_t *str)
{
    struct wcslist *ln;
    
    ln = smalloc(sizeof(*ln));
    memset(ln, 0, sizeof(*ln));
    ln->str = swcsdup(str);
    ln->len = wcslen(str);
    ln->next = *list;
    ln->prev = NULL;
    if(*list != NULL)
	(*list)->prev = ln;
    *list = ln;
    return(ln);
}

static void slmerge1(struct wcslist **list, wchar_t *str)
{
    size_t len;
    struct wcslist *cur, *next;
    
    len = wcslen(str);
    for(cur = *list; cur != NULL; cur = next)
    {
	next = cur->next;
	if(len <= cur->len)
	{
	    if(((len < cur->len) && wcsexists(cur->str, str)) || !wcscmp(str, cur->str))
		return;
	} else if(len > cur->len) {
	    if(wcsexists(str, cur->str))
		freesln(cur, list);
	}
    }
    newsl(list, str);
}

void slmergemax(struct wcslist **dest, struct wcslist *src)
{
    for(; src != NULL; src = src->next)
	slmerge1(dest, src->str);
}

static struct wcslist *makeminlist1(wchar_t *s1, wchar_t *s2)
{
    int i;
    wchar_t *p1, *p2, c;
    struct wcslist *list;
    
    list = NULL;
    for(p1 = s1; *p1 != L'\0'; p1++)
    {
	for(p2 = s2; *p2 != L'\0'; p2++)
	{
	    for(i = 0; (p1[i] != L'\0') && (p2[i] != L'\0') && (towlower(p1[i]) == towlower(p2[i])); i++);
	    if(i > 0)
	    {
		c = p2[i];
		p2[i] = L'\0';
		slmerge1(&list, p2);
		p2[i] = c;
	    }
	}
    }
    return(list);
}

struct wcslist *slmergemin(struct wcslist *l1, struct wcslist *l2)
{
    struct wcslist *cur1, *cur2, *list, *rlist;
    
    list = NULL;
    for(cur1 = l1; cur1 != NULL; cur1 = cur1->next)
    {
	for(cur2 = l2; cur2 != NULL; cur2 = cur2->next)
	{
	    rlist = makeminlist1(cur1->str, cur2->str);
	    slmergemax(&list, rlist);
	    freesl(&rlist);
	}
    }
    return(list);
}

static struct bound readbound(wchar_t *p, wchar_t **endret)
{
    struct bound ret;
    
    switch(*p)
    {
    case L'?':
	ret.min = 0;
	ret.max = 1;
	p++;
	break;
    case L'*':
	ret.min = 0;
	ret.max = -1;
	p++;
	break;
    case L'+':
	ret.min = 1;
	ret.max = -1;
	p++;
	break;
    case L'{':
	p++;
	ret.min = wcstol(p, &p, 10);
	if(*p == L',')
	{
	    p++;
	    if(*p == L'}')
		ret.max = -1;
	    else
		ret.max = wcstol(p, &p, 10);
	} else {
	    ret.max = ret.min;
	}
	if(*p != L'}')
	{
	    /* Cannot happen in a validated regex... */
	    flog(LOG_WARNING, "BUG? \"Impossible\" case encountered in search.c:readbound() (No `}' after `{'-type bound!");
	} else {
	    p++;
	}
	break;
    default:
	ret.min = 1;
	ret.max = 1;
	break;
    }
    if(endret != NULL)
	*endret = p;
    return(ret);
}

#define fnc(p) do {if((p) != NULL) free(p); (p) = NULL; p ## size = 0; p ## data = 0;} while(0)

static struct reinfo analyzere(wchar_t *re, wchar_t **endret, wchar_t endc)
{
    int i, commit, parsealt;
    struct reinfo ret, sinf;
    struct bound b;
    wchar_t *cs, *ns;
    size_t cssize, csdata, nssize, nsdata, len1, len2, maxlen, minlen;
    wchar_t beg, c;
    struct wcslist *list;
    
    memset(&ret, 0, sizeof(ret));
    commit = parsealt = 0;
    beg = 1;
    cs = ns = NULL;
    cssize = csdata = nssize = nsdata = 0;
    while((*re != endc) && !parsealt)
    {
	switch(*re)
	{
	case L'$':
	case L'^':
	    re++;
	    commit = 1;
	    break;
	case L'.':
	    re++;
	    b = readbound(re, &re);
	    if(b.max != 0)
		commit = 1;
	    break;
	case L'(':
	    re++;
	    sinf = analyzere(re, &re, L')');
	    re++;
	    b = readbound(re, &re);
	    if(sinf.onestr != NULL)
	    {
		for(i = 0; i < b.min; i++)
		{
		    bufcat(cs, sinf.onestr, wcslen(sinf.onestr));
		    bufcat(ns, sinf.onestr, wcslen(sinf.onestr));
		}
		if((b.max == -1) || (b.max > b.min))
		    commit = 1;
	    } else {
		commit = 1;
		if(b.min > 0)
		{
		    if(sinf.begstr != NULL)
			bufcat(cs, sinf.begstr, wcslen(sinf.begstr));
		    if(sinf.endstr != NULL)
			bufcat(ns, sinf.endstr, wcslen(sinf.endstr));
		}
	    }
	    if(sinf.begstr != NULL)
		free(sinf.begstr);
	    if(sinf.endstr != NULL)
		free(sinf.endstr);
	    if(sinf.onestr != NULL)
		free(sinf.onestr);
	    if(b.min > 0)
		slmergemax(&ret.strs, sinf.strs);
	    freesl(&sinf.strs);
	    break;
	case L'[':
	    c = L'\0';
	    re += 2;
	    /* Must exist in a validated RE... */
	    while(*(re++) != L']');
	    readbound(re, &re);
	    commit = 1;
	    break;
	case L'|':
	    re++;
	    parsealt = 1;
	    break;
	case L'\\':
	    re++;
	    /* A validated RE cannot end in a backslash, so fall
	     * through */
	default:
	    c = *(re++);
	    b = readbound(re, &re);
	    for(i = 0; i < b.min; i++)
		addtobuf(cs, c);
	    if((b.max == -1) || (b.max > b.min))
		commit = 1;
	    break;
	}
	if(commit)
	{
	    if(cs != NULL)
		addtobuf(cs, L'\0');
	    if(beg)
	    {
		if(cs != NULL)
		    ret.begstr = swcsdup(cs);
		beg = 0;
	    }
	    if(cs != NULL)
		slmerge1(&ret.strs, cs);
	    fnc(cs);
	    commit = 0;
	    if(ns != NULL)
	    {
		cs = swcsdup(ns);
		cssize = nssize;
		csdata = nsdata;
		fnc(ns);
	    }
	}
    }
    if(cs != NULL)
    {
	addtobuf(cs, L'\0');
	if(beg)
	    ret.onestr = swcsdup(cs);
	else
	    ret.endstr = swcsdup(cs);
	slmerge1(&ret.strs, cs);
	fnc(cs);
    }
    if(parsealt)
    {
	sinf = analyzere(re, &re, endc);
	list = slmergemin(ret.strs, sinf.strs);
	freesl(&ret.strs);
	freesl(&sinf.strs);
	ret.strs = list;
	if(sinf.begstr != NULL)
	{
	    if(ret.begstr != NULL)
	    {
		for(i = 0; (sinf.begstr[i] != L'\0') && (ret.begstr != L'\0') && (ret.begstr[i] == sinf.begstr[i]); i++);
		if(i == 0)
		    free(ret.begstr);
		else
		    ret.begstr[i] = L'\0';
	    }
	    free(sinf.begstr);
	} else {
	    if(ret.begstr != NULL)
	    {
		free(ret.begstr);
		ret.begstr = NULL;
	    }
	}
	if(sinf.endstr != NULL)
	{
	    if(ret.endstr != NULL)
	    {
		len1 = wcslen(ret.endstr);
		len2 = wcslen(sinf.endstr);
		if(len1 < len2)
		{
		    minlen = len1;
		    maxlen = len2;
		} else {
		    minlen = len2;
		    maxlen = len1;
		}
		for(i = 1; (i <= minlen) && (ret.endstr[len1 - i] == sinf.endstr[len2 - i]); i++);
		if(i == 1)
		    free(ret.endstr);
		else if(i <= maxlen)
		    wmemmove(ret.endstr, ret.endstr + (len1 - i) + 1, i);
	    }
	    free(sinf.endstr);
	} else {
	    if(ret.endstr != NULL)
	    {
		free(ret.endstr);
		ret.endstr = NULL;
	    }
	}
	if(sinf.onestr != NULL)
	{
	    if(ret.onestr != NULL)
	    {
		/* XXX: Comparing beginning and end of the onestrs and
		 * create begstr and endstr if there isn't an exact
		 * match.*/
		if(wcscmp(ret.onestr, sinf.onestr))
		{
		    free(ret.onestr);
		    ret.onestr = NULL;
		}
	    }
	    free(sinf.onestr);
	} else {
	    if(ret.onestr != NULL)
	    {
		free(ret.onestr);
		ret.onestr = NULL;
	    }
	}
    }
    if(endret != NULL)
	*endret = re;
    return(ret);
}

#undef fnc

struct wcslist *regexfindstrings(wchar_t *re)
{
    struct reinfo i;
    
    i = analyzere(re, NULL, L'\0');
    if(i.begstr != NULL)
	free(i.begstr);
    if(i.endstr != NULL)
	free(i.endstr);
    if(i.onestr != NULL)
	free(i.onestr);
    return(i.strs);
}

static struct sexpr *newsexpr(void)
{
    struct sexpr *sexpr;
    
    sexpr = smalloc(sizeof(*sexpr));
    memset(sexpr, 0, sizeof(*sexpr));
    sexpr->refcount = 1;
    return(sexpr);
}

void getsexpr(struct sexpr *sexpr)
{
    sexpr->refcount++;
}

void putsexpr(struct sexpr *sexpr)
{
    if(--sexpr->refcount != 0)
	return;
    if(sexpr->l != NULL)
	putsexpr(sexpr->l);
    if(sexpr->r != NULL)
	putsexpr(sexpr->r);
    if((sexpr->op == SOP_NAMERE) || (sexpr->op == SOP_LINKRE))
    {
	if(sexpr->d.re.sre != NULL)
	    free(sexpr->d.re.sre);
	if(sexpr->d.re.inited)
	    regfree(&sexpr->d.re.cre);
    }
    if((sexpr->op == SOP_NAMESS) || (sexpr->op == SOP_LINKSS))
    {
	if(sexpr->d.s != NULL)
	    free(sexpr->d.s);
    }
    free(sexpr);
}

static struct tok *newtok(void)
{
    struct tok *tok;
    
    tok = smalloc(sizeof(*tok));
    memset(tok, 0, sizeof(*tok));
    tok->next = NULL;
    return(tok);
}

static void freetok(struct tok *tok)
{
    if((tok->type == TOK_STR) && (tok->d.str != NULL))
	free(tok->d.str);
    if((tok->type == TOK_SE) && (tok->d.se != NULL))
	putsexpr(tok->d.se);
    free(tok);
}

static void pushtok(struct tok *tok, struct tok **st)
{
    tok->next = *st;
    *st = tok;
}

static struct tok *poptok(struct tok **st)
{
    struct tok *tok;
    
    tok = *st;
    *st = (*st)->next;
    return(tok);
}

int calccost(struct sexpr *sexpr)
{
    sexpr->tcost = sexpr->cost;
    if(sexpr->l != NULL)
	sexpr->tcost += calccost(sexpr->l);
    if(sexpr->r != NULL)
	sexpr->tcost += calccost(sexpr->r);
    return(sexpr->tcost);
}

struct sexpr *parsesexpr(int argc, wchar_t **argv)
{
    int i, done, std;
    struct tok *st, *tok, *tok2;
    struct sexpr *sexpr;
    char *buf;
    wchar_t *wbuf;
    
    std = 0;
    st = NULL;
    for(i = 0; i < argc; i++)
    {
	pushtok(tok = newtok(), &st);
	tok->type = TOK_STR;
	tok->d.str = swcsdup(argv[i]);
	std++;
	do
	{
	    done = 1;
	    if((st->type == TOK_STR) && !wcscmp(st->d.str, L"("))
	    {
		freetok(poptok(&st));
		pushtok(tok = newtok(), &st);
		tok->type = TOK_OP;
		done = 0;
	    } else if((st->type == TOK_STR) && !wcscmp(st->d.str, L")")) {
		freetok(poptok(&st));
		pushtok(tok = newtok(), &st);
		tok->type = TOK_CP;
		done = 0;
	    } else if((st->type == TOK_STR) && (!wcsncmp(st->d.str, L"N~", 2) || !wcsncmp(st->d.str, L"L~", 2))) {
		tok2 = poptok(&st);
		pushtok(tok = newtok(), &st);
		tok->type = TOK_SE;
		sexpr = newsexpr();
		if((wbuf = regexunquotesimple(tok2->d.str + 2)) != NULL)
		{
		    if(tok2->d.str[0] == L'N')
			sexpr->op = SOP_NAMESS;
		    else
			sexpr->op = SOP_LINKSS;
		    sexpr->d.s = wbuf;
		    sexpr->cost = 5;
		} else {
		    if(tok2->d.str[0] == L'N')
			sexpr->op = SOP_NAMERE;
		    else
			sexpr->op = SOP_LINKRE;
		    sexpr->cost = 20;
		    if((buf = icwcstombs(tok2->d.str + 2, "UTF-8")) == NULL)
		    {
			freetok(tok2);
			putsexpr(sexpr);
			goto out_err;
		    }
		    if(regcomp(&sexpr->d.re.cre, buf, REG_EXTENDED | REG_ICASE | REG_NOSUB))
		    {
			freetok(tok2);
			free(buf);
			putsexpr(sexpr);
			goto out_err;
		    }
		    free(buf);
		    sexpr->d.re.inited = 1;
		    sexpr->d.re.sre = swcsdup(tok2->d.str + 2);
		}
		getsexpr(tok->d.se = sexpr);
		freetok(tok2);
		putsexpr(sexpr);
		done = 0;
	    } else if((st->type == TOK_STR) && (!wcsncmp(st->d.str, L"S<", 2) || !wcsncmp(st->d.str, L"S=", 2) || !wcsncmp(st->d.str, L"S>", 2))) {
		tok2 = poptok(&st);
		pushtok(tok = newtok(), &st);
		tok->type = TOK_SE;
		sexpr = newsexpr();
		if(tok2->d.str[1] == L'<')
		    sexpr->op = SOP_SIZELT;
		else if(tok2->d.str[1] == L'=')
		    sexpr->op = SOP_SIZEEQ;
		else
		    sexpr->op = SOP_SIZEGT;
		sexpr->d.n = wcstol(tok2->d.str + 2, NULL, 0);
		sexpr->cost = 0;
		getsexpr(tok->d.se = sexpr);
		freetok(tok2);
		putsexpr(sexpr);
		done = 0;
	    } else if((std >= 3) && (st->type == TOK_CP) && (st->next->type == TOK_SE) && (st->next->next->type == TOK_OP)) {
		freetok(poptok(&st));
		tok = poptok(&st);
		freetok(poptok(&st));
		pushtok(tok, &st);
		std -= 2;
		done = 0;
	    } else if((std >= 2) && (st->type == TOK_SE) && (st->next->type == TOK_STR) && !wcscmp(st->next->d.str, L"!")) {
		sexpr = newsexpr();
		sexpr->op = SOP_NOT;
		sexpr->cost = 0;
		getsexpr(sexpr->l = st->d.se);
		freetok(poptok(&st));
		freetok(poptok(&st));
		pushtok(tok = newtok(), &st);
		tok->type = TOK_SE;
		getsexpr(tok->d.se = sexpr);
		putsexpr(sexpr);
		std -= 1;
		done = 0;
	    } else if((std >= 3) && (st->type == TOK_SE) && (st->next->type == TOK_STR) && (!wcscmp(st->next->d.str, L"&") || !wcscmp(st->next->d.str, L"|")) && (st->next->next->type == TOK_SE)) {
		sexpr = newsexpr();
		if(!wcscmp(st->next->d.str, L"&"))
		    sexpr->op = SOP_AND;
		else
		    sexpr->op = SOP_OR;
		sexpr->cost = 0;
		getsexpr(sexpr->l = st->next->next->d.se);
		getsexpr(sexpr->r = st->d.se);
		freetok(poptok(&st));
		freetok(poptok(&st));
		freetok(poptok(&st));
		pushtok(tok = newtok(), &st);
		tok->type = TOK_SE;
		getsexpr(tok->d.se = sexpr);
		putsexpr(sexpr);
		std -= 2;
		done = 0;
	    }
	} while(!done);
    }
    if((st == NULL) || (st->next != NULL) || (st->type != TOK_SE))
	goto out_err;
    getsexpr(sexpr = st->d.se);
    freetok(st);
    calccost(sexpr);
    return(sexpr);

 out_err:
    while(st != NULL)
	freetok(poptok(&st));
    return(NULL);
}

void optsexpr(struct sexpr *sexpr)
{
    struct sexpr *buf;
    
    if((sexpr->l != NULL) && (sexpr->r != NULL))
    {
	if(sexpr->l->tcost > sexpr->r->tcost)
	{
	    buf = sexpr->r;
	    sexpr->r = sexpr->l;
	    sexpr->l = buf;
	}
    }
    if(sexpr->l != NULL)
	optsexpr(sexpr->l);
    if(sexpr->r != NULL)
	optsexpr(sexpr->r);
}

struct wcslist *findsexprstrs(struct sexpr *sexpr)
{
    struct wcslist *list, *l1, *l2;
    
    list = NULL;
    switch(sexpr->op)
    {
    case SOP_AND:
	list = findsexprstrs(sexpr->l);
	l1 = findsexprstrs(sexpr->r);
	slmergemax(&list, l1);
	freesl(&l1);
	break;
    case SOP_OR:
	l1 = findsexprstrs(sexpr->l);
	l2 = findsexprstrs(sexpr->r);
	list = slmergemin(l1, l2);
	freesl(&l1);
	freesl(&l2);
	break;
    case SOP_NOT:
	break;
    case SOP_NAMERE:
    case SOP_LINKRE:
	list = regexfindstrings(sexpr->d.re.sre);
	break;
    case SOP_NAMESS:
    case SOP_LINKSS:
	slmerge1(&list, sexpr->d.s);
	break;
    default:
	break;
    }
    return(list);
}

static void unlinksqueue(struct srchlist *ln)
{
    if(ln->prev != NULL)
	ln->prev->next = ln->next;
    if(ln->next != NULL)
	ln->next->prev = ln->prev;
    if(ln == searchqueue)
	searchqueue = ln->next;
    free(ln);
}

static void ctexpire(int cancelled, void *data)
{
    committimer = NULL;
    if(!cancelled)
	trycommit();
}

static void estimatequeue(void)
{
    struct srchlist *cur;
    struct srchfnnlist *ln;
    time_t now, start;
    
    if(searchqueue == NULL)
	return;
    start = now = time(NULL);
    for(ln = searchqueue->srch->fnl; ln != NULL; ln = ln->next)
    {
	if((ln->fn->lastsrch != 0) && (ln->fn->lastsrch + ln->fn->srchwait > start))
	    start = ln->fn->lastsrch + ln->fn->srchwait;
    }
    if(start != searchqueue->srch->eta)
    {
	searchqueue->srch->eta = start;
	CBCHAINDOCB(searchqueue->srch, search_eta, searchqueue->srch);
    }
    for(cur = searchqueue->next; cur != NULL; cur = cur->next)
    {
	now = start;
	for(ln = cur->srch->fnl; ln != NULL; ln = ln->next)
	{
	    if(now + ln->fn->srchwait > start)
		start = now + ln->fn->srchwait;
	}
	if(start != cur->srch->eta)
	{
	    cur->srch->eta = start;
	    CBCHAINDOCB(cur->srch, search_eta, cur->srch);
	}
    }
    if((committimer == NULL) || ((time_t)committimer->at != searchqueue->srch->eta))
    {
	if(committimer != NULL)
	    canceltimer(committimer);
	committimer = timercallback(searchqueue->srch->eta, ctexpire, NULL);
    }
}

struct search *findsearch(int id)
{
    struct search *srch;
    
    for(srch = searches; srch != NULL; srch = srch->next)
    {
	if(srch->id == id)
	    break;
    }
    return(srch);
}

struct search *newsearch(wchar_t *owner, struct sexpr *sexpr)
{
    struct search *srch;
    static int id = 0;
    
    srch = smalloc(sizeof(*srch));
    memset(srch, 0, sizeof(*srch));
    srch->id = id++;
    srch->owner = swcsdup(owner);
    if((srch->sexpr = sexpr) != NULL)
	getsexpr(srch->sexpr);
    CBCHAININIT(srch, search_eta);
    CBCHAININIT(srch, search_commit);
    CBCHAININIT(srch, search_result);
    CBCHAININIT(srch, search_destroy);
    srch->next = searches;
    srch->prev = NULL;
    if(searches != NULL)
	searches->prev = srch;
    searches = srch;
    return(srch);
}

static void srchexpire(int cancelled, struct search *srch)
{
    srch->freetimer = NULL;
    if(!cancelled)
	freesearch(srch);
}

static void trycommit(void)
{
    struct srchfnnlist *ln;
    struct search *srch;
    time_t now;
    
    if(searchqueue == NULL)
	return;
    srch = searchqueue->srch;
    now = time(NULL);
    for(ln = srch->fnl; ln != NULL; ln = ln->next)
    {
	if(now < ln->fn->lastsrch + ln->fn->srchwait)
	    break;
    }
    if(ln != NULL)
	return;
    unlinksqueue(searchqueue);
    srch->state = SRCH_RUN;
    srch->eta = time(NULL);
    srch->committime = ntime();
    srch->freetimer = timercallback(ntime() + 300, (void (*)(int, void *))srchexpire, srch);
    CBCHAINDOCB(srch, search_commit, srch);
    for(ln = srch->fnl; ln != NULL; ln = ln->next)
	fnetsearch(ln->fn, srch, ln);
    estimatequeue();
}

void freesearch(struct search *srch)
{
    struct srchfnnlist *ln;
    struct srchlist *sln;
    
    if(srch->prev != NULL)
	srch->prev->next = srch->next;
    if(srch->next != NULL)
	srch->next->prev = srch->prev;
    if(srch == searches)
	searches = srch->next;
    estimatequeue();
    if(srch->freetimer != NULL)
	canceltimer(srch->freetimer);
    CBCHAINDOCB(srch, search_destroy, srch);
    CBCHAINFREE(srch, search_eta);
    CBCHAINFREE(srch, search_commit);
    CBCHAINFREE(srch, search_result);
    CBCHAINFREE(srch, search_destroy);
    while(srch->results != NULL)
	freesrchres(srch->results);
    for(sln = searchqueue; sln != NULL; sln = sln->next)
    {
	if(sln->srch == srch)
	{
	    unlinksqueue(sln);
	    break;
	}
    }
    while(srch->fnl != NULL)
    {
	ln = srch->fnl;
	srch->fnl = ln->next;
	CBCHAINDOCB(ln, searchfnl_destroy, ln);
	CBCHAINFREE(ln, searchfnl_destroy);
	putfnetnode(ln->fn);
	free(ln);
    }
    if(srch->sexpr != NULL)
	putsexpr(srch->sexpr);
    if(srch->owner != NULL)
	free(srch->owner);
    free(srch);
}

void searchaddfn(struct search *srch, struct fnetnode *fn)
{
    struct srchfnnlist *ln;
    
    for(ln = srch->fnl; ln != NULL; ln = ln->next)
    {
	if(ln->fn == fn)
	    return;
    }
    ln = smalloc(sizeof(*ln));
    memset(ln, 0, sizeof(*ln));
    getfnetnode(ln->fn = fn);
    CBCHAININIT(ln, searchfnl_destroy);
    ln->next = srch->fnl;
    srch->fnl = ln;
}

static void linksearch(struct search *srch, struct srchlist *prev)
{
    struct srchlist *new;
    
    new = smalloc(sizeof(*new));
    new->srch = srch;
    if(prev == NULL)
    {
	new->prev = NULL;
	new->next = searchqueue;
	if(searchqueue != NULL)
	    searchqueue->prev = new;
	searchqueue = new;
    } else {
	new->prev = prev;
	if((new->next = prev->next) != NULL)
	    new->next->prev = new;
	prev->next = new;
    }
    GCBCHAINDOCB(newsrchcb, srch);
    estimatequeue();
}

/* 
 * queuesearch is also the "scheduler" function - it finds a suitable
 * place in the queue for the new search. I'll make a weak attempt at
 * describing the algorithm:
 * First, we find the first search that doesn't have a lower priority
 * than this one. If there is no such, we just link this one onto the
 * end of the queue.
 * Then, if we have a search of this priority in the queue with the
 * same owner as the new search, we set lastmine to the search after
 * that one, otherwise, lastmine is the first search of this
 * priority. If lastmine is discovered either to not exist (that is,
 * our last search is at the end of the queue), or to be of lower
 * priority (higher number), we link it in at the appropriate end.
 * Then, we find the next search of the same priority and owner as
 * lastmine, and link this search in before it. That should yield a
 * 'round-robin-like' scheduling within priority boundaries. I think.
 */
void queuesearch(struct search *srch)
{
    struct srchlist *cur, *lastmine, *prev;
    wchar_t *nexto;
    
    for(prev = NULL, cur = searchqueue; cur != NULL; prev = cur, cur = cur->next)
    {
	if(cur->srch->prio >= srch->prio)
	    break;
    }
    if(cur == NULL)
    {
	linksearch(srch, prev);
	return;
    }
    lastmine = cur;
    for(; cur != NULL; prev = cur, cur = cur->next)
    {
	if(!wcscmp(cur->srch->owner, srch->owner))
	    lastmine = cur->next;
	if(cur->srch->prio > srch->prio)
	    break;
    }
    if((lastmine == NULL) || (lastmine->srch->prio > srch->prio))
    {
	linksearch(srch, prev);
	return;
    }
    nexto = lastmine->srch->owner;
    for(cur = lastmine->next; cur != NULL; prev = cur, cur = cur->next)
    {
	if(!wcscmp(cur->srch->owner, nexto))
	    break;
	if(cur->srch->prio > srch->prio)
	    break;
    }
    if(cur == NULL)
    {
	linksearch(srch, prev);
	return;
    }
    linksearch(srch, prev);
}

static int srisvalid(struct srchres *sr, struct sexpr *sexpr)
{
    int ret;
    char *buf;
    wchar_t *p;
    
    switch(sexpr->op)
    {
    case SOP_FALSE:
	return(0);
    case SOP_TRUE:
	return(1);
    case SOP_AND:
	if(!srisvalid(sr, sexpr->l))
	    return(0);
	return(srisvalid(sr, sexpr->r));
    case SOP_OR:
	if(srisvalid(sr, sexpr->l))
	    return(1);
	return(srisvalid(sr, sexpr->r));
    case SOP_NOT:
	return(!srisvalid(sr, sexpr->l));
    case SOP_NAMERE:
	if((buf = icwcstombs(sr->filename, "UTF-8")) == NULL)
	    return(0);
	ret = regexec(&sexpr->d.re.cre, buf, 0, NULL, 0);
	free(buf);
	return(!ret);
    case SOP_LINKRE:
	p = sr->filename;
	if(sr->fnet->filebasename != NULL)
	    p = sr->fnet->filebasename(p);
	if((buf = icwcstombs(p, "UTF-8")) == NULL)
	    return(0);
	ret = regexec(&sexpr->d.re.cre, buf, 0, NULL, 0);
	free(buf);
	return(!ret);
    case SOP_NAMESS:
	return(wcsexists(sr->filename, sexpr->d.s));
    case SOP_LINKSS:
	p = sr->filename;
	if(sr->fnet->filebasename != NULL)
	    p = sr->fnet->filebasename(p);
	return(wcsexists(p, sexpr->d.s));
    case SOP_SIZELT:
	return(sr->size < sexpr->d.n);
    case SOP_SIZEEQ:
	return(sr->size == sexpr->d.n);
    case SOP_SIZEGT:
	return(sr->size > sexpr->d.n);
    }
    return(0);
}

struct srchres *newsrchres(struct fnet *fnet, wchar_t *filename, wchar_t *peerid)
{
    struct srchres *sr;
    
    sr = smalloc(sizeof(*sr));
    memset(sr, 0, sizeof(*sr));
    sr->size = -1;
    sr->slots = -1;
    sr->fnet = fnet;
    sr->filename = swcsdup(filename);
    sr->peerid = swcsdup(peerid);
    return(sr);
}

void freesrchres(struct srchres *sr)
{
    if(sr->next != NULL)
	sr->next->prev = sr->prev;
    if(sr->prev != NULL)
	sr->prev->next = sr->next;
    if(sr->srch != NULL)
    {
	if(sr->srch->results == sr)
	    sr->srch->results = sr->next;
	sr->srch->numres--;
    }
    if(sr->hash != NULL)
	freehash(sr->hash);
    if(sr->filename != NULL)
	free(sr->filename);
    if(sr->peerid != NULL)
	free(sr->peerid);
    if(sr->peernick != NULL)
	free(sr->peernick);
    if(sr->fn != NULL)
	putfnetnode(sr->fn);
    free(sr);
}

struct srchres *dupsrchres(struct srchres *sr)
{
    struct srchres *new;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    new->size = sr->size;
    new->slots = sr->slots;
    new->fnet = sr->fnet;
    if(sr->peerid != NULL)
	new->peerid = swcsdup(sr->peerid);
    if(sr->peernick != NULL)
	new->peernick = swcsdup(sr->peernick);
    if(sr->filename != NULL)
	new->filename = swcsdup(sr->filename);
    if(sr->fn != NULL)
	getfnetnode(new->fn = sr->fn);
    if(sr->hash != NULL)
	new->hash = duphash(sr->hash);
    return(new);
}

static void linksr(struct search *srch, struct srchres *sr)
{
    sr->prev = NULL;
    sr->next = srch->results;
    if(srch->results != NULL)
	srch->results->prev = sr;
    srch->results = sr;
    sr->srch = srch;
    sr->time = ntime() - srch->committime;
    srch->numres++;
}

void submitsrchres(struct srchres *sr)
{
    struct search *srch;
    struct srchfnnlist *ln;
    struct srchres *dsr;
    
    for(srch = searches; srch != NULL; srch = srch->next)
    {
	if(srch->state == SRCH_RUN)
	{
	    if(!srisvalid(sr, srch->sexpr))
		continue;
	    for(ln = srch->fnl; ln != NULL; ln = ln->next)
	    {
		if(((sr->fn != NULL) && (ln->fn == sr->fn)) || ((sr->fn == NULL) && (sr->fnet == ln->fn->fnet)))
		    break;
	    }
	    if(ln != NULL)
	    {
		dsr = dupsrchres(sr);
		linksr(srch, dsr);
		CBCHAINDOCB(srch, search_result, srch, dsr);
	    }
	}
    }
}
