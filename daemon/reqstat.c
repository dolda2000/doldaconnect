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

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <attr/xattr.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "transfer.h"
#include "module.h"
#include "log.h"

struct trdata {
    size_t startpos;
};

static char *fn = NULL;

void filelog(char *format, ...)
{
    FILE *out;
    va_list args;
    char *b, *t, *p;
    time_t now;
    
    if(fn == NULL)
	return;
    if((out = fopen(fn, "a")) == NULL) {
	flog(LOG_WARNING, "could not open reqstat log file `%s': %s", fn, strerror(errno));
	return;
    }
    now = time(NULL);
    va_start(args, format);
    b = vsprintf2(format, args);
    va_end(args);
    t = ctime(&now);
    if((p = strchr(t, '\n')) != NULL)
	*p = 0;
    fprintf(out, "%s: %s\n", t, b);
    free(b);
    fclose(out);
}

void xainc(wchar_t *file, char *an, off_t inc)
{
    char buf[32];
    ssize_t al;
    off_t val;
    char *fn;
    
    if((fn = icswcstombs(file, NULL, NULL)) == NULL) {
	flog(LOG_WARNING, "could not convert filename %ls into local charset: %s", file, strerror(errno));
	return;
    }
    if((al = getxattr(fn, an, buf, sizeof(buf) - 1)) < 0) {
	if(errno != ENOATTR) {
	    flog(LOG_WARNING, "could not get xattr %s on %s: %s", an, fn, strerror(errno));
	    return;
	}
	val = 0;
    } else {
	buf[al] = 0;
	val = strtoll(buf, NULL, 10);
    }
    val += inc;
    al = snprintf(buf, sizeof(buf), "%ji", (intmax_t)val);
    if(setxattr(fn, an, buf, al, 0) < 0)
	flog(LOG_WARNING, "could not set xattr %s on %s: %s", an, fn, strerror(errno));
}

void request(struct transfer *transfer, struct trdata *data)
{
    filelog("request %ls", transfer->path);
    if(confgetint("reqstat", "xa"))
	xainc(transfer->path, "user.dc-req", 1);
}

void start(struct transfer *transfer, struct trdata *data)
{
    filelog("start %ls at %zi", transfer->path, data->startpos);
    if(confgetint("reqstat", "xa"))
	xainc(transfer->path, "user.dc-started", 1);
}

void finish(struct transfer *transfer, struct trdata *data)
{
    filelog("finish %ls at %zi, total %zi", transfer->path, transfer->curpos, transfer->curpos - data->startpos);
    if(confgetint("reqstat", "xa"))
	xainc(transfer->path, "user.dc-bytes", transfer->curpos - data->startpos);
}

static int chattr(struct transfer *transfer, wchar_t *attrib, struct trdata *data)
{
    if(!wcscmp(attrib, L"state")) {
	if(transfer->state == TRNS_MAIN)
	    data->startpos = transfer->curpos;
	start(transfer, data);
    } else if(!wcscmp(attrib, L"path")) {
	if(transfer->path[0] != L'/') {
	    CBUNREG(transfer, trans_ac, data);
	    CBUNREG(transfer, trans_destroy, data);
	    free(data);
	}
	request(transfer, data);
    }
    return(0);
}

static int destroy(struct transfer *transfer, struct trdata *data)
{
    if(transfer->curpos > data->startpos)
	finish(transfer, data);
    free(data);
    return(0);
}

static int reg(struct transfer *transfer, void *uudata)
{
    struct trdata *data;
    
    if(transfer->dir != TRNSD_UP)
	return(0);
    data = memset(smalloc(sizeof(*data)), 0, sizeof(*data));
    CBREG(transfer, trans_ac, (int (*)(struct transfer *, wchar_t *, void *))chattr, NULL, data);
    CBREG(transfer, trans_destroy, (int (*)(struct transfer *, void *))destroy, NULL, data);
    return(0);
}

static int chfile(struct configvar *var, void *uudata)
{
    if(fn != NULL)
	free(fn);
    if(var->val.str[0] == L'\0') {
	fn = NULL;
    } else {
	if((fn = icwcstombs(var->val.str, NULL)) == NULL)
	    flog(LOG_WARNING, "could not convert reqstat filename `%ls' into local charset: %s", var->val.str, strerror(errno));
    }
    return(0);
}

static int init(int hup)
{
    if(!hup) {
	GCBREG(newtransfercb, reg, NULL);
	chfile(confgetvar("reqstat", "file"), NULL);
	CBREG(confgetvar("reqstat", "file"), conf_update, chfile, NULL, NULL);
    }
    return(0);
}

static struct configvar myvars[] = {
    /** The name of a file to log upload request information to. If
     * unspecified, upload requests will not be logged. */
    {CONF_VAR_STRING, "file", {.str = L""}},
    /** If set to true, upload statistics of files will be accumulated
     * in counters stored in extends attributes (see attr(5)) on the
     * files themselves. The data accumulated is the number of
     * requests, the number of successfully started uploads and the
     * number of uploaded bytes. */
    {CONF_VAR_BOOL, "xa", {.num = 0}},
    {CONF_VAR_END}
};

static struct module me = {
    .conf = {
	.vars = myvars
    },
    .init = init,
    .name = "reqstat"
};

MODULE(me)
