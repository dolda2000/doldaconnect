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
#include <langinfo.h>
#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <wctype.h>
#include <stddef.h>
#include <wchar.h>
#include <iconv.h>
#include <arpa/inet.h>
#include <gdbm.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "conf.h"
#include "log.h"
#include "utils.h"

static struct configmod *modules = NULL;

#if 0
static void dumpconfig(void)
{
    struct configmod *mod;
    struct configvar *var;
    
    for(mod = modules; mod != NULL; mod = mod->next)
    {
	printf("%s:\n", mod->name);
	for(var = mod->vars; var->type != CONF_VAR_END; var++)
	{
	    switch(var->type)
	    {
	    case CONF_VAR_BOOL:
		printf("\t%s: %s\n", var->name, var->val.num?"t":"f");
		break;
	    case CONF_VAR_INT:
		printf("\t%s: %i\n", var->name, var->val.num);
		break;
	    case CONF_VAR_STRING:
		printf("\t%s: \"%ls\" (%i)\n", var->name, var->val.str, wcslen(var->val.str));
		break;
	    case CONF_VAR_IPV4:
		printf("\t%s: %s\n", var->name, inet_ntoa(var->val.ipv4));
		break;
	    }
	}
    }
}
#endif

struct configvar *confgetvar(char *modname, char *varname)
{
    struct configmod *m;
    struct configvar *v;
    
    for(m = modules; m != NULL; m = m->next)
    {
	if(!strcmp(m->name, modname))
	{
	    for(v = m->vars; v->type != CONF_VAR_END; v++)
	    {
		if(!strcmp(v->name, varname))
		    return(v);
	    }
	    break;
	}
    }
    return(NULL);
}

void confregmod(struct configmod *mod)
{
    struct configvar *var;
    
    for(var = mod->vars; var->type != CONF_VAR_END; var++)
    {
	switch(var->type)
	{
	case CONF_VAR_BOOL:
	case CONF_VAR_INT:
	    var->val.num = var->defaults.num;
	    break;
	case CONF_VAR_STRING:
	    if(var->defaults.str != NULL)
	    {
		var->val.str = swcsdup(var->defaults.str);
	    } else {
		var->val.str = NULL;
	    }
	    break;
	case CONF_VAR_IPV4:
	    var->val.ipv4.s_addr = var->defaults.ipv4.s_addr;
	    break;
	}
	CBCHAININIT(var, conf_update);
    }
    mod->next = modules;
    modules = mod;
}

int runconfcmd(int argc, wchar_t **argv)
{
    struct configmod *module;
    struct configvar *var;
    struct configcmd *cmd;
    int ret, handled;
    wchar_t *p;
    char *cmdn, *buf, *buf2, *valbuf;
    long num;
    struct in_addr newipv4;
    int cb;
    
    if(argc < 1)
	return(0);
    if((cmdn = icwcstombs(argv[0], "us-ascii")) == NULL)
    {
	flog(LOG_WARNING, "could not convert %ls to us-ascii", argv[0]);
	return(1);
    }
    ret = 1;
    handled = 0;
    if(!strcmp(cmdn, "set"))
    {
	handled = 1;
	ret = 0;
	if((p = wcschr(argv[1], L'.')) == NULL)
	{
	    flog(LOG_WARNING, "illegal configuration variable format: %ls", argv[1]);
	    errno = EINVAL;
	    free(cmdn);
	    return(1);
	}
	*(p++) = L'\0';
	if((buf = icwcstombs(argv[1], "us-ascii")) == NULL)
	{
	    flog(LOG_WARNING, "could not convert %ls to us-ascii", argv[1]);
	    free(cmdn);
	    return(1);
	}
	if((buf2 = icwcstombs(p, "us-ascii")) == NULL)
	{
	    free(buf);
	    flog(LOG_WARNING, "could not convert %ls to us-ascii", p);
	    free(cmdn);
	    return(1);
	}
	for(module = modules; module != NULL; module = module->next)
	{
	    if(!strcmp(module->name, buf) && (module->vars != NULL))
	    {
		for(var = module->vars; var->type != CONF_VAR_END; var++)
		{
		    if(!strcmp(var->name, buf2))
		    {
			cb = 0;
			switch(var->type)
			{
			case CONF_VAR_BOOL:
			    wcstolower(argv[2]);
			    if(!wcscmp(argv[2], L"off") ||
			       !wcscmp(argv[2], L"false") ||
			       !wcscmp(argv[2], L"no") ||
			       !wcscmp(argv[2], L"0"))
			    {
				if(var->val.num)
				    cb = 1;
				var->val.num = 0;
			    } else if(!wcscmp(argv[2], L"on") ||
				      !wcscmp(argv[2], L"true") ||
				      !wcscmp(argv[2], L"yes") ||
				      !wcscmp(argv[2], L"1")) {
				if(!var->val.num)
				    cb = 1;
				var->val.num = 1;
			    } else {
				flog(LOG_WARNING, "unrecognized boolean: %ls", argv[2]);
			    }
			    break;
			case CONF_VAR_INT:
			    num = wcstol(argv[2], &p, 0);
			    if(p == argv[2])
			    {
				flog(LOG_WARNING, "%ls: not a number, ignoring", argv[2]);
				ret = 1;
			    } else {
				if(*p != L'\0')
				    flog(LOG_WARNING, "%ls: could not entirely parse as a number, ignoring trailing garbage", argv[2]);
				if(num != var->val.num)
				    cb = 1;
				var->val.num = num;
			    }
			    break;
			case CONF_VAR_STRING:
			    if(wcscmp(var->val.str, argv[2]))
				cb = 1;
			    free(var->val.str);
			    var->val.str = swcsdup(argv[2]);
			    break;
			case CONF_VAR_IPV4:
			    if((valbuf = icwcstombs(argv[2], "us-ascii")) == NULL)
			    {
				flog(LOG_WARNING, "could not convert IPv4 address to as-ascii in var %s, ignoring", buf2);
			    } else {
				if(!inet_aton(valbuf, &newipv4))
				{
				    flog(LOG_WARNING, "could not parse IPv4 address (%s), ignoring", valbuf);
				    memcpy(&var->val.ipv4, &var->defaults.ipv4, sizeof(var->val.ipv4));
				} else {
				    if(memcmp(&newipv4, &var->val.ipv4, sizeof(newipv4)))
					cb = 1;
				    memcpy(&var->val.ipv4, &newipv4, sizeof(newipv4));
				}
				free(valbuf);
			    }
			    break;
			}
			if(cb)
			    CBCHAINDOCB(var, conf_update, var);
			break;
		    }
		}
		if(var == NULL)
		    flog(LOG_WARNING, "variable %s not found, ignoring set command", buf2);
		break;
	    }
	}
	if(module == NULL)
	    flog(LOG_WARNING, "module %s not found, ignoring set command", buf);
	free(buf2);
	free(buf);
    }
    for(module = modules; !handled && (module != NULL); module = module->next)
    {
	if(module->cmds != NULL)
	{
	    for(cmd = module->cmds; cmd->name != NULL; cmd++)
	    {
		if(!strcmp(cmd->name, cmdn))
		{
		    handled = 1;
		    ret = cmd->handler(argc, argv);
		    break;
		}
	    }
	}
    }
    if(!handled)
	flog(LOG_WARNING, "command not found: %s", cmdn);
    free(cmdn);
    return(ret);
}

void readconfig(FILE *stream)
{
    int state;
    wint_t c;
    wchar_t *words[16];
    wchar_t *buf, *p, *p2;
    int w;
    int line;
    
    buf = smalloc(sizeof(wchar_t) * 1024);
    state = 0;
    c = getwc(stream);
    w = 0;
    line = 1;
    p = buf;
    while(c != WEOF)
    {
	if(c == '#')
	{
	    do
		c = getwc(stream);
	    while((c != WEOF) && (c != L'\n'));
	    continue;
	}
	switch(state)
	{
	case 0:
	    if(iswspace(c))
	    {
		if(c == L'\n')
		{
		    line++;
		    if(runconfcmd(w, words))
			flog(LOG_WARNING, "ignoring this command on line %i", line);
		    w = 0;
		}
		c = getwc(stream);
	    } else {
		state = 1;
		p2 = p;
	    }
	    break;
	case 1:
	    if(c == L'\"')
	    {
		state = 2;
		c = getwc(stream);
	    } else if(iswspace(c)) {
		if(w >= 16)
		{
		    flog(LOG_WARNING, "too many words on config line %i, ignoring rest", line);
		} else {
		    *(p++) = L'\0';
		    words[w++] = p2;
		}
		state = 0;
	    } else {
		if(c == L'\\')
		    c = getwc(stream);
		if(p - buf < 1023)
		    *(p++) = c;
		else
		    flog(LOG_WARNING, "too many characters on config line %i, ignoring rest", line);
		c = getwc(stream);
	    }
	    break;
	case 2:
	    if(c == L'\"')
	    {
		c = getwc(stream);
		state = 1;
	    } else {
		if(c == L'\\')
		    c = getwc(stream);
		if(p - buf < 1023)
		    *(p++) = c;
		else
		    flog(LOG_WARNING, "too many characters on config line %i, ignoring rest", line);
		c = getwc(stream);
	    }
	    break;
	}
    }
    free(buf);
    if(ferror(stream))
	flog(LOG_WARNING, "error on configuration stream: %s", strerror(errno));
    if(state != 0)
	flog(LOG_WARNING, "unexpected end of file");
}

/* {store,fetch}var re-opens the database every time, just in case two
 * doldacond processes would be running simultaneously. */
void storevar(char *key, void *val, size_t len)
{
    char *dbname;
    GDBM_FILE db;
    datum k, v;
    
    dbname = findfile("dc-vardb", NULL, 1);
    if((db = gdbm_open(dbname, 0, GDBM_WRCREAT, 0666, NULL)) == NULL)
    {
	flog(LOG_CRIT, "could not open var database for writing, cannot continue: %s", gdbm_strerror(gdbm_errno));
	abort();
    }
    free(dbname);
    k.dptr = key;
    k.dsize = strlen(key);
    v.dptr = val;
    v.dsize = len;
    gdbm_store(db, k, v, GDBM_REPLACE);
    gdbm_close(db);
}

void *fetchvar(char *key, size_t *lenb)
{
    char *dbname;
    GDBM_FILE db;
    datum k, v;
    
    if((dbname = findfile("dc-vardb", NULL, 0)) == NULL)
	return(NULL);
    if((db = gdbm_open(dbname, 0, GDBM_READER, 0666, NULL)) == NULL)
	return(NULL);
    free(dbname);
    k.dptr = key;
    k.dsize = strlen(key);
    v = gdbm_fetch(db, k);
    gdbm_close(db);
    if(v.dptr == NULL)
	return(NULL);
    if(lenb != NULL)
	*lenb = v.dsize;
    return(v.dptr);
}
