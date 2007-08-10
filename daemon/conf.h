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
#ifndef _CONF_H
#define _CONF_H

#include <stddef.h>
#include <stdio.h>
#include <netinet/in.h>
#include <utils.h>

#define CONF_VAR_END -1
#define CONF_VAR_BOOL 0
#define CONF_VAR_INT 1
#define CONF_VAR_STRING 2
#define CONF_VAR_IPV4 3

struct configvar
{
    int type;
    char *name;
    union
    {
	int num;
	wchar_t *str;
	struct in_addr ipv4;
    } defaults;
    union
    {
	int num;
	wchar_t *str;
	struct in_addr ipv4;
    } val;
    CBCHAIN(conf_update, struct configvar *);
};

struct configcmd
{
    char *name;
    int (*handler)(int argc, wchar_t **argv);
};

struct configmod
{
    struct configmod *next;
    char *name;
    struct configvar *vars;
    struct configcmd *cmds;
};

struct configvar *confgetvar(char *modname, char *varname);
#define confgetint(m, v) (confgetvar((m), (v))->val.num)
#define confgetstr(m, v) (confgetvar((m), (v))->val.str)
void confregmod(struct configmod *mod);
void readconfig(FILE *stream);
void storevar(char *key, void *val, size_t len);
void *fetchvar(char *key, size_t *lenb);

#endif
