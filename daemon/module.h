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
#ifndef _MODULE_H
#define _MODULE_H

#include <stdio.h>

#include "conf.h"

struct module
{
    struct module *next;
    char *name;
    /* Called before the configuration file is read. Must either
     * succeed or call exit. Note that it is called both at startup
     * and when SIGHUP has been received (then with hup = 1). */
    void (*preinit)(int hup);
    /* Called when the configuration file has been read. Return zero
     * on success and non-zero on failure. Note, as with preinit, that
     * it can be called both at startup and after SIHUP. */
    int (*init)(int hup);
    /* Called every "cycle". Its return value determines whether the
     * module still has work to do, and thus determines whether the
     * next pollsocks should block or not. Return non-zero whenever
     * the module has more work to do. */
    int (*run)(void);
    /* Called when the daemon is shutting down. */
    void (*terminate)(void);
    struct configmod conf;
};

#define MODULE(mod) \
static void __attribute__ ((constructor)) __regmod(void) \
{ \
    extern struct module *modchain; \
    \
    if(mod.name == NULL) \
    { \
	fprintf(stderr, "module at %p has no name", &mod); \
	exit(1); \
    } \
    mod.conf.name = mod.name; \
    mod.next = modchain; \
    modchain = &mod; \
}

void regmod(struct module *);

#endif
