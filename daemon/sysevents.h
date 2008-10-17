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
#ifndef _SYSEVENTS_H
#define _SYSEVENTS_H

#include <sys/types.h>
#include "auth.h"

#define FD_END -1
#define FD_PIPE 0
#define FD_FILE 1

struct timer
{
    struct timer *next, *prev;
    double at;
    void (*func)(int cancelled, void *data);
    void *data;
};

struct child
{
    struct child *next, *prev;
    pid_t pid;
    void (*callback)(pid_t pid, int status, void *data);
    void *data;
};

void childcallback(pid_t pid, void (*func)(pid_t, int, void *), void *data);
struct timer *timercallback(double at, void (*func)(int, void *), void *data);
void canceltimer(struct timer *timer);
pid_t forksess(uid_t user, struct authhandle *auth, void (*ccbfunc)(pid_t, int, void *), void *data, ...);

#endif
