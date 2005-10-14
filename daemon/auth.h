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
#ifndef _AUTH_H
#define _AUTH_H

#include <wchar.h>

#define AUTH_SUCCESS 0  /* Authentication successful and done */
#define AUTH_DENIED 1   /* Ultimately failed - reason in handle->text */
#define AUTH_PASS 2     /* Pass data - look in handle->prompt */
#define AUTH_ERR 3      /* An error occurred that */

#define AUTH_PR_AUTO 0
#define AUTH_PR_NOECHO 1
#define AUTH_PR_ECHO 2
#define AUTH_PR_INFO 3
#define AUTH_PR_ERROR 4

struct authhandle;

struct authmech
{
    struct authmech *next;
    int enabled;
    wchar_t *name;
    int (*inithandle)(struct authhandle *handle, char *username);
    void (*release)(struct authhandle *handle);
    int (*authenticate)(struct authhandle *handle, char *data);
    int (*renewcred)(struct authhandle *handle);
    int (*opensess)(struct authhandle *handle);
    int (*closesess)(struct authhandle *handle);
};

struct authhandle
{
    int refcount;
    struct authmech *mech;
    int prompt;
    wchar_t *text;
    void *mechdata;
};

int authenticate(struct authhandle *handle, char *data);
struct authhandle *initauth(wchar_t *mechname, char *username);
void authgethandle(struct authhandle *auth);
void authputhandle(struct authhandle *auth);
int authrenewcred(struct authhandle *handle);
int authopensess(struct authhandle *handle);
int authclosesess(struct authhandle *handle);
void regmech(struct authmech *mech);

extern struct authmech *mechs;

#endif
