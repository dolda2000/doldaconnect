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
#include <wchar.h>
#include <errno.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "auth.h"
#include "utils.h"
#include "module.h"
#include "conf.h"

struct authmech *mechs = NULL;

static int authless_inithandle(struct authhandle *auth, char *username)
{
    return(0);
}

static void authless_release(struct authhandle *auth)
{
}

static int authless_authenticate(struct authhandle *auth, char *data)
{
    return(AUTH_SUCCESS);
}

static int authless_succeed_1param(struct authhandle *auth)
{
    return(AUTH_SUCCESS);
}

static struct authmech authless =
{
    .name = L"authless",
    .inithandle = authless_inithandle,
    .release = authless_release,
    .authenticate = authless_authenticate,
    .renewcred = authless_succeed_1param,
    .opensess = authless_succeed_1param,
    .closesess = authless_succeed_1param
};

static struct authhandle *newhandle(void)
{
    struct authhandle *auth;
    
    auth = smalloc(sizeof(*auth));
    auth->refcount = 1;
    auth->mech = NULL;
    auth->text = NULL;
    auth->mechdata = NULL;
    return(auth);
}

void authgethandle(struct authhandle *auth)
{
    auth->refcount++;
}

void authputhandle(struct authhandle *auth)
{
    if(--auth->refcount)
	return;
    if(auth->text != NULL)
	free(auth->text);
    if(auth->mechdata != NULL)
	auth->mech->release(auth);
    free(auth);
}

struct authhandle *initauth(wchar_t *mechname, char *username)
{
    struct authmech *mech;
    struct authhandle *auth;
    
    for(mech = mechs; mech != NULL; mech = mech->next)
    {
	if(mech->enabled && !wcscmp(mechname, mech->name))
	    break;
    }
    if(mech == NULL)
    {
	errno = ENOENT;
	return(NULL);
    }
    auth = newhandle();
    auth->mech = mech;
    if(mech->inithandle(auth, username))
    {
	authputhandle(auth);
	return(NULL);
    }
    return(auth);
}

int authenticate(struct authhandle *handle, char *data)
{
    if(handle->mech == NULL)
	return(AUTH_ERR);
    return(handle->mech->authenticate(handle, data));
}

int authrenewcred(struct authhandle *handle)
{
    if((handle->mech == NULL) || (handle->mech->renewcred == NULL))
	return(AUTH_SUCCESS);
    return(handle->mech->renewcred(handle));
}

int authopensess(struct authhandle *handle)
{
    if((handle->mech == NULL) || (handle->mech->opensess == NULL))
	return(AUTH_SUCCESS);
    return(handle->mech->opensess(handle));
}

int authclosesess(struct authhandle *handle)
{
    if((handle->mech == NULL) || (handle->mech->closesess == NULL))
	return(AUTH_SUCCESS);
    return(handle->mech->closesess(handle));
}

void regmech(struct authmech *mech)
{
    mech->next = mechs;
    mechs = mech;
}

static void preinit(int hup)
{
    extern struct authmech authmech_pam;
    
    if(hup)
	return;
    regmech(&authless);
    regmech(&authmech_pam);
}

static int init(int hup)
{
    authless.enabled = confgetint("auth", "authless");
    return(0);
}

static struct configvar myvars[] =
{
    {CONF_VAR_STRING, "pamserv", {.str = L"doldacond"}},
    {CONF_VAR_BOOL, "authless", {.num = 1}},
    {CONF_VAR_END}
};

static struct module me =
{
    .name = "auth",
    .conf =
    {
	.vars = myvars
    },
    .preinit = preinit,
    .init = init
};

MODULE(me)
