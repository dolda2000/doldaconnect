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

#include <pwd.h>
#include <sys/un.h>
#include <errno.h>
#include <string.h>
#include <wchar.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "auth.h"
#include "utils.h"
#include "module.h"
#include "conf.h"

struct unixdata {
    char *username;
};

static int inithandle(struct authhandle *auth, char *username)
{
    struct unixdata *data;
    
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    data->username = sstrdup(username);
    auth->mechdata = data;
    return(0);
}

static void release(struct authhandle *auth)
{
    struct unixdata *data;
    
    data = auth->mechdata;
    free(data->username);
    free(data);
}

static int unixauth(struct authhandle *auth, struct socket *sk, char *passdata)
{
    struct passwd *pwd;
    struct unixdata *data;
    
    data = auth->mechdata;
    if((pwd = getpwnam(data->username)) == NULL)
	return(AUTH_ERR);
    if(sk->ucred.uid == -1) {
	errno = EBADE;
	return(AUTH_ERR);
    }
    if(pwd->pw_uid == sk->ucred.uid) {
	flog(LOG_INFO, "successful authentication as %s with Unix credentials (uid=%i, gid=%i)", data->username, sk->ucred.uid, sk->ucred.gid);
	return(AUTH_SUCCESS);
    }
    auth->text = swcsdup(L"Unix credentials do not match supplied user name");
    return(AUTH_DENIED);
}

static int available(struct socket *sk)
{
    return((sk->family == PF_UNIX) && (sk->ucred.uid != -1));
}

static struct authmech mechdesc = {
    .inithandle = inithandle,
    .release = release,
    .authenticate = unixauth,
    .available = available,
    .name = L"unix",
    .enabled = 1
};

static int init(int hup)
{
    if(!hup)
	regmech(&mechdesc);
    return(0);
}

static struct module me = {
    .init = init,
    .name = "auth-unix"
};
MODULE(me)
