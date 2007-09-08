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
/*
 * I have decided that I don't like PAM. Maybe I'm inexperienced, so
 * please correct me if I'm wrong, but is it not so that
 * pam_authenticate blocks until the user has fully authenticated
 * herself? That isn't very good in a program that wants to do other
 * things at the same time. In my mind, pam_authenticate should return
 * with a conversation struct every time it wants data.
 *
 * My solution here, for now, is to use the ucontext context switching
 * functions to get back and forth from the conversation
 * function. Ugly? Yes indeed, it most certainly is, but what am I to
 * do, then? If there actually is a good way to do this that is built
 * into PAM, _please_, do mail me about it.
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "auth.h"
#include "utils.h"
#include "conf.h"
#include "log.h"
#include "module.h"

#ifdef HAVE_PAM
#include <ucontext.h>
#include <security/pam_appl.h>

struct pamdata
{
    pam_handle_t *pamh;
    volatile int pamret;
    ucontext_t mainctxt, pamctxt;
    void *pamstack;
    volatile int validctxt;
    volatile int convdone, converr;
    volatile char *passdata;
};

static int pamconv(int nmsg, const struct pam_message **msg, struct pam_response **resp, struct authhandle *auth)
{
    int i;
    struct pamdata *data;
    
    data = auth->mechdata;
    *resp = smalloc(sizeof(**resp) * nmsg);
    for(i = 0; i < nmsg; i++)
    {
	switch(msg[i]->msg_style)
	{
	case PAM_PROMPT_ECHO_OFF:
	    auth->prompt = AUTH_PR_NOECHO;
	    break;
	case PAM_PROMPT_ECHO_ON:
	    auth->prompt = AUTH_PR_ECHO;
	    break;
	case PAM_ERROR_MSG:
	    auth->prompt = AUTH_PR_ERROR;
	    break;
	case PAM_TEXT_INFO:
	    auth->prompt = AUTH_PR_INFO;
	    break;
	}
	if(auth->text != NULL)
	    free(auth->text);
	if((auth->text = icmbstowcs((char *)msg[i]->msg, NULL)) == NULL)
	{
	    flog(LOG_ERR, "could not convert PAM error %s into wcs: %s", msg[i]->msg, strerror(errno));
	    free(*resp);
	    *resp = NULL;
	    return(PAM_CONV_ERR);
	}
	if(swapcontext(&data->pamctxt, &data->mainctxt))
	{
	    flog(LOG_CRIT, "could not swap context in PAM conversation: %s", strerror(errno));
	    free(*resp);
	    *resp = NULL;
	    return(PAM_CONV_ERR);
	}
	if(data->converr)
	{
	    for(i--; i >= 0; i--)
		free((*resp)[i].resp);
	    free(*resp);
	    *resp = NULL;
	    return(PAM_CONV_ERR);
	}
	(*resp)[i].resp_retcode = PAM_SUCCESS;
	switch(msg[i]->msg_style)
	{
	case PAM_PROMPT_ECHO_OFF:
	case PAM_PROMPT_ECHO_ON:
	    (*resp)[i].resp = sstrdup((char *)data->passdata);
	    memset((void *)data->passdata, 0, strlen((char *)data->passdata));
	    break;
	default:
	    (*resp)[i].resp = NULL;
	    break;
	}
    }
    return(PAM_SUCCESS);
}

static void releasepam(struct pamdata *data)
{
    if(data->pamh != NULL)
    {
	if(data->validctxt)
	{
	    data->converr = 1;
	    if(swapcontext(&data->mainctxt, &data->pamctxt))
	    {
		flog(LOG_CRIT, "could not switch back to PAM context while releasing: %s", strerror(errno));
		return;
	    }
	}
	pam_end(data->pamh, data->pamret);
    }
    if(data->pamstack != NULL)
	free(data->pamstack);
    free(data);
}

static void release(struct authhandle *auth)
{
    releasepam((struct pamdata *)auth->mechdata);
}

static struct pamdata *newpamdata(void)
{
    struct pamdata *new;

    new = smalloc(sizeof(*new));
    new->pamh = NULL;
    new->pamret = PAM_SUCCESS;
    new->pamstack = NULL;
    new->validctxt = 0;
    new->converr = 0;
    return(new);
}

static int inithandle(struct authhandle *auth, char *username)
{
    char *buf;
    struct pamdata *data;
    struct pam_conv conv;
    
    data = newpamdata();
    conv.conv = (int (*)(int, const struct pam_message **, struct pam_response **, void *))pamconv;
    conv.appdata_ptr = auth;
    if((buf = icwcstombs(confgetstr("auth-pam", "pamserv"), NULL)) == NULL)
    {
	flog(LOG_ERR, "could not initialize pam since auth-pam.pamserv cannot be translated into the current locale: %s", strerror(errno));
	releasepam(data);
	return(1);
    }
    if((data->pamret = pam_start(buf, username, &conv, &data->pamh)) != PAM_SUCCESS)
    {
	flog(LOG_CRIT, "could not pam_start: %s", pam_strerror(NULL, data->pamret));
	releasepam(data);
	free(buf);
	errno = ENOTSUP; /* XXX */
	return(1);
    }
    free(buf);
    auth->mechdata = data;
    return(0);
}

static void pamauththread(struct authhandle *auth)
{
    struct pamdata *data;
    
    data = (struct pamdata *)auth->mechdata;
    data->validctxt = 1;
    data->pamret = pam_authenticate(data->pamh, 0);
    data->validctxt = 0;
}

static int pamauth(struct authhandle *auth, struct socket *sk, char *passdata)
{
    struct pamdata *data;
    
    data = auth->mechdata;
    if(!data->validctxt)
    {
	if(getcontext(&data->pamctxt))
	{
	    flog(LOG_CRIT, "could not get context: %s", strerror(errno));
	    return(AUTH_ERR);
	}
	data->pamctxt.uc_link = &data->mainctxt;
	if(data->pamstack == NULL)
	    data->pamstack = smalloc(65536);
	data->pamctxt.uc_stack.ss_sp = data->pamstack;
	data->pamctxt.uc_stack.ss_size = 65536;
	makecontext(&data->pamctxt, (void (*)(void))pamauththread, 1, auth);
	if(swapcontext(&data->mainctxt, &data->pamctxt))
	{
	    flog(LOG_CRIT, "Could not switch to PAM context: %s", strerror(errno));
	    return(AUTH_ERR);
	}
	if(!data->validctxt)
	{
	    if(data->pamret == PAM_AUTHINFO_UNAVAIL)
		return(AUTH_ERR);
	    else if(data->pamret == PAM_SUCCESS)
		return(AUTH_SUCCESS);
	    else
		return(AUTH_DENIED);
	}
	return(AUTH_PASS);
    } else {
	data->passdata = passdata;
	if(swapcontext(&data->mainctxt, &data->pamctxt))
	{
	    flog(LOG_CRIT, "could not switch back to PAM context: %s", strerror(errno));
	    return(AUTH_ERR);
	}
	if(!data->validctxt)
	{
	    if(data->pamret == PAM_AUTHINFO_UNAVAIL)
		return(AUTH_ERR);
	    else if(data->pamret == PAM_SUCCESS)
		return(AUTH_SUCCESS);
	    else
		return(AUTH_DENIED);
	}
	return(AUTH_PASS);
    }
}

static int renewcred(struct authhandle *auth)
{
    struct pamdata *data;
    
    data = auth->mechdata;
    if(data->pamh == NULL)
	return(AUTH_SUCCESS);
    data->pamret = pam_setcred(data->pamh, PAM_REFRESH_CRED);
    if(data->pamret != PAM_SUCCESS)
    {
	flog(LOG_INFO, "could not refresh credentials: %s", pam_strerror(data->pamh, data->pamret));
	return(AUTH_ERR);
    }
    return(AUTH_SUCCESS);
}

static int opensess(struct authhandle *auth)
{
    struct pamdata *data;
    char **envp;
    
    data = auth->mechdata;
    if(data->pamh == NULL)
    {
	flog(LOG_ERR, "bug: in auth-pam.c:opensess: called with NULL pamh");
	return(AUTH_ERR);
    }
    data->pamret = pam_setcred(data->pamh, PAM_ESTABLISH_CRED);
    if(data->pamret != PAM_SUCCESS)
    {
	flog(LOG_INFO, "could not establish credentials: %s", pam_strerror(data->pamh, data->pamret));
	return(AUTH_ERR);
    }
    data->pamret = pam_open_session(data->pamh, 0);
    if(data->pamret != PAM_SUCCESS)
    {
	flog(LOG_INFO, "could not open session: %s", pam_strerror(data->pamh, data->pamret));
	return(AUTH_ERR);
    }
    for(envp = pam_getenvlist(data->pamh); *envp; envp++)
	putenv(*envp);
    return(AUTH_SUCCESS);
}

static int closesess(struct authhandle *auth)
{
    int rc;
    struct pamdata *data;
    
    data = auth->mechdata;
    if(data->pamh == NULL)
    {
	flog(LOG_ERR, "bug: in auth-pam.c:closesess: called with NULL pamh");
	return(AUTH_ERR);
    }
    rc = AUTH_SUCCESS;
    data->pamret = pam_close_session(data->pamh, 0);
    if(data->pamret != PAM_SUCCESS)
    {
	flog(LOG_INFO, "could not open session: %s", pam_strerror(data->pamh, data->pamret));
	rc = AUTH_ERR;
    }
    data->pamret = pam_setcred(data->pamh, PAM_DELETE_CRED);
    if(data->pamret != PAM_SUCCESS)
    {
	flog(LOG_INFO, "could not establish credentials: %s", pam_strerror(data->pamh, data->pamret));
	rc = AUTH_ERR;
    }
    return(rc);
}

static struct authmech authmech_pam =
{
    .inithandle = inithandle,
    .release = release,
    .authenticate = pamauth,
    .renewcred = renewcred,
    .opensess = opensess,
    .closesess = closesess,
    .name = L"pam",
    .enabled = 1
};

static int init(int hup)
{
    if(!hup)
	regmech(&authmech_pam);
    return(0);
}

static struct configvar myvars[] =
{
    /** The name of the PAM service file to use. */
    {CONF_VAR_STRING, "pamserv", {.str = L"doldacond"}},
    {CONF_VAR_END}
};

static struct module me =
{
    .conf =
    {
	.vars = myvars
    },
    .init = init,
    .name = "auth-pam"
};

MODULE(me);

#endif /* HAVE_PAM */
