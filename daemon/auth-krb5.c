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
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <pwd.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/param.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "auth.h"
#include "utils.h"
#include "conf.h"
#include "log.h"
#include "module.h"
#include "sysevents.h"

#ifdef HAVE_KRB5

#include <krb5.h>
#include <com_err.h>

struct krb5data
{
    int state;
    krb5_auth_context context;
    krb5_ticket *ticket;
    krb5_creds *creds;
    krb5_ccache ccache;
    int renew;
    struct timer *renewtimer;
    char *username, *cname;
};

static void setrenew(struct krb5data *data);

static krb5_context k5context;
static krb5_principal myprinc;
static krb5_keytab keytab;

static void releasekrb5(struct krb5data *data)
{
    if(data->renewtimer != NULL)
	canceltimer(data->renewtimer);
    if(data->context != NULL)
	krb5_auth_con_free(k5context, data->context);
    if(data->ticket != NULL)
	krb5_free_ticket(k5context, data->ticket);
    if(data->creds != NULL)
	krb5_free_creds(k5context, data->creds);
    if(data->username != NULL)
	free(data->username);
    if(data->cname != NULL)
	free(data->cname);
    free(data);
}

static void release(struct authhandle *auth)
{
    releasekrb5((struct krb5data *)auth->mechdata);
}

static struct krb5data *newkrb5data(void)
{
    struct krb5data *new;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    return(new);
}

static int inithandle(struct authhandle *auth, char *username)
{
    int ret;
    struct krb5data *data;
    
    data = newkrb5data();
    if((ret = krb5_auth_con_init(k5context, &data->context)) != 0)
    {
	flog(LOG_ERR, "could initialize Kerberos auth context: %s", error_message(ret));
	releasekrb5(data);
	return(1);
    }
    krb5_auth_con_setflags(k5context, data->context, KRB5_AUTH_CONTEXT_DO_SEQUENCE);
    data->username = sstrdup(username);
    data->state = 0;
    auth->mechdata = data;
    return(0);
}

/* Copied from MIT Kerberos 5 1.3.3*/
static krb5_boolean my_krb5_kuserok(krb5_context context, krb5_principal principal, const char *luser, const char *loginfile, int authbydef)
{
    struct stat sbuf;
    struct passwd *pwd;
    char pbuf[MAXPATHLEN];
    krb5_boolean isok = FALSE;
    FILE *fp;
    char kuser[65];
    char *princname;
    char linebuf[BUFSIZ];
    char *newline;
    int gobble;

    /* no account => no access */
    if ((pwd = getpwnam(luser)) == NULL) {
	return(FALSE);
    }
    (void) strncpy(pbuf, pwd->pw_dir, sizeof(pbuf) - 1);
    pbuf[sizeof(pbuf) - 1] = '\0';
    (void) strncat(pbuf, loginfile, sizeof(pbuf) - 1 - strlen(pbuf));

    if (access(pbuf, F_OK)) {	 /* not accessible */
	/*
	 * if he's trying to log in as himself, and there is no .k5login file,
	 * let him.  To find out, call
	 * krb5_aname_to_localname to convert the principal to a name
	 * which we can string compare. 
	 */
	if (authbydef) {
	    if (!(krb5_aname_to_localname(context, principal,
					  sizeof(kuser), kuser))
		&& (strcmp(kuser, luser) == 0)) {
		return(TRUE);
	    }
	} else {
	    return(FALSE);
	}
    }
    if (krb5_unparse_name(context, principal, &princname))
	return(FALSE);			/* no hope of matching */

    /* open ~/.k5login */
    if ((fp = fopen(pbuf, "r")) == NULL) {
	free(princname);
	return(FALSE);
    }
    /*
     * For security reasons, the .k5login file must be owned either by
     * the user himself, or by root.  Otherwise, don't grant access.
     */
    if (fstat(fileno(fp), &sbuf)) {
	fclose(fp);
	free(princname);
	return(FALSE);
    }
    if ((sbuf.st_uid != pwd->pw_uid) && sbuf.st_uid) {
	fclose(fp);
	free(princname);
	return(FALSE);
    }

    /* check each line */
    while (!isok && (fgets(linebuf, BUFSIZ, fp) != NULL)) {
	/* null-terminate the input string */
	linebuf[BUFSIZ-1] = '\0';
	newline = NULL;
	/* nuke the newline if it exists */
	if ((newline = strchr(linebuf, '\n')))
	    *newline = '\0';
	if (!strcmp(linebuf, princname)) {
	    isok = TRUE;
	    continue;
	}
	/* clean up the rest of the line if necessary */
	if (!newline)
	    while (((gobble = getc(fp)) != EOF) && gobble != '\n');
    }
    free(princname);
    fclose(fp);
    return(isok);
}

static void renewcreds(int cancelled, struct krb5data *data)
{
    int ret;
    char ccnambuf[50];
    krb5_ccache tmpcc;
    krb5_creds newcreds;
    static int ccserial = 0;
    
    data->renewtimer = NULL;
    if(cancelled)
	return;
    memset(&newcreds, 0, sizeof(newcreds));
    snprintf(ccnambuf, sizeof(ccnambuf), "MEMORY:%i", ccserial++);
    if((ret = krb5_cc_resolve(k5context, ccnambuf, &tmpcc)) != 0)
    {
	flog(LOG_ERR, "could not resolve a temporary ccache `%s': %s", ccnambuf, error_message(ret));
	data->renew = 0;
	return;
    }
    if((ret = krb5_cc_initialize(k5context, tmpcc, data->ticket->enc_part2->client)) != 0)
    {
	flog(LOG_ERR, "could not initialize temporary ccache: %s", error_message(ret));
	krb5_cc_destroy(k5context, tmpcc);
	data->renew = 0;
	return;
    }
    if((ret = krb5_cc_store_cred(k5context, tmpcc, data->creds)) != 0)
    {
	flog(LOG_ERR, "could not store creds into temporary ccache: %s", error_message(ret));
	krb5_cc_destroy(k5context, tmpcc);
	data->renew = 0;
	return;
    }
    if((ret = krb5_get_renewed_creds(k5context, &newcreds, data->ticket->enc_part2->client, tmpcc, NULL)) != 0)
    {
	flog(LOG_ERR, "could not get renewed tickets for %s: %s", data->username, error_message(ret));
	krb5_cc_destroy(k5context, tmpcc);
	data->renew = 0;
	return;
    }
    krb5_free_creds(k5context, data->creds);
    data->creds = NULL;
    if((ret = krb5_copy_creds(k5context, &newcreds, &data->creds)) != 0)
    {
	flog(LOG_ERR, "could not copy renewed creds: %s", error_message(ret));
	krb5_cc_destroy(k5context, tmpcc);
	data->renew = 0;
	return;
    }
    krb5_free_cred_contents(k5context, &newcreds);
    krb5_cc_destroy(k5context, tmpcc);
    flog(LOG_ERR, "successfully renewed krb5 creds for %s", data->username);
    setrenew(data);
}

static void setrenew(struct krb5data *data)
{
    krb5_ticket_times times;
    time_t now, good;
    
    times = data->creds->times;
    if(!times.starttime)
	times.starttime = times.authtime;
    now = time(NULL);
    if(times.endtime < now)
    {
	flog(LOG_DEBUG, "tickets already expired, cannot renew");
	data->renew = 0;
	return;
    }
    good = times.starttime + (((times.endtime - times.starttime) * 9) / 10);
    data->renewtimer = timercallback(good, (void (*)(int, void *))renewcreds, data);
}

static int krbauth(struct authhandle *auth, struct socket *sk, char *passdata)
{
    int ret;
    struct krb5data *data;
    char *msg;
    size_t msglen;
    int authorized;
    krb5_data k5d;
    krb5_flags apopt;
    krb5_creds **fwdcreds;
    
    data = auth->mechdata;
    if(passdata == NULL)
    {
	auth->prompt = AUTH_PR_AUTO;
	if(auth->text != NULL)
	    free(auth->text);
	auth->text = swcsdup(L"Send hex-encoded krb5 data");
	data->state = 1;
	return(AUTH_PASS);
    } else {
	if((msg = hexdecode(passdata, &msglen)) == NULL)
	{
	    if(auth->text != NULL)
		free(auth->text);
	    auth->text = swcsdup(L"Invalid hex encoding");
	    return(AUTH_DENIED);
	}
	switch(data->state)
	{
	case 1:
	    k5d.length = msglen;
	    k5d.data = msg;
	    if((ret = krb5_rd_req(k5context, &data->context, &k5d, myprinc, keytab, &apopt, &data->ticket)) != 0)
	    {
		flog(LOG_INFO, "kerberos authentication failed for %s: %s", data->username, error_message(ret));
		if(auth->text != NULL)
		    free(auth->text);
		auth->text = icmbstowcs((char *)error_message(ret), NULL);
		free(msg);
		return(AUTH_DENIED);
	    }
	    free(msg);
	    if(apopt & AP_OPTS_MUTUAL_REQUIRED)
	    {
		if((ret = krb5_mk_rep(k5context, data->context, &k5d)) != 0)
		{
		    flog(LOG_WARNING, "krb5_mk_rep returned an error: %s", error_message(ret));
		    return(AUTH_ERR);
		}
		msg = hexencode(k5d.data, k5d.length);
		if(auth->text != NULL)
		    free(auth->text);
		auth->text = icmbstowcs(msg, "us-ascii");
		free(msg);
		free(k5d.data);
	    } else {
		if(auth->text != NULL)
		    free(auth->text);
		auth->text = swcsdup(L"");
	    }
	    data->state = 2;
	    return(AUTH_PASS);
	case 2:
	    ret = atoi(msg);
	    free(msg);
	    if(ret == 1)
	    {
		/* That is, the client has accepted us as a valid
		 * server.  Now check if the client is authorized. */
		if((ret = krb5_unparse_name(k5context, data->ticket->enc_part2->client, &data->cname)) != 0)
		{
		    flog(LOG_ERR, "krb_unparse_name returned an error: %s", error_message(ret));
		    return(AUTH_ERR);
		}
		authorized = 0;
		if(!authorized && my_krb5_kuserok(k5context, data->ticket->enc_part2->client, data->username, "/.k5login", 1))
		    authorized = 1;
		/* Allow a seperate ACL for DC principals */
		if(!authorized && my_krb5_kuserok(k5context, data->ticket->enc_part2->client, data->username, "/.dc-k5login", 0))
		    authorized = 1;
		if(authorized)
		{
		    flog(LOG_INFO, "krb5 principal %s successfully authorized as %s%s", data->cname, data->username, (data->creds == NULL)?"":" (with fwd creds)");
		    return(AUTH_SUCCESS);
		} else {
		    flog(LOG_INFO, "krb5 principal %s not authorized as %s", data->cname, data->username);
		}
	    }
	    if(ret == 2)
	    {
		if(auth->text != NULL)
		    free(auth->text);
		auth->text = swcsdup(L"");
		data->state = 3;
		return(AUTH_PASS);
	    }
	    return(AUTH_DENIED);
	case 3:
	    k5d.length = msglen;
	    k5d.data = msg;
	    if((ret = krb5_rd_cred(k5context, data->context, &k5d, &fwdcreds, NULL)) != 0)
	    {
		flog(LOG_ERR, "krb5_rd_cred returned an error: %s", error_message(ret));
		free(msg);
		return(AUTH_ERR);
	    }
	    free(msg);
	    if(*fwdcreds == NULL)
	    {
		flog(LOG_ERR, "forwarded credentials array was empty (from %s)", data->username);
		krb5_free_tgt_creds(k5context, fwdcreds);
		return(AUTH_ERR);
	    }
	    /* Copy only the first credential. (Change this if it becomes a problem) */
	    ret = krb5_copy_creds(k5context, *fwdcreds, &data->creds);
	    krb5_free_tgt_creds(k5context, fwdcreds);
	    if(ret != 0)
	    {
		flog(LOG_ERR, "could not copy forwarded credentials: %s", error_message(ret));
		return(AUTH_ERR);
	    }
	    if(confgetint("auth-krb5", "renewcreds"))
	    {
		data->renew = 1;
		setrenew(data);
	    }
	    if(auth->text != NULL)
		free(auth->text);
	    auth->text = swcsdup(L"");
	    data->state = 2;
	    return(AUTH_PASS);
	default:
	    free(msg);
	    flog(LOG_ERR, "BUG? Invalid state encountered in krbauth: %i", data->state);
	    return(AUTH_ERR);
	}
    }
}

static int opensess(struct authhandle *auth)
{
    int ret;
    struct krb5data *data;
    char *buf, *buf2;
    int fd;
    struct passwd *pwent;
    
    data = auth->mechdata;
    if(data->creds != NULL)
    {
	if((pwent = getpwnam(data->username)) == NULL)
	{
	    flog(LOG_ERR, "could not get passwd entry for forwarded tickets (user %s): %s", data->username, strerror(errno));
	    return(AUTH_ERR);
	}
	if(!confgetint("auth-krb5", "usedefcc"))
	{
	    buf = sprintf2("/tmp/krb5cc_dc_%i_XXXXXX", pwent->pw_uid);
	    if((fd = mkstemp(buf)) < 0)
	    {
		free(buf);
		flog(LOG_ERR, "could not create temporary file for ccache: %s", strerror(errno));
		return(AUTH_ERR);
	    }
	    close(fd);
	    buf2 = sprintf2("FILE:%s", buf);
	    if((ret = krb5_cc_resolve(k5context, buf2, &data->ccache)) != 0)
	    {
		free(buf);
		free(buf2);
		flog(LOG_ERR, "could not resolve ccache name \"%s\": %s", buf2, error_message(ret));
		return(AUTH_ERR);
	    }
	    setenv("KRB5CCNAME", buf2, 1);
	    free(buf2);
	    if((ret = krb5_cc_initialize(k5context, data->ccache, data->ticket->enc_part2->client)) != 0)
	    {
		free(buf);
		flog(LOG_ERR, "could not initialize ccache: %s", error_message(ret));
		return(AUTH_ERR);
	    }
	    if((ret = krb5_cc_store_cred(k5context, data->ccache, data->creds)) != 0)
	    {
		free(buf);
		flog(LOG_ERR, "could not store forwarded TGT into ccache: %s", error_message(ret));
		return(AUTH_ERR);
	    }
	    if(chown(buf, pwent->pw_uid, pwent->pw_gid))
	    {
		free(buf);
		flog(LOG_ERR, "could not chown new ccache to %i:%i: %s", pwent->pw_uid, pwent->pw_gid, strerror(errno));
		return(AUTH_ERR);
	    }
	    free(buf);
	} else {
	    if((buf = (char *)krb5_cc_default_name(k5context)) == NULL) {
		flog(LOG_ERR, "could not get default ccache name");
		return(AUTH_ERR);
	    }
	    if((ret = krb5_cc_resolve(k5context, buf, &data->ccache)) != 0)
	    {
		flog(LOG_ERR, "could not resolve ccache name \"%s\": %s", buf2, error_message(ret));
		return(AUTH_ERR);
	    }
	    setenv("KRB5CCNAME", buf, 1);
	    if((ret = krb5_cc_initialize(k5context, data->ccache, data->ticket->enc_part2->client)) != 0)
	    {
		flog(LOG_ERR, "could not initialize ccache: %s", error_message(ret));
		return(AUTH_ERR);
	    }
	    if((ret = krb5_cc_store_cred(k5context, data->ccache, data->creds)) != 0)
	    {
		flog(LOG_ERR, "could not store forwarded TGT into ccache: %s", error_message(ret));
		return(AUTH_ERR);
	    }
	}
    }
    return(AUTH_SUCCESS);
}

static int closesess(struct authhandle *auth)
{
    struct krb5data *data;
    
    data = auth->mechdata;
    if(data->ccache != NULL)
    {
	krb5_cc_destroy(k5context, data->ccache);
	data->ccache = NULL;
    }
    return(AUTH_SUCCESS);
}

struct authmech authmech_krb5 =
{
    .inithandle = inithandle,
    .release = release,
    .authenticate = krbauth,
    .opensess = opensess,
    .closesess = closesess,
    .name = L"krb5",
    .enabled = 1
};

static int init(int hup)
{
    int ret;
    char *buf;
    krb5_principal newprinc;
    
    if(!hup)
    {
	regmech(&authmech_krb5);
	if((ret = krb5_init_context(&k5context)))
	{
	    flog(LOG_CRIT, "could not initialize Kerberos context: %s", error_message(ret));
	    return(1);
	}
	if((buf = icwcstombs(confgetstr("auth-krb5", "service"), NULL)) == NULL)
	{
	    flog(LOG_CRIT, "could not convert service name (%ls) into local charset: %s", confgetstr("auth-krb5", "service"), strerror(errno));
	    return(1);
	} else {
	    if((ret = krb5_sname_to_principal(k5context, NULL, buf, KRB5_NT_SRV_HST, &myprinc)) != 0)
	    {
		flog(LOG_CRIT, "could not get principal for service %s: %s", buf, error_message(ret));
		free(buf);
		return(1);
	    }
	    free(buf);
	}
	if((buf = icwcstombs(confgetstr("auth-krb5", "keytab"), NULL)) == NULL)
	{
	    flog(LOG_ERR, "could not convert keytab name (%ls) into local charset: %s, using default keytab instead", confgetstr("auth-krb5", "keytab"), strerror(errno));
	    keytab = NULL;
	} else {
	    if((ret = krb5_kt_resolve(k5context, buf, &keytab)) != 0)
	    {
		flog(LOG_ERR, "could not open keytab %s: %s, using default keytab instead", buf, error_message(ret));
		keytab = NULL;
	    }
	    free(buf);
	}
    }
    if(hup)
    {
	if((buf = icwcstombs(confgetstr("auth-krb5", "service"), NULL)) == NULL)
	{
	    flog(LOG_CRIT, "could not convert service name (%ls) into local charset: %s, not updating principal", confgetstr("auth-krb5", "service"), strerror(errno));
	} else {
	    if((ret = krb5_sname_to_principal(k5context, NULL, buf, KRB5_NT_SRV_HST, &newprinc)) != 0)
	    {
		flog(LOG_CRIT, "could not get principal for service %s: %s, not updating principal", buf, error_message(ret));
	    } else {
		krb5_free_principal(k5context, myprinc);
		myprinc = newprinc;
	    }
	    free(buf);
	}
	if(keytab != NULL)
	    krb5_kt_close(k5context, keytab);
	if((buf = icwcstombs(confgetstr("auth-krb5", "keytab"), NULL)) == NULL)
	{
	    flog(LOG_ERR, "could not convert keytab name (%ls) into local charset: %s, using default keytab instead", confgetstr("auth-krb5", "keytab"), strerror(errno));
	    keytab = NULL;
	} else {
	    if((ret = krb5_kt_resolve(k5context, buf, &keytab)) != 0)
	    {
		flog(LOG_ERR, "could not open keytab %s: %s, using default keytab instead", buf, error_message(ret));
		keytab = NULL;
	    }
	    free(buf);
	}
    }
    return(0);
}

static void terminate(void)
{
    if(keytab != NULL)
	krb5_kt_close(k5context, keytab);
    krb5_free_principal(k5context, myprinc);
    krb5_free_context(k5context);
}

static struct configvar myvars[] =
{
    /** The name of the service principal to use for Kerberos V
     * authentication. */
    {CONF_VAR_STRING, "service", {.str = L"doldacond"}},
    /** The path to an alternative keytab file. If unspecified, the
     * system default keytab will be used. */
    {CONF_VAR_STRING, "keytab", {.str = L""}},
    /** Whether to renew renewable credentials automatically before
     * they expire. */
    {CONF_VAR_BOOL, "renewcreds", {.num = 1}},
    /** If true, the default credentials cache will be used, which is
     * useful for e.g. Linux kernel key handling. If false, a file
     * credentials cache will be created using mkstemp(3), using the
     * pattern /tmp/krb5cc_dc_$UID_XXXXXX. */
    {CONF_VAR_BOOL, "usedefcc", {.num = 0}},
    {CONF_VAR_END}
};

static struct module me =
{
    .conf =
    {
	.vars = myvars
    },
    .init = init,
    .terminate = terminate,
    .name = "auth-krb5"
};

MODULE(me);

#endif /* HAVE_KRB5 */
