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

#include <unistd.h>
/* I'm very unsure about this, but for now it defines wcstoll (which
 * should be defined anyway) and doesn't break anything... let's keep
 * two eyes wide open, though. */
#define __USE_ISOC99
#include <wchar.h>
#include <wctype.h>
#include <pwd.h>
#include <string.h>
#include <malloc.h>
#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

#ifdef HAVE_KRB5
#include <krb5.h>
#include <com_err.h>
#endif

struct logindata;

struct authmech
{
    wchar_t *name;
    void (*process)(struct dc_response *resp, struct logindata *data);
    int (*init)(struct logindata *data);
    void (*release)(struct logindata *data);
};

struct logindata
{
    int (*conv)(int type, wchar_t *text, char **resp, void *data);
    void (*callback)(int err, wchar_t *reason, void *data);
    char *username;
    int freeusername;
    int useauthless;
    void *data;
    void *mechdata;
    struct authmech *mech;
};

struct gencbdata
{
    void (*callback)(int resp, void *data);
    void *data;
};

struct fnetcbdata
{
    void (*callback)(struct dc_fnetnode *fn, int resp, void *data);
    int fnid;
    void *data;
};

struct dc_fnetnode *dc_fnetnodes = NULL;
struct dc_transfer *dc_transfers = NULL;

static void freelogindata(struct logindata *data)
{
    if((data->mech != NULL) && (data->mech->release != NULL))
	data->mech->release(data);
    if(data->freeusername)
	free(data->username);
    free(data);
}

static int logincallback(struct dc_response *resp);

static void process_authless(struct dc_response *resp, struct logindata *data)
{
    switch(resp->code)
    {
    case 200:
	data->callback(DC_LOGIN_ERR_SUCCESS, NULL, data->data);
	freelogindata(data);
	break;
/*
    case 303:
    case 304:
	if((ires = dc_interpret(resp)) != NULL)
	{
	    buf = NULL;
	    if(data->conv((resp->code == 303)?DC_LOGIN_CONV_INFO:DC_LOGIN_CONV_ERROR, ires->argv[0].val.str, &buf, data->data))
	    {
		data->callback(DC_LOGIN_ERR_CONV, NULL, data->data);
		freelogindata(data);
	    } else {
		dc_queuecmd(logincallback, data, L"pass", L"", NULL);
	    }
	    if(buf != NULL)
	    {
		memset(buf, 0, strlen(buf));
		free(buf);
	    }
	    dc_freeires(ires);
	}
	break;
*/
    case 505:
	data->callback(DC_LOGIN_ERR_SERVER, NULL, data->data);
	freelogindata(data);
	break;
    case 506:
	data->callback(DC_LOGIN_ERR_AUTHFAIL, NULL, data->data);
	freelogindata(data);
	break;
    default:
	data->callback(DC_LOGIN_ERR_USER, NULL, data->data);
	freelogindata(data);
	break;
    }
}

static void process_pam(struct dc_response *resp, struct logindata *data)
{
    struct dc_intresp *ires;
    int convtype;
    char *buf;
    
    switch(resp->code)
    {
    case 200:
	data->callback(DC_LOGIN_ERR_SUCCESS, NULL, data->data);
	freelogindata(data);
	break;
    case 301:
    case 302:
    case 303:
    case 304:
	if(resp->code == 301)
	    convtype = DC_LOGIN_CONV_NOECHO;
	else if(resp->code == 302)
	    convtype = DC_LOGIN_CONV_ECHO;
	else if(resp->code == 303)
	    convtype = DC_LOGIN_CONV_INFO;
	else if(resp->code == 304)
	    convtype = DC_LOGIN_CONV_ERROR;
	if((ires = dc_interpret(resp)) != NULL)
	{
	    buf = NULL;
	    if(data->conv(convtype, ires->argv[0].val.str, &buf, data->data))
	    {
		data->callback(DC_LOGIN_ERR_CONV, NULL, data->data);
		freelogindata(data);
	    } else {
		dc_queuecmd(logincallback, data, L"pass", L"%%s", buf, NULL);
	    }
	    if(buf != NULL)
	    {
		memset(buf, 0, strlen(buf));
		free(buf);
	    }
	    dc_freeires(ires);
	}
	break;
    case 505:
	data->callback(DC_LOGIN_ERR_SERVER, NULL, data->data);
	freelogindata(data);
	break;
    case 506:
	data->callback(DC_LOGIN_ERR_AUTHFAIL, NULL, data->data);
	freelogindata(data);
	break;
    default:
	data->callback(DC_LOGIN_ERR_USER, NULL, data->data);
	freelogindata(data);
	break;
    }
}

#ifdef HAVE_KRB5
struct krb5data
{
    int state;
    krb5_context context;
    krb5_principal sprinc, myprinc;
    krb5_ccache ccache;
    krb5_auth_context authcon;
    krb5_data reqbuf;
    krb5_creds *servcreds;
    int valid, fwd, fwded;
};

static char *hexencode(char *data, size_t datalen)
{
    char *buf, this;
    size_t bufsize, bufdata;
    int dig;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(; datalen > 0; datalen--, data++)
    {
	dig = (*data & 0xF0) >> 4;
	if(dig > 9)
	    this = 'A' + dig - 10;
	else
	    this = dig + '0';
	addtobuf(buf, this);
	dig = *data & 0x0F;
	if(dig > 9)
	    this = 'A' + dig - 10;
	else
	    this = dig + '0';
	addtobuf(buf, this);
    }
    addtobuf(buf, 0);
    return(buf);
}

static char *hexdecode(char *data, size_t *len)
{
    char *buf, this;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(; *data; data++)
    {
	if((*data >= 'A') && (*data <= 'F'))
	{
	    this = (this & 0x0F) | ((*data - 'A' + 10) << 4);
	} else if((*data >= '0') && (*data <= '9')) {
	    this = (this & 0x0F) | ((*data - '0') << 4);
	} else {
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	data++;
	if(!*data)
	{
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	if((*data >= 'A') && (*data <= 'F'))
	{
	    this = (this & 0xF0) | (*data - 'A' + 10);
	} else if((*data >= '0') && (*data <= '9')) {
	    this = (this & 0xF0) | (*data - '0');
	} else {
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	addtobuf(buf, this);
    }
    addtobuf(buf, 0);
    if(len != NULL)
	*len = bufdata - 1;
    return(buf);
}

static void process_krb5(struct dc_response *resp, struct logindata *data)
{
    int ret;
    struct dc_intresp *ires;
    struct krb5data *krb;
    krb5_data k5d;
    krb5_ap_rep_enc_part *repl;
    char *buf;
    
    krb = data->mechdata;
    switch(resp->code)
    {
    case 200:
	data->callback(DC_LOGIN_ERR_SUCCESS, NULL, data->data);
	freelogindata(data);
	break;
    case 300:
	switch(krb->state)
	{
	case 0:
	    buf = hexencode(krb->reqbuf.data, krb->reqbuf.length);
	    dc_queuecmd(logincallback, data, L"pass", L"%%s", buf, NULL);
	    free(buf);
	    krb->state = 1;
	    break;
	case 1:
	    if((ires = dc_interpret(resp)) != NULL)
	    {
		k5d.data = hexdecode(icswcstombs(ires->argv[0].val.str, NULL, NULL), &k5d.length);
		if(!krb->valid)
		{
		    if((ret = krb5_rd_rep(krb->context, krb->authcon, &k5d, &repl)) != 0)
		    {
			data->callback(DC_LOGIN_ERR_SERVER, NULL, data->data);
			freelogindata(data);
			break;
		    }
		    /* XXX: Do I need to do something with this? */
		    krb->valid = 1;
		    krb5_free_ap_rep_enc_part(krb->context, repl);
		}
		if(krb->fwd && !krb->fwded)
		{
		    if(krb->reqbuf.data != NULL)
			free(krb->reqbuf.data);
		    krb->reqbuf.data = NULL;
		    if((ret = krb5_fwd_tgt_creds(krb->context, krb->authcon, NULL, krb->servcreds->client, krb->servcreds->server, 0, 1, &krb->reqbuf)) != 0)
		    {
			fprintf(stderr, "krb5_fwd_tgt_creds reported an error: %s\n", error_message(ret));
			dc_queuecmd(logincallback, data, L"pass", L"31", NULL);
			krb->fwd = 0;
			krb->state = 2;
		    } else {
			dc_queuecmd(logincallback, data, L"pass", L"32", NULL);
			krb->state = 0;
			krb->fwded = 1;
		    }
		} else {
		    dc_queuecmd(logincallback, data, L"pass", L"31", NULL);
		    krb->state = 2;
		}
		free(k5d.data);
		dc_freeires(ires);
	    }
	    break;
	default:
	    data->callback(DC_LOGIN_ERR_USER, NULL, data->data);
	    freelogindata(data);
	    break;
	}
	break;
    case 505:
	data->callback(DC_LOGIN_ERR_SERVER, NULL, data->data);
	freelogindata(data);
	break;
    case 506:
	data->callback(DC_LOGIN_ERR_AUTHFAIL, NULL, data->data);
	freelogindata(data);
	break;
    default:
	data->callback(DC_LOGIN_ERR_USER, NULL, data->data);
	freelogindata(data);
	break;
    }
}

static int init_krb5(struct logindata *data)
{
    int ret;
    struct krb5data *krb;
    krb5_data cksum;
    krb5_creds creds;
    
    krb = smalloc(sizeof(*krb));
    memset(krb, 0, sizeof(*krb));
    krb->fwd = 1;
    krb->fwded = 0;
    data->mechdata = krb;
    if((ret = krb5_init_context(&krb->context)) != 0)
    {
	fprintf(stderr, "krb5_init_context reported an error: %s\n", error_message(ret));
	return(1);
    }
    if((ret = krb5_auth_con_init(krb->context, &krb->authcon)) != 0)
    {
	fprintf(stderr, "krb5_auth_con_init reported an error: %s\n", error_message(ret));
	return(1);
    }
    krb5_auth_con_setflags(krb->context, krb->authcon, KRB5_AUTH_CONTEXT_DO_SEQUENCE);
    if((ret = krb5_sname_to_principal(krb->context, dc_gethostname(), "doldacond", KRB5_NT_SRV_HST, &krb->sprinc)) != 0)
    {
	fprintf(stderr, "krb5_sname_to_principal reported an error: %s\n", error_message(ret));
	return(1);
    }
    if((ret = krb5_cc_default(krb->context, &krb->ccache)) != 0)
    {
	fprintf(stderr, "krb5_cc_default reported an error: %s\n", error_message(ret));
	return(1);
    }
    if((ret = krb5_cc_get_principal(krb->context, krb->ccache, &krb->myprinc)) != 0)
    {
	fprintf(stderr, "krb5_cc_default reported an error: %s\n", error_message(ret));
	return(1);
    }
    memset(&creds, 0, sizeof(creds));
    creds.client = krb->myprinc;
    creds.server = krb->sprinc;
    if((ret = krb5_get_credentials(krb->context, 0, krb->ccache, &creds, &krb->servcreds)) != 0)
    {
	fprintf(stderr, "krb5_get_credentials reported an error: %s\n", error_message(ret));
	return(1);
    }
    /* WTF is this checksum stuff?! The Krb docs don't say a word about it! */
    cksum.data = sstrdup(dc_gethostname());
    cksum.length = strlen(cksum.data);
    if((ret = krb5_mk_req_extended(krb->context, &krb->authcon, AP_OPTS_MUTUAL_REQUIRED, &cksum, krb->servcreds, &krb->reqbuf)) != 0)
    {
	fprintf(stderr, "krb5_mk_req_extended reported an error: %s\n", error_message(ret));
	return(1);
    }
    free(cksum.data);
    krb->state = 0;
    return(0);
}

static void release_krb5(struct logindata *data)
{
    struct krb5data *krb;
    
    if((krb = data->mechdata) == NULL)
	return;
    if(krb->servcreds != NULL)
	krb5_free_creds(krb->context, krb->servcreds);
    if(krb->reqbuf.data != NULL)
	free(krb->reqbuf.data);
    if(krb->sprinc != NULL)
	krb5_free_principal(krb->context, krb->sprinc);
    if(krb->myprinc != NULL)
	krb5_free_principal(krb->context, krb->myprinc);
    if(krb->ccache != NULL)
	krb5_cc_close(krb->context, krb->ccache);
    if(krb->authcon != NULL)
	krb5_auth_con_free(krb->context, krb->authcon);
    if(krb->context != NULL)
	krb5_free_context(krb->context);
    free(krb);
}
#endif

/* Arranged in order of priority */
static struct authmech authmechs[] =
{
#ifdef HAVE_KRB5
    {
	.name = L"krb5",
	.process = process_krb5,
	.init = init_krb5,
	.release = release_krb5
    },
#endif
    {
	.name = L"authless",
	.process = process_authless,
	.init = NULL,
	.release = NULL
    },
    {
	.name = L"pam",
	.process = process_pam,
	.init = NULL,
	.release = NULL
    },
    {
	.name = NULL
    }
};

static int builtinconv(int type, wchar_t *text, char **resp, void *data)
{
    char *buf, *pass;
    
    if(isatty(0))
    {
	if((buf = icwcstombs(text, NULL)) == NULL)
	    return(1);
	pass = getpass(buf);
	free(buf);
	*resp = sstrdup(pass);
	memset(pass, 0, strlen(pass));
	return(0);
    }
    return(1);
}

static int logincallback(struct dc_response *resp)
{
    int i;
    struct dc_intresp *ires;
    struct logindata *data;
    int mech;
    char *username;
    struct passwd *pwent;
    void *odata, *ndata;
    
    data = resp->data;
    if(!wcscmp(resp->cmdname, L"lsauth"))
    {
	if(resp->code == 201)
	{
	    data->callback(DC_LOGIN_ERR_NOLOGIN, NULL, data->data);
	    freelogindata(data);
	} else {
	    mech = -1;
	    while((ires = dc_interpret(resp)) != NULL)
	    {
		if(!data->useauthless && !wcscmp(ires->argv[0].val.str, L"authless"))
		{
		    dc_freeires(ires);
		    continue;
		}
		for(i = 0; authmechs[i].name != NULL; i++)
		{
		    if(!wcscmp(authmechs[i].name, ires->argv[0].val.str) && ((i < mech) || (mech == -1)))
		    {
			odata = data->mechdata;
			data->mechdata = NULL;
			if((authmechs[i].init != NULL) && authmechs[i].init(data))
			{
			    if(authmechs[i].release != NULL)
				authmechs[i].release(data);
			    data->mechdata = odata;
			    fprintf(stderr, "authentication mechanism %ls failed, trying further...\n", authmechs[i].name);
			} else {
			    if((data->mech != NULL) && data->mech->release != NULL)
			    {
				ndata = data->mechdata;
				data->mechdata = odata;
				data->mech->release(data);
				data->mechdata = ndata;
			    }
			    mech = i;
			    data->mech = authmechs + i;
			}
			break;
		    }
		}
		dc_freeires(ires);
	    }
	    if(mech == -1)
	    {
		data->callback(DC_LOGIN_ERR_NOLOGIN, NULL, data->data);
		freelogindata(data);
	    } else {
		if((username = data->username) == NULL)
		{
		    if((pwent = getpwuid(getuid())) == NULL)
		    {
			data->callback(DC_LOGIN_ERR_USER, NULL, data->data);
			freelogindata(data);
			return(1);
		    }
		    username = pwent->pw_name;
		}
		dc_queuecmd(logincallback, data, L"login", data->mech->name, L"%%s", username, NULL);
	    }
	}
    } else if(!wcscmp(resp->cmdname, L"login") || !wcscmp(resp->cmdname, L"pass")) {
	data->mech->process(resp, data);
    }
    return(1);
}

void dc_loginasync(char *username, int useauthless, int (*conv)(int, wchar_t *, char **, void *), void (*callback)(int, wchar_t *, void *), void *udata)
{
    struct logindata *data;
    
    data = smalloc(sizeof(*data));
    if(conv == NULL)
	conv = builtinconv;
    data->conv = conv;
    data->mech = NULL;
    data->data = udata;
    data->mechdata = NULL;
    data->callback = callback;
    data->useauthless = useauthless;
    data->freeusername = 0;
    if(username == NULL)
    {
	data->username = NULL;
    } else {
	data->username = sstrdup(username);
	data->freeusername = 1;
    }
    dc_queuecmd(logincallback, data, L"lsauth", NULL);
}

static struct dc_fnetpeerdatum *finddatum(struct dc_fnetnode *fn, wchar_t *id)
{
    struct dc_fnetpeerdatum *datum;
    
    for(datum = fn->peerdata; datum != NULL; datum = datum->next)
    {
	if(!wcscmp(datum->id, id))
	    break;
    }
    return(datum);
}

static struct dc_fnetpeerdatum *adddatum(struct dc_fnetnode *fn, wchar_t *id, int dt)
{
    struct dc_fnetpeerdatum *datum;
    
    datum = smalloc(sizeof(*datum));
    memset(datum, 0, sizeof(*datum));
    datum->refcount = 0;
    datum->dt = dt;
    datum->id = swcsdup(id);
    datum->prev = NULL;
    datum->next = fn->peerdata;
    if(fn->peerdata != NULL)
	fn->peerdata->prev = datum;
    fn->peerdata = datum;
    return(datum);
}

static struct dc_fnetpeerdi *difindoradd(struct dc_fnetpeer *peer, struct dc_fnetpeerdatum *datum)
{
    int i;
    
    for(i = 0; i < peer->dinum; i++)
    {
	if(peer->di[i].datum == datum)
	    return(&peer->di[i]);
    }
    peer->di = srealloc(peer->di, sizeof(struct dc_fnetpeerdi) * ++(peer->dinum));
    memset(&peer->di[i], 0, sizeof(struct dc_fnetpeerdi));
    peer->di[i].datum = datum;
    datum->refcount++;
    return(&peer->di[i]);
}

static void putdatum(struct dc_fnetnode *fn, struct dc_fnetpeerdatum *datum)
{
    if(--datum->refcount > 0)
	return;
    if(datum->next != NULL)
	datum->next->prev = datum->prev;
    if(datum->prev != NULL)
	datum->prev->next = datum->next;
    if(fn->peerdata == datum)
	fn->peerdata = datum->next;
    free(datum->id);
    free(datum);
}

static void peersetnum(struct dc_fnetpeer *peer, wchar_t *id, int value)
{
    struct dc_fnetpeerdatum *datum;
    struct dc_fnetpeerdi *di;
    
    if((datum = finddatum(peer->fn, id)) == NULL)
	datum = adddatum(peer->fn, id, DC_FNPD_INT);
    di = difindoradd(peer, datum);
    di->d.num = value;
}

static void peersetlnum(struct dc_fnetpeer *peer, wchar_t *id, long long value)
{
    struct dc_fnetpeerdatum *datum;
    struct dc_fnetpeerdi *di;
    
    if((datum = finddatum(peer->fn, id)) == NULL)
	datum = adddatum(peer->fn, id, DC_FNPD_INT);
    di = difindoradd(peer, datum);
    di->d.lnum = value;
}

static void peersetstr(struct dc_fnetpeer *peer, wchar_t *id, wchar_t *value)
{
    struct dc_fnetpeerdatum *datum;
    struct dc_fnetpeerdi *di;
    
    if((datum = finddatum(peer->fn, id)) == NULL)
	datum = adddatum(peer->fn, id, DC_FNPD_INT);
    di = difindoradd(peer, datum);
    if(di->d.str != NULL)
	free(di->d.str);
    di->d.str = swcsdup(value);
}

struct dc_fnetpeer *dc_fnetfindpeer(struct dc_fnetnode *fn, wchar_t *id)
{
    struct dc_fnetpeer *peer;
    
    for(peer = fn->peers; peer != NULL; peer = peer->next)
    {
	if(!wcscmp(peer->id, id))
	    break;
    }
    return(peer);
}

static struct dc_fnetpeer *addpeer(struct dc_fnetnode *fn, wchar_t *id, wchar_t *nick)
{
    struct dc_fnetpeer *peer;
    
    peer = smalloc(sizeof(*peer));
    memset(peer, 0, sizeof(*peer));
    peer->fn = fn;
    peer->id = swcsdup(id);
    peer->nick = swcsdup(nick);
    peer->next = fn->peers;
    peer->prev = NULL;
    if(fn->peers != NULL)
	fn->peers->prev = peer;
    fn->peers = peer;
    return(peer);
}

static void delpeer(struct dc_fnetpeer *peer)
{
    int i;
    
    if(peer->next != NULL)
	peer->next->prev = peer->prev;
    if(peer->prev != NULL)
	peer->prev->next = peer->next;
    if(peer->fn->peers == peer)
	peer->fn->peers = peer->next;
    free(peer->id);
    free(peer->nick);
    for(i = 0; i < peer->dinum; i++)
    {
	if((peer->di[i].datum->dt == DC_FNPD_STR) && (peer->di[i].d.str != NULL))
	    free(peer->di[i].d.str);
	putdatum(peer->fn, peer->di[i].datum);
    }
    free(peer);
}

static struct dc_fnetnode *newfn(void)
{
    struct dc_fnetnode *fn;
    
    fn = smalloc(sizeof(*fn));
    memset(fn, 0, sizeof(*fn));
    fn->id = -1;
    fn->name = NULL;
    fn->fnet = NULL;
    fn->state = fn->numusers = fn->found = 0;
    fn->destroycb = NULL;
    fn->udata = NULL;
    fn->next = dc_fnetnodes;
    fn->prev = NULL;
    if(dc_fnetnodes != NULL)
	dc_fnetnodes->prev = fn;
    dc_fnetnodes = fn;
    return(fn);
}

static void freefn(struct dc_fnetnode *fn)
{
    if(fn->next != NULL)
	fn->next->prev = fn->prev;
    if(fn->prev != NULL)
	fn->prev->next = fn->next;
    if(fn == dc_fnetnodes)
	dc_fnetnodes = fn->next;
    if(fn->destroycb != NULL)
	fn->destroycb(fn);
    while(fn->peers != NULL)
	delpeer(fn->peers);
    while(fn->peerdata != NULL)
    {
	fn->peerdata->refcount = 0;
	putdatum(fn, fn->peerdata);
    }
    if(fn->name != NULL)
	free(fn->name);
    if(fn->fnet != NULL)
	free(fn->fnet);
    free(fn);
}

struct dc_fnetnode *dc_findfnetnode(int id)
{
    struct dc_fnetnode *fn;
    
    for(fn = dc_fnetnodes; fn != NULL; fn = fn->next)
    {
	if(fn->id == id)
	    break;
    }
    return(fn);
}

static struct dc_transfer *newtransfer(void)
{
    struct dc_transfer *transfer;
    
    transfer = smalloc(sizeof(*transfer));
    memset(transfer, 0, sizeof(*transfer));
    transfer->id = -1;
    transfer->peerid = transfer->peernick = transfer->path = NULL;
    transfer->state = DC_TRNS_WAITING;
    transfer->dir = DC_TRNSD_UNKNOWN;
    transfer->size = -1;
    transfer->curpos = -1;
    transfer->destroycb = NULL;
    transfer->udata = NULL;
    transfer->next = dc_transfers;
    transfer->prev = NULL;
    if(dc_transfers != NULL)
	dc_transfers->prev = transfer;
    dc_transfers = transfer;
    return(transfer);
}

static void freetransfer(struct dc_transfer *transfer)
{
    if(transfer->next != NULL)
	transfer->next->prev = transfer->prev;
    if(transfer->prev != NULL)
	transfer->prev->next = transfer->next;
    if(transfer == dc_transfers)
	dc_transfers = transfer->next;
    if(transfer->destroycb != NULL)
	transfer->destroycb(transfer);
    if(transfer->peerid != NULL)
	free(transfer->peerid);
    if(transfer->peernick != NULL)
	free(transfer->peernick);
    if(transfer->path != NULL)
	free(transfer->path);
    free(transfer);
}

struct dc_transfer *dc_findtransfer(int id)
{
    struct dc_transfer *transfer;
    
    for(transfer = dc_transfers; transfer != NULL; transfer = transfer->next)
    {
	if(transfer->id == id)
	    break;
    }
    return(transfer);
}

static int getfnlistcallback(struct dc_response *resp)
{
    struct dc_intresp *ires;
    struct gencbdata *data;
    struct dc_fnetnode *fn, *next;
    
    data = resp->data;
    if(resp->code == 200)
    {
	for(fn = dc_fnetnodes; fn != NULL; fn = fn->next)
	    fn->found = 0;
	while((ires = dc_interpret(resp)) != NULL)
	{
	    if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	    {
		fn->found = 1;
		if(fn->fnet != NULL)
		    free(fn->fnet);
		fn->fnet = swcsdup(ires->argv[1].val.str);
		if(fn->name != NULL)
		    free(fn->name);
		fn->name = swcsdup(ires->argv[2].val.str);
		fn->numusers = ires->argv[3].val.num;
		fn->state = ires->argv[4].val.num;
	    } else {
		fn = newfn();
		fn->id = ires->argv[0].val.num;
		fn->fnet = swcsdup(ires->argv[1].val.str);
		fn->name = swcsdup(ires->argv[2].val.str);
		fn->numusers = ires->argv[3].val.num;
		fn->state = ires->argv[4].val.num;
		fn->found = 1;
	    }
	    dc_freeires(ires);
	}
	for(fn = dc_fnetnodes; fn != NULL; fn = next)
	{
	    next = fn->next;
	    if(!fn->found)
		freefn(fn);
	}
	data->callback(200, data->data);
	free(resp->data);
    } else if(resp->code == 201) {
	while(dc_fnetnodes != NULL)
	    freefn(dc_fnetnodes);
	data->callback(201, data->data);
	free(resp->data);
    } else if(resp->code == 502) {
	while(dc_fnetnodes != NULL)
	    freefn(dc_fnetnodes);
	data->callback(502, data->data);
	free(resp->data);
    }
    return(1);
}

static int gettrlistcallback(struct dc_response *resp)
{
    struct dc_intresp *ires;
    struct gencbdata *data;
    struct dc_transfer *transfer, *next;
    
    data = resp->data;
    if(resp->code == 200)
    {
	for(transfer = dc_transfers; transfer != NULL; transfer = transfer->next)
	    transfer->found = 0;
	while((ires = dc_interpret(resp)) != NULL)
	{
	    if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	    {
		transfer->found = 1;
		if((transfer->path == NULL) || wcscmp(transfer->path, ires->argv[5].val.str))
		{
		    if(transfer->path != NULL)
			free(transfer->path);
		    transfer->path = swcsdup(ires->argv[5].val.str);
		}
		if((transfer->peerid == NULL) || wcscmp(transfer->peerid, ires->argv[3].val.str))
		{
		    if(transfer->peerid != NULL)
			free(transfer->peerid);
		    transfer->peerid = swcsdup(ires->argv[3].val.str);
		}
		if((transfer->peernick == NULL) || wcscmp(transfer->peernick, ires->argv[4].val.str))
		{
		    if(transfer->peernick != NULL)
			free(transfer->peernick);
		    transfer->peernick = swcsdup(ires->argv[4].val.str);
		}
		transfer->dir = ires->argv[1].val.num;
		transfer->state = ires->argv[2].val.num;
		transfer->size = ires->argv[6].val.num;
		transfer->curpos = ires->argv[7].val.num;
		if(transfer->hash != NULL)
		{
		    free(transfer->hash);
		    transfer->hash = NULL;
		}
		if(wcslen(ires->argv[8].val.str) > 0)
		    transfer->hash = swcsdup(ires->argv[8].val.str);
	    } else {
		transfer = newtransfer();
		transfer->id = ires->argv[0].val.num;
		transfer->dir = ires->argv[1].val.num;
		transfer->state = ires->argv[2].val.num;
		transfer->peerid = swcsdup(ires->argv[3].val.str);
		transfer->peernick = swcsdup(ires->argv[4].val.str);
		transfer->path = swcsdup(ires->argv[5].val.str);
		transfer->size = ires->argv[6].val.num;
		transfer->curpos = ires->argv[7].val.num;
		if(wcslen(ires->argv[8].val.str) > 0)
		    transfer->hash = swcsdup(ires->argv[8].val.str);
		transfer->found = 1;
	    }
	    dc_freeires(ires);
	}
	for(transfer = dc_transfers; transfer != NULL; transfer = next)
	{
	    next = transfer->next;
	    if(!transfer->found)
		freetransfer(transfer);
	}
	data->callback(200, data->data);
	free(data);
    } else if(resp->code == 201) {
	while(dc_transfers != NULL)
	    freetransfer(dc_transfers);
	data->callback(201, data->data);
	free(data);
    } else if(resp->code == 502) {
	while(dc_transfers != NULL)
	    freetransfer(dc_transfers);
	data->callback(502, data->data);
	free(data);
    }
    return(1);
}

static int getpeerlistcallback(struct dc_response *resp)
{
    int i, o;
    struct dc_fnetnode *fn;
    struct fnetcbdata *data;
    struct dc_fnetpeer *peer, *next;
    struct dc_fnetpeerdatum *datum;
    
    data = resp->data;
    if((fn = dc_findfnetnode(data->fnid)) == NULL)
    {
	data->callback(NULL, -1, data->data);
	free(data);
	return(1);
    }
    if(resp->code == 200)
    {
	for(peer = fn->peers; peer != NULL; peer = peer->next)
	    peer->found = 0;
	for(i = 0; i < resp->numlines; i++)
	{
	    if((peer = dc_fnetfindpeer(fn, resp->rlines[i].argv[1])) == NULL)
		peer = addpeer(fn, resp->rlines[i].argv[1], resp->rlines[i].argv[2]);
	    peer->found = 1;
	    for(o = 3; o < resp->rlines[i].argc; o += 2)
	    {
		if((datum = finddatum(fn, resp->rlines[i].argv[o])) != NULL)
		{
		    switch(datum->dt)
		    {
		    case DC_FNPD_INT:
			peersetnum(peer, datum->id, wcstol(resp->rlines[i].argv[o + 1], NULL, 10));
			break;
		    case DC_FNPD_LL:
			peersetlnum(peer, datum->id, wcstoll(resp->rlines[i].argv[o + 1], NULL, 10));
			break;
		    case DC_FNPD_STR:
			peersetstr(peer, datum->id, resp->rlines[i].argv[o + 1]);
			break;
		    }
		}
	    }
	}
	for(peer = fn->peers; peer != NULL; peer = next)
	{
	    next = peer->next;
	    if(!peer->found)
		delpeer(peer);
	}
    } else if(resp->code == 201) {
	while(fn->peers != NULL)
	    delpeer(fn->peers);
    }
    data->callback(fn, resp->code, data->data);
    free(data);
    return(1);
}

static int getpalistcallback(struct dc_response *resp)
{
    struct dc_fnetnode *fn;
    struct dc_intresp *ires;
    struct fnetcbdata *data;
    
    data = resp->data;
    if((fn = dc_findfnetnode(data->fnid)) == NULL)
    {
	data->callback(NULL, -1, data->data);
	free(data);
	return(1);
    }
    if(resp->code == 200)
    {
	while((ires = dc_interpret(resp)) != NULL)
	{
	    adddatum(fn, ires->argv[0].val.str, ires->argv[1].val.num);
	    dc_freeires(ires);
	}
	dc_queuecmd(getpeerlistcallback, data, L"lspeers", L"%%i", fn->id, NULL);
    } else if(resp->code == 201) {
	dc_queuecmd(getpeerlistcallback, data, L"lspeers", L"%%i", fn->id, NULL);
    } else {
	data->callback(fn, resp->code, data->data);
	free(data);
    }
    return(1);
}

void dc_getfnlistasync(void (*callback)(int, void *), void *udata)
{
    struct gencbdata *data;
    
    data = smalloc(sizeof(*data));
    data->callback = callback;
    data->data = udata;
    dc_queuecmd(getfnlistcallback, data, L"lsnodes", NULL);
}

void dc_gettrlistasync(void (*callback)(int, void *), void *udata)
{
    struct gencbdata *data;
    
    data = smalloc(sizeof(*data));
    data->callback = callback;
    data->data = udata;
    dc_queuecmd(gettrlistcallback, data, L"lstrans", NULL);
}

void dc_getpeerlistasync(struct dc_fnetnode *fn, void (*callback)(struct dc_fnetnode *, int, void *), void *udata)
{
    struct fnetcbdata *data;
    
    data = smalloc(sizeof(*data));
    data->callback = callback;
    data->fnid = fn->id;
    data->data = udata;
    dc_queuecmd(getpalistcallback, data, L"lspa", L"%%i", fn->id, NULL);
}

void dc_uimisc_disconnected(void)
{
    while(dc_fnetnodes != NULL)
	freefn(dc_fnetnodes);
    while(dc_transfers != NULL)
	freetransfer(dc_transfers);
}

void dc_uimisc_handlenotify(struct dc_response *resp)
{
    int i;
    struct dc_fnetnode *fn;
    struct dc_transfer *transfer;
    struct dc_fnetpeer *peer;
    struct dc_intresp *ires;
    
    if((ires = dc_interpret(resp)) == NULL)
	return;
    switch(resp->code)
    {
    case 601:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	    fn->state = ires->argv[1].val.num;
	break;
    case 602:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	{
	    if(fn->name != NULL)
		free(fn->name);
	    fn->name = swcsdup(ires->argv[1].val.str);
	}
	break;
    case 603:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	    freefn(fn);
	break;
    case 604:
	fn = newfn();
	fn->id = ires->argv[0].val.num;
	if(fn->fnet != NULL)
	    free(fn->fnet);
	fn->fnet = swcsdup(ires->argv[1].val.str);
	fn->state = DC_FNN_STATE_SYN;
	fn->numusers = 0;
	break;
    case 605:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	    fn->numusers = ires->argv[1].val.num;
	break;
    case 610:
	transfer = newtransfer();
	transfer->id = ires->argv[0].val.num;
	transfer->dir = ires->argv[1].val.num;
	if(transfer->dir == DC_TRNSD_UP)
	    transfer->state = DC_TRNS_HS;
	transfer->peerid = swcsdup(ires->argv[2].val.str);
	if(ires->argv[3].val.str[0])
	    transfer->path = swcsdup(ires->argv[3].val.str);
	break;
    case 611:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	    transfer->state = ires->argv[1].val.num;
	break;
    case 612:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	{
	    if(transfer->peernick != NULL)
		free(transfer->peernick);
	    transfer->peernick = swcsdup(ires->argv[1].val.str);
	}
	break;
    case 613:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	    transfer->size = ires->argv[1].val.num;
	break;
    case 614:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	{
	    transfer->error = ires->argv[1].val.num;
	    time(&transfer->errortime);
	}
	break;
    case 615:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	    transfer->curpos = ires->argv[1].val.num;
	break;
    case 616:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	{
	    if(transfer->path != NULL)
		free(transfer->path);
	    transfer->path = swcsdup(ires->argv[1].val.str);
	}
	break;
    case 617:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	    freetransfer(transfer);
	break;
    case 618:
	if((transfer = dc_findtransfer(ires->argv[0].val.num)) != NULL)
	{
	    if(transfer->hash != NULL)
	    {
		free(transfer->hash);
		transfer->hash = NULL;
	    }
	    if(wcslen(ires->argv[1].val.str) > 0)
		transfer->hash = swcsdup(ires->argv[1].val.str);
	}
	break;
    case 630:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	{
	    if((peer = dc_fnetfindpeer(fn, ires->argv[1].val.str)) == NULL)
	    {
		peer = addpeer(fn, ires->argv[1].val.str, ires->argv[2].val.str);
		if(fn->newpeercb != NULL)
		    fn->newpeercb(peer);
	    }
	}
	break;
    case 631:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	{
	    if((peer = dc_fnetfindpeer(fn, ires->argv[1].val.str)) != NULL)
	    {
		if(fn->delpeercb != NULL)
		    fn->delpeercb(peer);
		delpeer(peer);
	    }
	}
	break;
    case 632:
	if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
	{
	    if((peer = dc_fnetfindpeer(fn, ires->argv[1].val.str)) != NULL)
	    {
		if(wcscmp(ires->argv[2].val.str, peer->nick))
		{
		    free(peer->nick);
		    peer->nick = swcsdup(ires->argv[2].val.str);
		}
		for(i = 4; i < resp->rlines[0].argc; i += 3)
		{
		    switch(wcstol(resp->rlines[0].argv[i + 1], NULL, 10))
		    {
		    case DC_FNPD_INT:
			peersetnum(peer, resp->rlines[0].argv[i], wcstol(resp->rlines[0].argv[i + 2], NULL, 10));
			break;
		    case DC_FNPD_LL:
			peersetlnum(peer, resp->rlines[0].argv[i], wcstoll(resp->rlines[0].argv[i + 2], NULL, 10));
			break;
		    case DC_FNPD_STR:
			peersetstr(peer, resp->rlines[0].argv[i], resp->rlines[0].argv[i + 2]);
			break;
		    }
		}
		if(fn->chpeercb != NULL)
		    fn->chpeercb(peer);
	    }
	}
	break;
    default:
	break;
    }
    dc_freeires(ires);
    resp->curline = 0;
}

/* Note the backspace handling - it's not as elegant as possible, but
 * it helps avoid the "box-of-toothpicks" syndrome when writing search
 * expressions manually. */
wchar_t **dc_lexsexpr(wchar_t *sexpr)
{
    wchar_t **ret;
    wchar_t *buf;
    size_t retsize, retdata, bufsize, bufdata;
    int state;
    
    ret = NULL;
    buf = NULL;
    retsize = retdata = bufsize = bufdata = 0;
    state = 0;
    while(*sexpr != L'\0')
    {
	switch(state)
	{
	case 0:
	    if(!iswspace(*sexpr))
		state = 1;
	    else
		sexpr++;
	    break;
	case 1:
	    if(iswspace(*sexpr))
	    {
		if(buf != NULL)
		{
		    addtobuf(buf, L'\0');
		    addtobuf(ret, buf);
		    buf = NULL;
		    bufsize = bufdata = 0;
		}
		state = 0;
	    } else if((*sexpr == L'(') ||
		      (*sexpr == L')') ||
		      (*sexpr == L'&') ||
		      (*sexpr == L'|') ||
		      (*sexpr == L'!')) {
		if(buf != NULL)
		{
		    addtobuf(buf, L'\0');
		    addtobuf(ret, buf);
		    buf = NULL;
		    bufsize = bufdata = 0;
		}
		addtobuf(buf, *sexpr);
		addtobuf(buf, L'\0');
		addtobuf(ret, buf);
		buf = NULL;
		bufsize = bufdata = 0;
		sexpr++;
	    } else if(*sexpr == L'\"') {
		sexpr++;
		state = 2;
	    } else if(*sexpr == L'\\') {
		sexpr++;
		if(*sexpr == L'\0')
		{
		    addtobuf(buf, *sexpr);
		} else if((*sexpr == L'\\') || (*sexpr == L'\"')) {
		    addtobuf(buf, *sexpr);
		    sexpr++;
		} else {
		    addtobuf(buf, L'\\');
		    addtobuf(buf, *sexpr);
		    sexpr++;
		}
	    } else {
		addtobuf(buf, *(sexpr++));
	    }
	    break;
	case 2:
	    if(*sexpr == L'\\')
	    {
		sexpr++;
		if(*sexpr == L'\0')
		{
		    addtobuf(buf, *sexpr);
		} else if((*sexpr == L'\\') || (*sexpr == L'\"')) {
		    addtobuf(buf, *sexpr);
		    sexpr++;
		} else {
		    addtobuf(buf, L'\\');
		    addtobuf(buf, *sexpr);
		    sexpr++;
		}
	    } else if(*sexpr == L'\"') {
		state = 1;
		sexpr++;
	    } else {
		addtobuf(buf, *(sexpr++));
	    }
	    break;
	}
    }
    if(buf != NULL)
    {
	addtobuf(buf, L'\0');
	addtobuf(ret, buf);
    }
    addtobuf(ret, NULL);
    return(ret);
}

void dc_freewcsarr(wchar_t **arr)
{
    wchar_t **buf;
    
    if(arr == NULL)
	return;
    for(buf = arr; *buf != NULL; buf++)
	free(*buf);
    free(arr);
}
