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
#define _GNU_SOURCE
#include <unistd.h>
#include <stdlib.h>
#include <malloc.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip6.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <wchar.h>
#include <wctype.h>
#include <iconv.h>
#include <pwd.h>
#include <time.h>
#include <fcntl.h>
#include <signal.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "conf.h"
#include "auth.h"
#include "utils.h"
#include "net.h"
#include "module.h"
#include "sysevents.h"
#include "filenet.h"
#include "transfer.h"
#include "search.h"
#include "client.h"

#define PERM_DISALLOW 1
#define PERM_ADMIN 2
#define PERM_FNETCTL 4
#define PERM_TRANS 8
#define PERM_TRANSCU 16
#define PERM_CHAT 32
#define PERM_SRCH 64

#define NOTIF_END 0
#define NOTIF_INT 1
#define NOTIF_STR 2
#define NOTIF_FLOAT 3
#define NOTIF_ID 4
#define NOTIF_PEND 0
#define NOTIF_WAIT 1

struct uidata;

struct command
{
    wchar_t *name;
    void (*handler)(struct socket *sk, struct uidata *data, int argc, wchar_t **argv);
};

struct qcommand
{
    struct qcommand *next;
    struct command *cmd;
    int argc;
    wchar_t **argv;
};

struct uiuser
{
    struct uiuser *next, *prev;
    int used;
    wchar_t *name;
    unsigned long perms;
    int delete;
};

struct notif
{
    struct notif *next, *prev;
    struct uidata *ui;
    int state;
    int code;
    double rlimit;
    size_t argc;
    struct timer *exptimer;
    struct notifarg
    {
	int dt;
	union
	{
	    int n;
	    wchar_t *s;
	    double d;
	} d;
    } *argv;
};

struct uidata
{
    struct uidata *next, *prev;
    struct socket *sk;
    struct qcommand *queue, *queuelast;
    struct authhandle *auth;
    int close;
    union
    {
	struct
	{
	    int fnact:1;
	    int fnchat:1;
	    int fnpeer:1;
	    int tract:1;
	    int trprog:1;
	    int srch:1;
	} b;
	int w;
    } notify;
    wchar_t *username;
    struct uiuser *userinfo;
    uid_t uid;
    struct notif *fnotif, *lnotif;
    char *fcmdbuf;
    size_t fcmdbufdata, fcmdbufsize;
    pid_t fcmdpid;
    struct socket *fcmdsk;
    /* Read buffer */
    char *inbuf;
    size_t inbufsize, indata;
    /* Wordset storage */
    wchar_t **argv;
    int argc, args;
    /* WCS conversation stuff */
    wchar_t *cb; /* Conversation buffer */
    size_t cbsize, cbdata;
    iconv_t ichandle;
    /* Parser data */
    int ps; /* Parser state */
    wchar_t *pp; /* Current parse pointer */
    wchar_t *cw; /* Current word (building storage) */
    size_t cwsize, cwdata;
};

static int srcheta(struct search *srch, void *uudata);
static int srchcommit(struct search *srch, void *uudata);
static int srchres(struct search *srch, struct srchres *sr, void *uudata);

struct uiuser *users = NULL;
struct uidata *actives = NULL;
struct socket *uisocket = NULL;

static wchar_t *quoteword(wchar_t *word)
{
    wchar_t *wp, *buf, *bp;
    int dq, numbs, numc;
    
    dq = 0;
    numbs = 0;
    numc = 0;
    if(*word == L'\0')
    {
	dq = 1;
    } else {
	for(wp = word; *wp != L'\0'; wp++)
	{
	    if(!dq && iswspace(*wp))
		dq = 1;
	    if((*wp == L'\\') || (*wp == L'\"'))
		numbs++;
	    numc++;
	}
    }
    if(!dq && !numbs)
	return(NULL);
    bp = buf = smalloc(sizeof(wchar_t) * (numc + numbs + (dq?2:0) + 1));
    if(dq)
	*(bp++) = L'\"';
    for(wp = word; *wp != L'\0'; wp++)
    {
	if((*wp == L'\\') || (*wp == L'\"'))
	    *(bp++) = L'\\';
	*(bp++) = *wp;
    }
    if(dq)
	*(bp++) = L'\"';
    *(bp++) = L'\0';
    return(buf);
}

static void sq(struct socket *sk, int cont, ...)
{
    int num, freepart;
    va_list al;
    char *final;
    wchar_t *buf;
    wchar_t *part, *tpart;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    num = 0;
    va_start(al, cont);
    while((part = va_arg(al, wchar_t *)) != NULL)
    {
	if(*part == L'%')
	{
	    /*
	     * This kludge demands that all arguments that you call it
	     * with are the size of an int. That happens to be the
	     * case for most datatypes on most platforms and
	     * compilers, but I don't know exactly which ones, and
	     * also a long long is a notable candidate of an arg that
	     * is not the size of an int on 32-bit archs. If it breaks
	     * some existing call on your architecture, please tell
	     * me.
	     */
	    part = vswprintf2(tpart = (part + 1), al);
	    for(; *tpart != L'\0'; tpart++)
	    {
		if(*tpart == L'%')
		{
		    if(tpart[1] == L'%')
			tpart++;
		    else
			va_arg(al, int);
		}
	    }
	    freepart = 1;
	} else {
	    freepart = 0;
	}
	if((tpart = quoteword(part)) != NULL)
	{
	    if(freepart)
		free(part);
	    part = tpart;
	    freepart = 1;
	}
	if((num > 1) || ((num == 1) && !(cont & 1)))
	    addtobuf(buf, L' ');
	bufcat(buf, part, wcslen(part));
	if((num == 0) && (cont & 1))
	    addtobuf(buf, L'-');
	num++;
	if(freepart)
	    free(part);
    }
    if(cont & 2)
	bufcat(buf, L" \0", 2);
    else
	bufcat(buf, L"\r\n\0", 3);
    if((final = icwcstombs(buf, "utf-8")) == NULL)
    {
	flog(LOG_CRIT, "could not convert \"%ls\" into utf-8: %s", buf, strerror(errno));
	free(buf);
	return;
    }
    va_end(al);
    free(buf);
    sockqueue(sk, final, strlen(final));
    free(final);
}

struct uiuser *finduser(wchar_t *name)
{
    struct uiuser *user;
    
    for(user = users; user != NULL; user = user->next)
    {
	if(!wcscmp(user->name, name))
	    break;
    }
    return(user);
}

static void logout(struct uidata *data)
{
    data->userinfo = NULL;
    if(data->username != NULL)
	free(data->username);
    data->username = NULL;
    if(data->auth != NULL)
	authputhandle(data->auth);
    data->auth = NULL;
}

static int haspriv(struct uidata *data, int perm)
{
    if(data->userinfo == NULL)
	return(0);
    if(data->userinfo->perms & perm)
	return(1);
    else
	return(0);
}

/* Useful macros for the command functions: */
#define haveargs(n) do { if(argc < n) { sq(sk, 0, L"501", L"Wrong number of arguments", NULL); return; } } while(0)
#define havepriv(p) do { if((data->userinfo == NULL) || ((data->userinfo->perms & (p)) != (p))) { sq(sk, 0, L"502", L"%Unauthorized request - %x needed", (p), NULL); return; } } while(0)

static void cmd_connect(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int valid;
    
    if(confgetint("ui", "onlylocal"))
    {
	switch(sk->remote->sa_family)
	{
	case AF_INET:
	    valid = ((struct sockaddr_in *)sk->remote)->sin_addr.s_addr == INADDR_LOOPBACK;
	    break;
	case AF_INET6:
	    valid = !memcmp(&((struct sockaddr_in6 *)sk->remote)->sin6_addr, &in6addr_loopback, sizeof(in6addr_loopback));
	    break;
	default:
	    valid = 0;
	    break;
	}
	if(!valid)
	{
	    sq(sk, 0, L"502", L"Only localhost connections allowed to this host", NULL);
	    sk->close = 1;
	    data->close = 1;
	    return;
	}
    }
    sq(sk, 0, L"200", L"%Dolda Connect daemon v%s", VERSION, NULL);
}

static void cmd_notfound(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    if((argv != NULL) && (argv[0] != NULL))
	sq(sk, 0, L"500", L"%Command not found: %ls", argv[0], NULL);
    else
	sq(sk, 0, L"500", L"No command", NULL);
}

static void cmd_shutdown(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    extern volatile int running;
    
    havepriv(PERM_ADMIN);
    flog(LOG_NOTICE, "UI shutdown request from %ls, shutting down", data->username);
    running = 0;
    sq(sk, 0, L"200", L"Daemon shutting down", NULL);
}

static void cmd_quit(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    sq(sk, 0, L"200", L"Closing connection", NULL);
    data->close = 1;
}

static void cmd_lsauth(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct authmech *mech, *prev;
    
    prev = NULL;
    for(mech = mechs; mech != NULL; mech = mech->next)
    {
	if(mech->enabled)
	{
	    if(prev != NULL)
		sq(sk, 1, L"200", prev->name, NULL);
	    prev = mech;
	}
    }
    if(prev == NULL)
	sq(sk, 0, L"201", L"No authentication methods supported", NULL);
    else
	sq(sk, 0, L"200", prev->name, NULL);
}

static void cmd_login(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    char *buf;
    int code;
    struct passwd *pwd;
    
    haveargs(3);
    if(data->username != NULL)
    {
	if(data->userinfo != NULL)
	    sq(sk, 0, L"503", L"Already logged in", NULL);
	else
	    sq(sk, 0, L"503", L"Already logging in", NULL);
	return;
    }
    if((buf = icwcstombs(argv[2], NULL)) == NULL)
    {
	sq(sk, 0, L"504", L"Could not convert username to locale charset", NULL);
	return;
    }
    data->username = swcsdup(argv[2]);
    if((pwd = getpwnam(buf)) == NULL)
	data->uid = -1;
    else
	data->uid = pwd->pw_uid;
    if((data->auth = initauth(argv[1], buf)) == NULL)
    {
	if(errno == ENOENT)
	    sq(sk, 0, L"508", L"No such authentication mechanism", NULL);
	else
	    sq(sk, 0, L"505", L"Could not initialize authentication system", L"%%s", strerror(errno), NULL);
	free(buf);
	logout(data);
	return;
    }
    free(buf);
    switch(authenticate(data->auth, NULL))
    {
    case AUTH_SUCCESS:
	data->userinfo = finduser(data->username);
	if(data->userinfo == NULL)
	    data->userinfo = finduser(L"default");
	if(data->uid == -1)
	{
	    sq(sk, 0, L"506", L"Authentication error", NULL);
	    flog(LOG_INFO, "user %ls authenticated successfully, but no account existed", data->username);
	    logout(data);
	} else if((data->userinfo == NULL) || (data->userinfo->perms & PERM_DISALLOW)) {
	    sq(sk, 0, L"506", L"Authentication error", NULL);
	    flog(LOG_INFO, "user %ls authenticated successfully, but was not authorized", data->username);
	    logout(data);
	} else {
	    sq(sk, 0, L"200", L"Welcome", NULL);
	    flog(LOG_INFO, "%ls (UID %i) logged in", data->username, data->uid);
	}
	break;
    case AUTH_DENIED:
	sq(sk, 0, L"506", L"Authentication error", L"%%ls", (data->auth->text == NULL)?L"":(data->auth->text), NULL);
	logout(data);
	break;
    case AUTH_PASS:
	switch(data->auth->prompt)
	{
	case AUTH_PR_AUTO:
	    code = 300;
	    break;
	case AUTH_PR_NOECHO:
	    code = 301;
	    break;
	case AUTH_PR_ECHO:
	    code = 302;
	    break;
	case AUTH_PR_INFO:
	    code = 303;
	    break;
	case AUTH_PR_ERROR:
	    code = 304;
	    break;
	}
	sq(sk, 0, L"%%i", code, data->auth->text, NULL);
	break;
    case AUTH_ERR:
	sq(sk, 0, L"505", L"System error", L"%%s", strerror(errno), NULL);
	logout(data);
	break;
    default:
	flog(LOG_WARNING, "BUG? Non-caught return from authenticate in cmd_login");
	sq(sk, 0, L"505", L"System error", L"%%s", strerror(errno), NULL);
	logout(data);
	break;
    }
}

static void cmd_pass(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    char *buf;
    int code;

    haveargs(2);
    if((buf = icwcstombs(argv[1], NULL)) == NULL)
    {
	sq(sk, 0, L"504", L"Could not convert data to locale charset", NULL);
	return;
    }
    if((data->auth == NULL) || (data->userinfo != NULL))
    {
	sq(sk, 0, L"507", L"Data not expected", NULL);
	return;
    }
    switch(authenticate(data->auth, buf))
    {
    case AUTH_SUCCESS:
	data->userinfo = finduser(data->username);
	if(data->userinfo == NULL)
	    data->userinfo = finduser(L"default");
	if(data->uid == -1)
	{
	    sq(sk, 0, L"506", L"Authentication error", NULL);
	    flog(LOG_INFO, "user %ls authenticated successfully, but no account existed", data->username);
	    logout(data);
	} else if((data->userinfo == NULL) || (data->userinfo->perms & PERM_DISALLOW)) {
	    sq(sk, 0, L"506", L"Authentication error", NULL);
	    flog(LOG_INFO, "user %ls authenticated successfully, but was not authorized", data->username);
	    logout(data);
	} else {
	    sq(sk, 0, L"200", L"Welcome", NULL);
	    flog(LOG_INFO, "%ls (UID %i) logged in", data->username, data->uid);
	}
	break;
    case AUTH_DENIED:
	sq(sk, 0, L"506", L"Authentication error", L"%%ls", (data->auth->text == NULL)?L"":(data->auth->text), NULL);
	logout(data);
	break;
    case AUTH_PASS:
	switch(data->auth->prompt)
	{
	case AUTH_PR_AUTO:
	    code = 300;
	    break;
	case AUTH_PR_NOECHO:
	    code = 301;
	    break;
	case AUTH_PR_ECHO:
	    code = 302;
	    break;
	case AUTH_PR_INFO:
	    code = 303;
	    break;
	case AUTH_PR_ERROR:
	    code = 304;
	    break;
	}
	sq(sk, 0, L"%%i", code, data->auth->text, NULL);
	break;
    case AUTH_ERR:
	sq(sk, 0, L"505", L"System error", L"%%s", strerror(errno), NULL);
	logout(data);
	break;
    default:
	flog(LOG_WARNING, "BUG? Non-caught return from authenticate in cmd_pass");
	sq(sk, 0, L"505", L"System error", L"%%s", strerror(errno), NULL);
	logout(data);
	break;
    }
    free(buf);
}

static void cmd_fnetconnect(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int i;
    char *buf;
    int err;
    struct fnetnode *fn;
    
    haveargs(3);
    havepriv(PERM_FNETCTL);
    if((buf = icwcstombs(argv[2], NULL)) == NULL)
    {
	sq(sk, 0, L"504", L"Could not convert data to locale charset", NULL);
	return;
    }
    fn = fnetinitconnect(argv[1], buf);
    err = errno;
    free(buf);
    if(fn == NULL)
    {
	if(errno == EPROTONOSUPPORT)
	    sq(sk, 0, L"511", L"No such network name", NULL);
	else
	    sq(sk, 0, L"509", L"Could not parse the address", L"%%s", strerror(err), NULL);
	return;
    }
    for(i = 3; i < argc - 1; i += 2)
    {
	if(!wcscmp(argv[i], L"nick"))
	    fnetsetnick(fn, argv[i + 1]);
    }
    linkfnetnode(fn);
    fnetsetname(fn, argv[2]);
    putfnetnode(fn);
    sq(sk, 0, L"200", L"Connection under way", NULL);
}

static void cmd_lsnodes(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct fnetnode *fn;

    if(fnetnodes == NULL)
    {
	sq(sk, 0, L"201", L"No connected nodes", NULL);
	return;
    }
    for(fn = fnetnodes; fn != NULL; fn = fn->next)
    {
	sq(sk, (fn->next != NULL)?1:0, L"200", L"%%i", fn->id, fn->fnet->name, (fn->name == NULL)?L"":fn->name, L"%%i", fn->numpeers, L"%%i", fn->state, NULL);
    }
}

static void cmd_disconnect(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct fnetnode *fn;
    int i;
    
    haveargs(2);
    havepriv(PERM_FNETCTL);
    /* Note - Programmatical user interfaces must only give one
     * argument per command, the multiple argument form is only for
     * convenience when manually controlling the daemon via
     * eg. telnet. The reason is that the return codes aren't clear
     * enough for the multiple argument form. */
    for(i = 1; i < argc; i++)
    {
	if((fn = findfnetnode(wcstol(argv[i], NULL, 0))) == NULL)
	{
	    sq(sk, 0, L"510", L"No such node", NULL);
	    return;
	}
	killfnetnode(fn);
	unlinkfnetnode(fn);
    }
    sq(sk, 0, L"200", L"Node flagged for disconnection", NULL);
}

static void cmd_lspa(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct fnetnode *fn;
    struct fnetpeerdatum *datum;
    
    haveargs(2);
    if((fn = findfnetnode(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"510", L"No such node", NULL);
	return;
    }
    if(fn->peerdata == NULL)
    {
	sq(sk, 0, L"201", L"No data available", NULL);
    } else {
	for(datum = fn->peerdata; datum != NULL; datum = datum->next)
	    sq(sk, (datum->next != NULL)?1:0, L"200", datum->id, L"%%i", datum->datatype, NULL);
    }
}

static void cmd_lspeers(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int i;
    struct fnetnode *fn;
    struct fnetpeer *peer;
    wchar_t buf[40];
    
    haveargs(2);
    if((fn = findfnetnode(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"510", L"No such node", NULL);
	return;
    }
    if(fn->peers == NULL)
    {
	sq(sk, 0, L"201", L"No peers avaiable", NULL);
    } else {
	for(peer = fn->peers; peer != NULL; peer = peer->next)
	{
	    sq(sk, 2 | ((peer->next != NULL)?1:0), L"200", peer->id, peer->nick, NULL);
	    for(i = 0; i < peer->dinum; i++)
	    {
		if(peer->peerdi[i].datum->datatype == FNPD_INT)
		    sq(sk, 2, peer->peerdi[i].datum->id, L"%%i", peer->peerdi[i].data.num, NULL);
		/* Note: A long long is not the size of an int, so
		 * sq() can't handle the conversion itself. */
		if(peer->peerdi[i].datum->datatype == FNPD_LL)
		{
		    swprintf(buf, 40, L"%lli", peer->peerdi[i].data.lnum);
		    sq(sk, 2, peer->peerdi[i].datum->id, buf, NULL);
		}
		if((peer->peerdi[i].datum->datatype == FNPD_STR) && (peer->peerdi[i].data.str != NULL))
		    sq(sk, 2, peer->peerdi[i].datum->id, peer->peerdi[i].data.str, NULL);
	    }
	    sq(sk, 0, NULL);
	}
    }
}

static void cmd_download(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int i;
    struct fnet *net;
    struct fnetnode *fn;
    struct transfer *transfer;
    struct fnetpeer *peer;
    
    haveargs(4);
    if((argc > 5) && ((argc % 2) == 0))
    {
	sq(sk, 0, L"501", L"Must have an even number of arguments", NULL);
	return;
    }
    havepriv(PERM_TRANS);
    if((*(argv[1]) >= L'0') && (*(argv[1]) <= L'9'))
    {
	if((fn = findfnetnode(wcstol(argv[1], NULL, 0))) == NULL)
	{
	    sq(sk, 0, L"510", L"No such node", NULL);
	    return;
	}
	net = fn->fnet;
    } else {
	fn = NULL;
	if((net = findfnet(argv[1])) == NULL)
	{
	    sq(sk, 0, L"511", L"No such network name", NULL);
	    return;
	}
    }
    transfer = newtransfer();
    authgethandle(transfer->auth = data->auth);
    transfer->fnet = net;
    transfer->peerid = swcsdup(argv[2]);
    transfer->path = swcsdup(argv[3]);
    transfer->dir = TRNSD_DOWN;
    transfer->owner = data->uid;
    if(fn != NULL)
    {
	transfer->fn = fn;
	getfnetnode(fn);
	linktransfer(transfer);
	if(((peer = fnetfindpeer(fn, transfer->peerid)) != NULL) && (peer->nick != NULL))
	    transfersetnick(transfer, peer->nick);
    } else {
	linktransfer(transfer);
    }
    if(argc > 4)
	transfersetsize(transfer, wcstol(argv[4], NULL, 0));
    if(argc > 5)
    {
	for(i = 5; i < argc; i += 2)
	{
	    if(!wcscmp(argv[i], L"hash"))
	    {
		transfersethash(transfer, parsehash(argv[i + 1]));
	    } else {
		transferaddarg(transfer, argv[i], argv[i + 1]);
	    }
	}
    }
    sq(sk, 0, L"200", L"%%i", transfer->id, L"Download queued", NULL);
    transfersetactivity(transfer, L"create");
}

static void cmd_lstrans(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct transfer *transfer, *pt;
    
    havepriv(PERM_TRANS);
    pt = NULL;
    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if((transfer->dir != TRNSD_DOWN) || (transfer->owner == data->uid))
	{
	    if(pt != NULL)
		sq(sk, 1, L"200", L"%%i", pt->id, L"%%i", pt->dir,
		   L"%%i", pt->state, pt->peerid,
		   (pt->peernick == NULL)?L"":(pt->peernick),
		   (pt->path == NULL)?L"":(pt->path),
		   L"%%i", pt->size, L"%%i", pt->curpos,
		   (pt->hash == NULL)?L"":unparsehash(pt->hash),
		   NULL);
	    pt = transfer;
	}
    }
    if(pt == NULL)
	sq(sk, 0, L"201", L"No transfers", NULL);
    else
	sq(sk, 0, L"200", L"%%i", pt->id, L"%%i", pt->dir,
	   L"%%i", pt->state, pt->peerid,
	   (pt->peernick == NULL)?L"":(pt->peernick),
	   (pt->path == NULL)?L"":(pt->path),
	   L"%%i", pt->size, L"%%i", pt->curpos,
	   (pt->hash == NULL)?L"":unparsehash(pt->hash),
	   NULL);
}

static void cmd_cancel(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct transfer *transfer;
    
    haveargs(2);
    havepriv(PERM_TRANS);
    if((transfer = findtransfer(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"512", L"No such transfer", NULL);
	return;
    }
    if((transfer->dir == TRNSD_UP) && !(data->userinfo->perms & PERM_TRANSCU))
    {
	sq(sk, 0, L"502", L"You are not allowed to cancel uploads", NULL);
	return;
    }
    if((transfer->dir == TRNSD_DOWN) && (transfer->owner != data->uid))
    {
	sq(sk, 0, L"502", L"You do not own that transfer", NULL);
	return;
    }
    transfer->close = 1;
    sq(sk, 0, L"200", L"Transfer cancelled", NULL);
}

static void cmd_notify(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int i, val;
    
    if((argc % 2) != 1)
    {
	sq(sk, 0, L"501", L"Must have an even number of arguments", NULL);
	return;
    }
    for(i = 1; i < argc; i += 2)
    {
	if(!wcscasecmp(argv[i + 1], L"on"))
	    val = 1;
	else
	    val = 0;
	if(!wcscasecmp(argv[i], L"all"))
	{
	    if(val)
		data->notify.w = ~0;
	    else
		data->notify.w = 0;
	} else if(!wcscasecmp(argv[i], L"fn:chat")) {
	    data->notify.b.fnchat = val;
	} else if(!wcscasecmp(argv[i], L"fn:act")) {
	    data->notify.b.fnact = val;
	} else if(!wcscasecmp(argv[i], L"fn:peer")) {
	    data->notify.b.fnpeer = val;
	} else if(!wcscasecmp(argv[i], L"trans:act")) {
	    data->notify.b.tract = val;
	} else if(!wcscasecmp(argv[i], L"trans:prog")) {
	    data->notify.b.trprog = val;
	} else if(!wcscasecmp(argv[i], L"srch:act")) {
	    data->notify.b.srch = val;
	}
    }
    sq(sk, 0, L"200", L"Notification alteration succeeded", NULL);
}

static void cmd_sendchat(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct fnetnode *fn;
    int public;
    
    haveargs(5);
    havepriv(PERM_CHAT);
    if((fn = findfnetnode(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"510", L"No such node", NULL);
	return;
    }
    public = wcstol(argv[2], NULL, 0);
    if((public != 0) && (public != 1))
    {
	sq(sk, 0, L"509", L"Second argument must be 0 or 1", NULL);
	return;
    }
    if(fn->state != FNN_EST)
    {
	sq(sk, 0, L"513", L"Hub is in state FNN_EST", NULL);
	return;
    }
    if(fnetsendchat(fn, public, argv[3], argv[4]))
    {
	if(errno == ENOTSUP)
	    sq(sk, 0, L"513", L"This network does not support chatting", NULL);
	else if(errno == EPERM)
	    sq(sk, 0, L"502", L"This node does not allow you to chat", NULL);
	else if(errno == EILSEQ)
	    sq(sk, 0, L"504", L"This network could not support all the characters in that message", NULL);
	else
	    sq(sk, 0, L"505", L"Could not chat", NULL);
	return;
    }
    sq(sk, 0, L"200", L"Chat string sent", NULL);
}

static void cmd_search(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct search *srch;
    struct fnetnode *fn;
    struct sexpr *sexpr;
    int i;
    
    haveargs(3);
    havepriv(PERM_SRCH);
    srch = newsearch(data->username, NULL);
    for(i = 1; i < argc; i++)
    {
	if(!wcscmp(argv[i], L"all"))
	{
	    for(fn = fnetnodes; fn != NULL; fn = fn->next)
	    {
		if(fn->state == FNN_EST)
		    searchaddfn(srch, fn);
	    }
	    i++;
	    break;
	} else if(!wcscmp(argv[i], L"prio")) {
	    if(++i == argc)
	    {
		sq(sk, 0, L"501", L"No argument to prio", NULL);
		freesearch(srch);
		return;
	    }
	    srch->prio = wcstol(argv[i], NULL, 0);
	} else if(iswdigit(*argv[i])) {
	    if((fn = findfnetnode(wcstol(argv[i], NULL, 0))) == NULL)
	    {
		sq(sk, 0, L"510", L"No such node", NULL);
		freesearch(srch);
		return;
	    }
	    searchaddfn(srch, fn);
	} else {
	    break;
	}
    }
    if(srch->fnl == NULL)
    {
	sq(sk, 0, L"501", L"No fnetnodes to search found on line", NULL);
	freesearch(srch);
	return;
    }
    if(i == argc)
    {
	sq(sk, 0, L"501", L"No search expression found on line", NULL);
	freesearch(srch);
	return;
    }
    if((sexpr = parsesexpr(argc - i, argv + i)) == NULL)
    {
	sq(sk, 0, L"509", L"Could not parse search expression", NULL);
	freesearch(srch);
	return;
    }
    optsexpr(sexpr);
    getsexpr(srch->sexpr = sexpr);
    queuesearch(srch);
    CBREG(srch, search_eta, srcheta, NULL, NULL);
    CBREG(srch, search_commit, srchcommit, NULL, NULL);
    CBREG(srch, search_result, srchres, NULL, NULL);
    sq(sk, 0, L"200", L"%%i", srch->id, L"%%i", srch->eta - time(NULL), NULL);
    putsexpr(sexpr);
}

static void cmd_lssrch(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct search *srch, *pt;
    time_t now;
    
    havepriv(PERM_SRCH);
    pt = NULL;
    now = time(NULL);
    for(srch = searches; srch != NULL; srch = srch->next)
    {
	if(!wcscmp(srch->owner, data->username))
	{
	    if(pt != NULL)
		sq(sk, 1, L"200", L"%%i", pt->id, L"%%i", pt->state, L"%%i", pt->eta - now, L"%%i", pt->numres, NULL);
	    pt = srch;
	}
    }
    if(pt == NULL)
	sq(sk, 0, L"201", L"No searches", NULL);
    else
	sq(sk, 0, L"200", L"%%i", pt->id, L"%%i", pt->state, L"%%i", pt->eta - now, L"%%i", pt->numres, NULL);
}

static void cmd_lssr(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct search *srch;
    struct srchres *sr;
    wchar_t buf[64];
    
    haveargs(2);
    havepriv(PERM_SRCH);
    if((srch = findsearch(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"514", L"No such search", NULL);
	return;
    }
    if(srch->results == NULL)
    {
	sq(sk, 0, L"201", L"No results", NULL);
    } else {
	for(sr = srch->results; sr != NULL; sr = sr->next)
	{
	    swprintf(buf, 64, L"%f", sr->time);
	    sq(sk, (sr->next != NULL)?1:0, L"200", L"%%ls", sr->filename, sr->fnet->name, L"%%ls", sr->peerid, L"%%i", sr->size, L"%%i", sr->slots, L"%%i", (sr->fn == NULL)?-1:(sr->fn->id), buf, L"%%ls", (sr->hash == NULL)?L"":unparsehash(sr->hash), NULL);
	}
    }
}

static void cmd_cansrch(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct search *srch;
    int i;
    
    haveargs(2);
    havepriv(PERM_SRCH);
    /* Note - Programmatical user interfaces must only give one
     * argument per command, the multiple argument form is only for
     * convenience when manually controlling the daemon via
     * eg. telnet. The reason is that the return codes aren't clear
     * enough for the multiple argument form. */
    for(i = 1; i < argc; i++)
    {
	if((srch = findsearch(wcstol(argv[i], NULL, 0))) == NULL)
	{
	    sq(sk, 0, L"514", L"No such search", NULL);
	    return;
	}
	freesearch(srch);
    }
    sq(sk, 0, L"200", L"Search cancelled", NULL);
}

static void fcmdread(struct socket *sk, struct uidata *data)
{
    char *buf;
    size_t bufsize;
    
    if((buf = sockgetinbuf(sk, &bufsize)) == NULL)
	return;
    bufcat(data->fcmdbuf, buf, bufsize);
    free(buf);
}

static void fcmderr(struct socket *sk, int err, struct uidata *data)
{
    wchar_t *wbuf, *p, *p2;
    
    if(err)
    {
	flog(LOG_WARNING, "error occurred on filtercmd pipe socket: %s", strerror(err));
	kill(-data->fcmdpid, SIGHUP);
	putsock(data->fcmdsk);
	data->fcmdsk = NULL;
	if(data->fcmdbuf != NULL)
	{
	    free(data->fcmdbuf);
	    data->fcmdbuf = NULL;
	}
	data->fcmdbufsize = data->fcmdbufdata = 0;
	sq(data->sk, 0, L"505", L"An error occurred on the pipe to the filtercmd", L"%%s", strerror(err), NULL);
	return;
    }
    putsock(data->fcmdsk);
    data->fcmdsk = NULL;
    data->fcmdpid = 0;
    if(data->fcmdbuf == NULL)
    {
	wbuf = swcsdup(L"");
    } else {
	addtobuf(data->fcmdbuf, 0);
	wbuf = icmbstowcs(data->fcmdbuf, NULL);
	free(data->fcmdbuf);
    }
    data->fcmdbuf = NULL;
    data->fcmdbufsize = data->fcmdbufdata = 0;
    if(wbuf == NULL)
    {
	sq(data->sk, 0, L"504", L"Filtercmd sent data which could not be converted from the local charset", NULL);
	return;
    }
    p = wbuf;
    for(p2 = wcschr(p, L'\n'); p2 != NULL; p2 = wcschr(p, L'\n'))
    {
	*(p2++) = L'\0';
	sq(data->sk, (*p2 == L'\0')?0:1, L"200", L"%%ls", p, NULL);
	p = p2;
    }
    if(*p == L'\0')
    {
	if(p == wbuf)
	    sq(data->sk, 0, L"201", L"No data returned", NULL);
    } else {
	sq(data->sk, 0, L"200", L"%%ls", p, NULL);
    }
    free(wbuf);
}

static void cmd_filtercmd(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    int i;
    pid_t pid;
    int pipe;
    char **cargv, **pp;
    char *filtercmd, *argbuf;
    size_t cargvsize, cargvdata;
    struct passwd *pwent;
    
    haveargs(2);
    havepriv(PERM_TRANS);
    if((pwent = getpwuid(data->uid)) == NULL)
    {
	flog(LOG_WARNING, "no passwd entry for UI user %i", data->uid);
	sq(sk, 0, L"505", L"System error - Could not fork session", "Internal error", NULL);
	return;
    }
    if((filtercmd = findfile(icswcstombs(confgetstr("ui", "filtercmd"), NULL, NULL), "dcdl-filtercmd", pwent->pw_dir)) == NULL)
    {
	flog(LOG_WARNING, "could not find filtercmd executable for user %s", pwent->pw_name);
	sq(sk, 0, L"505", L"System error - Could not fork session", L"Could not find filtercmd executable", NULL);
	return;
    }
    cargv = NULL;
    cargvsize = cargvdata = 0;
    addtobuf(cargv, filtercmd);
    for(i = 1; i < argc; i++)
    {
	if((argbuf = icwcstombs(argv[i], NULL)) == NULL)
	{
	    for(i = 0; i < cargvdata; i++)
		free(cargv[i]);
	    free(cargv);
	    sq(sk, 0, L"504", L"%Could not convert argument %i into local character set", i, L"%%s", strerror(errno), NULL);
	    return;
	}
	addtobuf(cargv, argbuf);
    }
    addtobuf(cargv, NULL);
    if((pid = forksess(data->uid, data->auth, NULL, NULL, FD_FILE, 0, O_RDWR, "/dev/null", FD_PIPE, 1, O_RDONLY, &pipe, FD_FILE, 2, O_RDWR, "/dev/null", FD_END)) < 0)
    {
	flog(LOG_WARNING, "could not fork session in filtercmd: %s", strerror(errno));
	sq(sk, 0, L"505", L"System error - Could not fork session", L"%%s", strerror(errno), NULL);
	return;
    }
    if(pid == 0)
    {
	execv(filtercmd, cargv);
	flog(LOG_WARNING, "could not exec filtercmd %s: %s", filtercmd, strerror(errno));
	exit(127);
    }
    for(pp = cargv; *pp; pp++)
	free(*pp);
    free(cargv);
    data->fcmdsk = wrapsock(pipe);
    data->fcmdpid = pid;
    if(data->fcmdbuf != NULL)
    {
	free(data->fcmdbuf);
	data->fcmdbuf = NULL;
    }
    data->fcmdbufsize = data->fcmdbufdata = 0;
    data->fcmdsk->data = data;
    data->fcmdsk->readcb = (void (*)(struct socket *, void *))fcmdread;
    data->fcmdsk->errcb = (void (*)(struct socket *, int, void *))fcmderr;
}

static void cmd_lstrarg(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct transfer *transfer;
    struct transarg *ta;
    
    haveargs(2);
    havepriv(PERM_TRANS);
    if((transfer = findtransfer(wcstol(argv[1], NULL, 0))) == NULL)
    {
	sq(sk, 0, L"512", L"No such transfer", NULL);
	return;
    }
    if((transfer->dir == TRNSD_DOWN) && (transfer->owner != data->uid))
    {
	sq(sk, 0, L"502", L"You do not own that transfer", NULL);
	return;
    }
    if(transfer->args == NULL)
    {
	sq(sk, 0, L"201", L"Transfer has no arguments", NULL);
    } else {
	for(ta = transfer->args; ta != NULL; ta = ta->next)
	    sq(sk, ta->next != NULL, L"200", L"%%ls", ta->rec, L"%%ls", ta->val, NULL);
    }
}

static void cmd_hashstatus(struct socket *sk, struct uidata *data, int argc, wchar_t **argv)
{
    struct sharecache *node;
    int total, hashed;
    
    total = hashed = 0;
    for(node = shareroot->child; node != NULL; node = nextscnode(node))
    {
	if(node->f.b.type == FILE_REG)
	{
	    total++;
	    if(node->f.b.hastth)
		hashed++;
	}
    }
    sq(sk, 0, L"200", L"%%i", total, L"tth", L"%%i", hashed, NULL);
}

#undef haveargs
#undef havepriv

/*
 * Reserved command numbers for nameless commands:
 *  0: Issued when a client has connected
 *  1: Issued when a named command couldn't be found
 */

static struct command commands[] =
{
    {NULL, cmd_connect},
    {NULL, cmd_notfound},
    {L"shutdown", cmd_shutdown},
    {L"quit", cmd_quit},
    {L"lsauth", cmd_lsauth},
    {L"login", cmd_login},
    {L"pass", cmd_pass},
    {L"cnct", cmd_fnetconnect},
    {L"lsnodes", cmd_lsnodes},
    {L"dcnct", cmd_disconnect},
    {L"lspa", cmd_lspa},
    {L"lspeers", cmd_lspeers},
    {L"download", cmd_download},
    {L"lstrans", cmd_lstrans},
    {L"cancel", cmd_cancel},
    {L"notify", cmd_notify},
    {L"sendchat", cmd_sendchat},
    {L"search", cmd_search},
    {L"lssrch", cmd_lssrch},
    {L"lssr", cmd_lssr},
    {L"cansrch", cmd_cansrch},
    {L"filtercmd", cmd_filtercmd},
    {L"lstrarg", cmd_lstrarg},
    {L"hashstatus", cmd_hashstatus},
    {NULL, NULL}
};

static void freequeuecmd(struct qcommand *qcmd)
{
    int i;
    
    if(qcmd->argv != NULL)
    {
	for(i = 0; i < qcmd->argc; i++)
	    free(qcmd->argv[i]);
	free(qcmd->argv);
    }
    free(qcmd);
}

static struct qcommand *unlinkqcmd(struct uidata *data)
{
    struct qcommand *qcmd;
    
    qcmd = data->queue;
    if(qcmd != NULL)
    {
	data->queue = qcmd->next;
	if(qcmd == data->queuelast)
	    data->queuelast = qcmd->next;
    }
    return(qcmd);
}

static void notifappendv(struct notif *notif, va_list args)
{
    int dt, ca;
    
    while((dt = va_arg(args, int)) != NOTIF_END)
    {
	ca = notif->argc;
	notif->argv = realloc(notif->argv, sizeof(*notif->argv) * ++notif->argc);
	notif->argv[ca].dt = dt;
	switch(dt)
	{
	case NOTIF_INT:
	case NOTIF_ID:
	    notif->argv[ca].d.n = va_arg(args, int);
	    break;
	case NOTIF_STR:
	    notif->argv[ca].d.s = wcsdup(va_arg(args, wchar_t *));
	    break;
	case NOTIF_FLOAT:
	    notif->argv[ca].d.d = va_arg(args, double);
	    break;
	}
    }
}

static void notifappend(struct notif *notif, ...)
{
    va_list args;
    
    va_start(args, notif);
    notifappendv(notif, args);
    va_end(args);
}

static struct notif *newnotif(struct uidata *data, int code, ...)
{
    struct notif *notif;
    va_list args;
    
    notif = smalloc(sizeof(*notif));
    memset(notif, 0, sizeof(*notif));
    notif->rlimit = 0.0;
    notif->ui = data;
    notif->code = code;
    va_start(args, code);
    notifappendv(notif, args);
    va_end(args);
    notif->next = NULL;
    notif->prev = data->lnotif;
    if(data->lnotif != NULL)
	data->lnotif->next = notif;
    else
	data->fnotif = notif;
    data->lnotif = notif;
    return(notif);
}

static void freenotif(struct notif *notif)
{
    int i;
    
    if(notif->next != NULL)
	notif->next->prev = notif->prev;
    if(notif->prev != NULL)
	notif->prev->next = notif->next;
    if(notif == notif->ui->fnotif)
	notif->ui->fnotif = notif->next;
    if(notif == notif->ui->lnotif)
	notif->ui->lnotif = notif->prev;
    if(notif->exptimer != NULL)
	canceltimer(notif->exptimer);
    for(i = 0; i < notif->argc; i++)
    {
	if(notif->argv[i].dt == NOTIF_STR)
	    free(notif->argv[i].d.s);
    }
    if(notif->argv != NULL)
	free(notif->argv);
    free(notif);
}

static void notifexpire(int cancelled, struct notif *notif)
{
    notif->exptimer = NULL;
    if(!cancelled)
	freenotif(notif);
}

static struct notif *findnotif(struct notif *notif, int dir, int state, int code, int id)
{
    int i, cont;
    
    for(; notif != NULL; notif = (dir?notif->next:notif->prev))
    {
	if((notif->code == code) && ((state < 0) || (state == notif->state)))
	{
	    cont = 0;
	    if(id >= 0)
	    {
		for(i = 0; i < notif->argc; i++)
		{
		    if((notif->argv[i].dt == NOTIF_ID) && (notif->argv[i].d.n != id))
		    {
			cont = 1;
			break;
		    }
		}
	    }
	    if(cont)
		continue;
	    break;
	}
    }
    return(notif);
}

static void freeuidata(struct uidata *data)
{
    int i;
    struct qcommand *qcmd;
    
    if(data->next != NULL)
	data->next->prev = data->prev;
    if(data->prev != NULL)
	data->prev->next = data->next;
    if(data == actives)
	actives = data->next;
    data->sk->readcb = NULL;
    data->sk->errcb = NULL;
    putsock(data->sk);
    while((qcmd = unlinkqcmd(data)) != NULL)
	freequeuecmd(qcmd);
    iconv_close(data->ichandle);
    if(data->cw != NULL)
	free(data->cw);
    if(data->cb != NULL)
	free(data->cb);
    if(data->argv != NULL)
    {
	for(i = 0; i < data->argc; i++)
	    free(data->argv[i]);
	free(data->argv);
    }
    if(data->auth != NULL)
	authputhandle(data->auth);
    if(data->username != NULL)
    {
	if(data->userinfo != NULL)
	    flog(LOG_INFO, "%ls logged out", data->username);
	free(data->username);
    }
    free(data->inbuf);
    while(data->fnotif != NULL)
	freenotif(data->fnotif);
    if(data->fcmdbuf != NULL)
	free(data->fcmdbuf);
    if(data->fcmdpid != 0)
	kill(-data->fcmdpid, SIGHUP);
    if(data->fcmdsk != NULL)
	putsock(data->fcmdsk);
    free(data);
}

static void queuecmd(struct uidata *data, struct command *cmd, int argc, wchar_t **argv)
{
    struct qcommand *new;
    
    new = smalloc(sizeof(*new));
    new->cmd = cmd;
    new->argc = argc;
    new->argv = argv;
    new->next = NULL;
    if(data->queuelast != NULL)
	data->queuelast->next = new;
    data->queuelast = new;
    if(data->queue == NULL)
	data->queue = new;
}

static struct uidata *newuidata(struct socket *sk)
{
    struct uidata *data;
    
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    data->sk = sk;
    getsock(sk);
    data->inbuf = smalloc(1024);
    data->uid = -1;
    if((data->ichandle = iconv_open("wchar_t", "utf-8")) == (iconv_t)-1)
    {
	flog(LOG_CRIT, "iconv cannot handle UTF-8: %s", strerror(errno));
	return(NULL);
    }
    data->next = actives;
    data->prev = NULL;
    if(actives != NULL)
	actives->prev = data;
    actives = data;
    return(data);
}

static void uiread(struct socket *sk, struct uidata *data)
{
    int ret, done;
    char *newbuf;
    char *p1, *p2;
    wchar_t *porig;
    size_t datalen, len2;
    struct command *cur;
    
    if(data->indata > 1024)
	data->indata = 0;
    if((newbuf = sockgetinbuf(sk, &datalen)) == NULL)
	return;
    sizebuf(&data->inbuf, &data->inbufsize, data->indata + datalen, 1, 1);
    memcpy(data->inbuf + data->indata, newbuf, datalen);
    free(newbuf);
    data->indata += datalen;
    if(data->cb == NULL)
    {
	data->cb = smalloc(sizeof(wchar_t) * (data->cbsize = 64));
	data->cbdata = 0;
	data->pp = data->cb;
    }
    done = 0;
    while(!done)
    {
	if(data->cbsize == data->cbdata)
	{
	    len2 = data->pp - data->cb;
	    data->cb = srealloc(data->cb, sizeof(wchar_t) * (data->cbsize *= 2));
	    data->pp = data->cb + len2;
	}
	p1 = data->inbuf;
	p2 = (char *)(porig = (data->cb + data->cbdata));
	len2 = sizeof(wchar_t) * (data->cbsize - data->cbdata);
	ret = iconv(data->ichandle, &p1, &data->indata, &p2, &len2);
	memmove(data->inbuf, p1, data->indata);
	/* Just a sanity check */
	if(((p2 - ((char *)data->cb)) % sizeof(wchar_t)) != 0)
	{
	    flog(LOG_CRIT, "Aiya! iconv does strange things to our wchar_t's!");
	    abort();
	}
	data->cbdata += (((wchar_t *)p2) - porig);
	if(ret < 0)
	{
	    switch(errno)
	    {
	    case EILSEQ:
		/* XXX: Should this really just ignore it? */
		data->indata = 0;
		done = 1;
		break;
	    case EINVAL:
		done = 1;
		break;
	    case E2BIG:
		/* Just a sanity check */
		if(data->cbsize != data->cbdata)
		{
		    flog(LOG_CRIT, "Aiya! iconv doesn't give us wchar_t's!");
		    abort();
		}
		break;
	    default:
		flog(LOG_WARNING, "bug: strange error from iconv in uiread: %s", strerror(errno));
		break;
	    }
	} else {
	    done = 1;
	}
    }
    done = 0;
    while(!done && (data->pp - data->cb < data->cbdata))
    {
	switch(data->ps)
	{
	case 0:
	    if(iswspace(*data->pp))
	    {
		if(*data->pp == L'\r')
		{
		    if(data->pp == data->cb + data->cbdata - 1)
		    {
			done = 1;
			break;
		    }
		    if(*(++data->pp) == L'\n')
		    {
			if((data->argv != NULL) && (data->argv[0] != NULL))
			{
			    for(cur = commands; cur->handler != NULL; cur++)
			    {
				if(cur->name == NULL)
				    continue;
				if(!wcscasecmp(cur->name, data->argv[0]))
				{
				    queuecmd(data, cur, data->argc, data->argv);
				    break;
				}
			    }
			    if(cur->handler == NULL)
				queuecmd(data, &commands[1], data->argc, data->argv);
			} else {
			    queuecmd(data, &commands[1], data->argc, data->argv);
			}
			data->argv = NULL;
			data->args = 0;
			data->argc = 0;
			wmemmove(data->cb, data->pp, data->cbdata -= (data->pp - data->cb));
			data->pp = data->cb;
		    } else {
			data->pp++;
		    }
		} else {
		    data->pp++;
		}
	    } else {
		data->ps = 1;
		data->cwdata = 0;
	    }
	    break;
	case 1:
	    if(iswspace(*data->pp))
	    {
		addtobuf(data->cw, L'\0');
		sizebuf(&data->argv, &data->args, data->argc + 1, sizeof(*data->argv), 1);
		data->argv[data->argc++] = data->cw;
		data->cw = NULL;
		data->cwsize = 0;
		data->cwdata = 0;
		data->ps = 0;
	    } else if(*data->pp == L'\"') {
		data->ps = 2;
		data->pp++;
	    } else if(*data->pp == L'\\') {
		if(data->pp == data->cb + data->cbdata - 1)
		{
		    done = 1;
		    break;
		}
		addtobuf(data->cw, *(++data->pp));
		data->pp++;
	    } else {
		addtobuf(data->cw, *(data->pp++));
	    }
	    break;
	case 2:
	    if(*data->pp == L'\"') 
	    {
		data->ps = 1;
	    } else if(*data->pp == L'\\') {
		if(data->pp == data->cb + data->cbdata - 1)
		{
		    done = 1;
		    break;
		}
		addtobuf(data->cw, *(++(data->pp)));
	    } else {
		addtobuf(data->cw, *data->pp);
	    }
	    data->pp++;
	    break;
	}
    }
}

static void uierror(struct socket *sk, int err, struct uidata *data)
{
    if(err)
	flog(LOG_WARNING, "error occurred on UI socket: %s", strerror(err));
    freeuidata(data);
}

static void uiaccept(struct socket *sk, struct socket *newsk, void *data)
{
    struct uidata *uidata;
    
    newsk->data = uidata = newuidata(newsk);
    socksettos(newsk, confgetint("ui", "uitos"));
    if(uidata == NULL)
	return;
    newsk->errcb = (void (*)(struct socket *, int, void *))uierror;
    newsk->readcb = (void (*)(struct socket *, void *))uiread;
    queuecmd(uidata, &commands[0], 0, NULL);
}

static int srcheta(struct search *srch, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_SRCH) && data->notify.b.srch && !wcscmp(srch->owner, data->username))
	    newnotif(data, 620, NOTIF_ID, srch->id, NOTIF_INT, srch->eta - time(NULL), NOTIF_END);
    }
    return(0);
}

static int srchcommit(struct search *srch, void *uudata)
{
    struct uidata *data;

    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_SRCH) && data->notify.b.srch && !wcscmp(srch->owner, data->username))
	    newnotif(data, 621, NOTIF_ID, srch->id, NOTIF_END);
    }
    return(0);
}

static int srchres(struct search *srch, struct srchres *sr, void *uudata)
{
    struct uidata *data;

    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_SRCH) && data->notify.b.srch && !wcscmp(srch->owner, data->username))
	{
	    newnotif(data, 622, NOTIF_ID, srch->id, NOTIF_STR, sr->filename, NOTIF_STR, sr->fnet->name, NOTIF_STR, sr->peerid, NOTIF_INT, sr->size,
		     NOTIF_INT, sr->slots, NOTIF_INT, (sr->fn == NULL)?-1:(sr->fn->id), NOTIF_FLOAT, sr->time, NOTIF_STR, (sr->hash == NULL)?L"":unparsehash(sr->hash), NOTIF_END);
	}
    }
    return(0);
}

static int recvchat(struct fnetnode *fn, int public, wchar_t *name, wchar_t *peer, wchar_t *string, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_CHAT) && data->notify.b.fnchat)
	    newnotif(data, 600, NOTIF_ID, fn->id, NOTIF_INT, public, NOTIF_STR, name, NOTIF_STR, peer, NOTIF_STR, string, NOTIF_END);
    }
    return(0);
}

static int fnactive(struct fnetnode *fn, wchar_t *attrib, void *uudata)
{
    struct uidata *data;
    struct notif *notif;
    
    if(!wcscmp(attrib, L"state"))
    {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(data->notify.b.fnact)
		newnotif(data, 601, NOTIF_ID, fn->id, NOTIF_INT, fn->state, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"name")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(data->notify.b.fnact)
		newnotif(data, 602, NOTIF_ID, fn->id, NOTIF_STR, fn->name, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"numpeers")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(data->notify.b.fnact)
	    {
		if((notif = findnotif(data->fnotif, 1, NOTIF_PEND, 605, fn->id)) != NULL)
		    notif->argv[1].d.n = fn->numpeers;
		else
		    newnotif(data, 605, NOTIF_ID, fn->id, NOTIF_INT, fn->numpeers, NOTIF_END)->rlimit = 0.5;
	    }
	}
    }
    return(0);
}

static int fnunlink(struct fnetnode *fn, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(data->notify.b.fnact)
	    newnotif(data, 603, NOTIF_ID, fn->id, NOTIF_END);
    }
    return(0);
}

static int peernew(struct fnetnode *fn, struct fnetpeer *peer, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(data->notify.b.fnpeer)
	    newnotif(data, 630, NOTIF_INT, fn->id, NOTIF_STR, peer->id, NOTIF_STR, peer->nick, NOTIF_END);
    }
    return(0);
}

static int peerdel(struct fnetnode *fn, struct fnetpeer *peer, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(data->notify.b.fnpeer)
	    newnotif(data, 631, NOTIF_INT, fn->id, NOTIF_STR, peer->id, NOTIF_END);
    }
    return(0);
}

static int peerchange(struct fnetnode *fn, struct fnetpeer *peer, struct fnetpeerdi *di, void *uudata)
{
    struct uidata *data;
    struct notif *notif;
    wchar_t buf[32];
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(data->notify.b.fnpeer)
	{
	    for(notif = data->fnotif; notif != NULL; notif = notif->next)
	    {
		if((notif->code == 632) && (notif->state == NOTIF_PEND) && (notif->argv[0].d.n == fn->id) && !wcscmp(notif->argv[1].d.s, peer->id))
		    break;
	    }
	    if(notif == NULL)
		notif = newnotif(data, 632, NOTIF_INT, fn->id, NOTIF_STR, peer->id, NOTIF_STR, peer->nick, NOTIF_END);
	    notifappend(notif, NOTIF_STR, di->datum->id, NOTIF_INT, di->datum->datatype, NOTIF_END);
	    switch(di->datum->datatype)
	    {
	    case FNPD_INT:
		notifappend(notif, NOTIF_INT, di->data.num, NOTIF_END);
		break;
	    case FNPD_STR:
		notifappend(notif, NOTIF_STR, di->data.str, NOTIF_END);
		break;
	    case FNPD_LL:
		swprintf(buf, sizeof(buf) / sizeof(*buf), L"%lli", di->data.lnum);
		notifappend(notif, NOTIF_STR, buf, NOTIF_END);
		break;
	    }
	}
    }
    return(0);
}

static int newfnetnode(struct fnetnode *fn, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(data->notify.b.fnact)
	    newnotif(data, 604, NOTIF_ID, fn->id, NOTIF_STR, fn->fnet->name, NOTIF_END);
    }
    CBREG(fn, fnetnode_ac, fnactive, NULL, NULL);
    CBREG(fn, fnetnode_chat, recvchat, NULL, NULL);
    CBREG(fn, fnetnode_unlink, fnunlink, NULL, NULL);
    CBREG(fn, fnetpeer_new, peernew, NULL, NULL);
    CBREG(fn, fnetpeer_del, peerdel, NULL, NULL);
    CBREG(fn, fnetpeer_chdi, peerchange, NULL, NULL);
    return(0);
}

static int transferchattr(struct transfer *transfer, wchar_t *attrib, void *uudata)
{
    struct uidata *data;
    
    if(!wcscmp(attrib, L"state"))
    {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 611, NOTIF_ID, transfer->id, NOTIF_INT, transfer->state, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"nick")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 612, NOTIF_ID, transfer->id, NOTIF_STR, transfer->peernick, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"size")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 613, NOTIF_ID, transfer->id, NOTIF_INT, transfer->size, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"error")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 614, NOTIF_ID, transfer->id, NOTIF_INT, transfer->error, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"path")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 616, NOTIF_ID, transfer->id, NOTIF_STR, transfer->path, NOTIF_END);
	}
    } else if(!wcscmp(attrib, L"hash")) {
	for(data = actives; data != NULL; data = data->next)
	{
	    if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
		newnotif(data, 618, NOTIF_ID, transfer->id, NOTIF_STR, (transfer->hash == NULL)?L"":unparsehash(transfer->hash), NOTIF_END);
	}
    }
    return(0);
}

static int transferprog(struct transfer *transfer, void *uudata)
{
    struct uidata *data;
    struct notif *notif;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_TRANS) && data->notify.b.trprog && ((transfer->owner == 0) || (transfer->owner == data->uid)))
	{
	    if((notif = findnotif(data->fnotif, 1, NOTIF_PEND, 615, transfer->id)) != NULL)
		notif->argv[1].d.n = transfer->curpos;
	    else
		newnotif(data, 615, NOTIF_ID, transfer->id, NOTIF_INT, transfer->curpos, NOTIF_END)->rlimit = 0.5;
	}
    }
    return(0);
}

static int transferdestroyed(struct transfer *transfer, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
	    newnotif(data, 617, NOTIF_ID, transfer->id, NOTIF_END);
    }
    return(0);
}

static int newtransfernotify(struct transfer *transfer, void *uudata)
{
    struct uidata *data;
    
    for(data = actives; data != NULL; data = data->next)
    {
	if(haspriv(data, PERM_TRANS) && data->notify.b.tract && ((transfer->owner == 0) || (transfer->owner == data->uid)))
	    newnotif(data, 610, NOTIF_ID, transfer->id, NOTIF_INT, transfer->dir, NOTIF_STR, transfer->peerid, NOTIF_STR, (transfer->path == NULL)?L"":transfer->path, NOTIF_END);
    }
    CBREG(transfer, trans_ac, transferchattr, NULL, NULL);
    CBREG(transfer, trans_p, transferprog, NULL, NULL);
    CBREG(transfer, trans_destroy, transferdestroyed, NULL, NULL);
    return(0);
}

static struct uiuser *newuser(wchar_t *name, unsigned long perms)
{
    struct uiuser *new;
    
    new = smalloc(sizeof(*new));
    new->used = 0;
    new->name = swcsdup(name);
    new->perms = perms;
    new->delete = 0;
    new->next = users;
    new->prev = NULL;
    if(users != NULL)
	users->prev = new;
    users = new;
    return(new);
}

static void freeuser(struct uiuser *user)
{
    if(user->next != NULL)
	user->next->prev = user->prev;
    if(user->prev != NULL)
	user->prev->next = user->next;
    if(user == users)
	users = user->next;
    free(user->name);
    free(user);
}

static int conf_user(int argc, wchar_t **argv)
{
    int i, perms, permmod;
    struct uiuser *user;
    wchar_t *p;
    
    if(argc < 3)
    {
	flog(LOG_WARNING, "not enough arguments given for user command");
	return(1);
    }
    perms = 0;
    for(i = 2; i < argc; i++)
    {
	if(!iswalpha(argv[i][0]))
	    p = argv[i] + 1;
	else
	    p = argv[i];
	if(!wcscmp(p, L"disallow"))
	    permmod = PERM_DISALLOW;
	if(!wcscmp(p, L"admin"))
	    permmod = PERM_ADMIN;
	if(!wcscmp(p, L"fnetctl"))
	    permmod = PERM_FNETCTL;
	if(!wcscmp(p, L"trans"))
	    permmod = PERM_TRANS;
	if(!wcscmp(p, L"transcu"))
	    permmod = PERM_TRANSCU;
	if(!wcscmp(p, L"chat"))
	    permmod = PERM_CHAT;
	if(!wcscmp(p, L"srch"))
	    permmod = PERM_SRCH;
	if(!wcscmp(p, L"all"))
	    permmod = ~0;
	if(argv[i][0] == L'-')
	    perms &= ~permmod;
	else
	    perms |= permmod;
    }
    if((user = finduser(argv[1])) == NULL)
    {
	newuser(argv[1], perms);
    } else {
	user->delete = 0;
	user->perms = perms;
    }
    return(0);
}

static void preinit(int hup)
{
    struct uiuser *user;
    
    if(!hup)
    {
	newuser(L"default", 0);
    } else {
	for(user = users; user != NULL; user = user->next)
	{
	    if(!wcscmp(user->name, L"default"))
		user->delete = 1;
	}
    }
}

#ifdef HAVE_IPV6
static struct sockaddr *getnameforport(int port, socklen_t *len)
{
    static struct sockaddr_in6 addr;
    
    memset(&addr, 0, sizeof(addr));
    addr.sin6_family = AF_INET6;
    addr.sin6_port = htons(port);
    addr.sin6_addr = in6addr_any;
    if(len != NULL)
	*len = sizeof(addr);
    return((struct sockaddr *)&addr);
}
#else
static struct sockaddr *getnameforport(int port, socklen_t *len)
{
    static struct sockaddr_in addr;
    
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    if(len != NULL)
	*len = sizeof(addr);
    return((struct sockaddr *)&addr);
}
#endif

static int portupdate(struct configvar *var, void *uudata)
{
    struct sockaddr *addr;
    socklen_t addrlen;
    struct socket *newsock;
    
    addr = getnameforport(var->val.num, &addrlen);
    if((uisocket = netcslistenlocal(SOCK_STREAM, addr, addrlen, uiaccept, NULL)) == NULL)
    {
	flog(LOG_WARNING, "could not create new UI socket, reverting to old: %s", strerror(errno));
	return(0);
    }
    if(uisocket != NULL)
	putsock(uisocket);
    uisocket = newsock;
    return(0);
}

static int init(int hup)
{
    struct sockaddr *addr;
    socklen_t addrlen;
    struct uiuser *user, *next;
    
    if(hup)
    {
	for(user = users; user != NULL; user = next)
	{
	    next = user->next;
	    if(user->delete)
		freeuser(user);
	}
    }
    if(!hup)
    {
	if(uisocket != NULL)
	    putsock(uisocket);
	addr = getnameforport(confgetint("ui", "port"), &addrlen);
	if((uisocket = netcslistenlocal(SOCK_STREAM, addr, addrlen, uiaccept, NULL)) == NULL)
	{
	    flog(LOG_CRIT, "could not create UI socket: %s", strerror(errno));
	    return(1);
	}
	CBREG(confgetvar("ui", "port"), conf_update, portupdate, NULL, NULL);
	GCBREG(newfncb, newfnetnode, NULL);
	GCBREG(newtransfercb, newtransfernotify, NULL);
    }
    return(0);
}

static int run(void)
{
    int i, id;
    struct uidata *data, *next;
    struct qcommand *qcmd;
    struct notif *notif, *nnotif;
    wchar_t buf[64];
    
    for(data = actives; data != NULL; data = next)
    {
	next = data->next;
	if(data->close)
	    freeuidata(data);
    }
    for(data = actives; data != NULL; data = data->next)
    {
	for(notif = data->fnotif; notif != NULL; notif = nnotif)
	{
	    nnotif = notif->next;
	    if(notif->state == NOTIF_WAIT)
		continue;
	    id = -1;
	    for(i = 0; i < notif->argc; i++)
	    {
		if(notif->argv[i].dt == NOTIF_ID)
		{
		    id = notif->argv[i].d.n;
		    break;
		}
	    }
	    if(findnotif(notif->prev, 0, -1, notif->code, id) != NULL)
		continue;
	    sq(data->sk, 2, L"%%i", notif->code, NULL);
	    for(i = 0; i < notif->argc; i++)
	    {
		switch(notif->argv[i].dt)
		{
		case NOTIF_INT:
		case NOTIF_ID:
		    sq(data->sk, 2, L"%%i", notif->argv[i].d.n, NULL);
		    break;
		case NOTIF_STR:
		    if(notif->argv[i].d.s[0] == L'%')
			sq(data->sk, 2, L"%%s", notif->argv[i].d.s, NULL);
		    else
			sq(data->sk, 2, notif->argv[i].d.s, NULL);
		    break;
		case NOTIF_FLOAT:
		    swprintf(buf, 64, L"%f", notif->argv[i].d.d);
		    sq(data->sk, 2, buf, NULL);
		    break;
		}
	    }
	    sq(data->sk, 0, NULL);
	    if(notif->rlimit != 0)
	    {
		notif->state = NOTIF_WAIT;
		notif->exptimer = timercallback(ntime() + notif->rlimit, (void (*)(int, void *))notifexpire, notif);
	    } else {
		freenotif(notif);
	    }
	}
	if((qcmd = unlinkqcmd(data)) != NULL)
	{
	    qcmd->cmd->handler(data->sk, data, qcmd->argc, qcmd->argv);
	    freequeuecmd(qcmd);
	    return(1);
	}
    }
    return(0);
}

static void terminate(void)
{
    while(users != NULL)
	freeuser(users);
}

static struct configvar myvars[] =
{
    {CONF_VAR_BOOL, "onlylocal", {.num = 1}},
    {CONF_VAR_INT, "port", {.num = 1500}},
    {CONF_VAR_INT, "uitos", {.num = SOCK_TOS_MINDELAY}},
    {CONF_VAR_STRING, "filtercmd", {.str = L"dc-filtercmd"}},
    {CONF_VAR_END}
};

static struct configcmd mycmds[] =
{
    {"user", conf_user},
    {NULL}
};

static struct module me =
{
    .name = "ui",
    .conf =
    {
	.vars = myvars,
	.cmds = mycmds
    },
    .preinit = preinit,
    .init = init,
    .run = run,
    .terminate = terminate
};

MODULE(me)
