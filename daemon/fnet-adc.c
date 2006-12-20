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
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <wctype.h>
#include <iconv.h>
#include <errno.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "filenet.h"
#include "log.h"
#include "module.h"
#include "utils.h"
#include "client.h"
#include "transfer.h"
#include "sysevents.h"
#include "net.h"
#include "tiger.h"

/* Protocol states */
#define ADC_PROTOCOL 0
#define ADC_IDENTIFY 1
#define ADC_VERIFY 2
#define ADC_NORMAL 3
#define ADC_DATA 4

struct command {
    wchar_t *name;
    int minargs, needsender;
    void (*func)(struct fnetnode *fn, wchar_t *command, wchar_t *sender, int argc, wchar_t **argv);
};

struct qcmd {
    struct qcmd *next;
    wchar_t **args;
};

struct adchub {
    char *inbuf;
    size_t inbufdata, inbufsize;
    wchar_t *sid;
    wchar_t *cb;
    size_t cbdata, cbsize;
    wchar_t **sup;
    iconv_t ich;
    int state;
    struct qcmd *queue;
};

static wchar_t *eoc;
/* I've never understood why both of these are necessary, but... */
static wchar_t *privid, *cid;

static wchar_t **parseadc(wchar_t *cmdline)
{
    wchar_t **ret;
    size_t retsize, retdata;
    wchar_t *p, *p2, *ep;
    wchar_t r;
    ssize_t len;
    
    ret = NULL;
    retsize = retdata = 0;
    p = cmdline;
    do {
	if((p2 = wcschr(p, L' ')) != NULL)
	    *(p2++) = L'\0';
	len = wcslen(p);
	for(ep = p; len > 0; ep++, len--) {
	    if(*ep == L'\\') {
		if(ep[1] == L's') {
		    r = L' ';
		} else if(ep[1] == L'n') {
		    r = L'\n';
		} else if(ep[1] == L'\\') {
		    r = L'\\';
		} else {
		    memmove(ep, ep + 2, (len -= 2) * sizeof(*ep));
		    ep--;
		    continue;
		}
		memmove(ep, ep + 1, --len * sizeof(*ep));
		*ep = r;
	    }
	}
	*ep = L'\0';
	addtobuf(ret, swcsdup(p));
	p = p2;
    } while(p2 != NULL);
    addtobuf(ret, NULL);
    return(ret);
}

static void sendeoc(struct socket *sk)
{
    sockqueue(sk, "\n", 1);
}

static void sendadc1(struct socket *sk, int sep, wchar_t *arg)
{
    char *mbsbuf;
    wchar_t *buf;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    if(sep)
	addtobuf(buf, L' ');
    for(; *arg != L'\0'; arg++) {
	if(*arg == L' ')
	    bufcat(buf, L"\\s", 2);
	else if(*arg == L'\n')
	    bufcat(buf, L"\\n", 2);
	else if(*arg == L'\\')
	    bufcat(buf, L"\\\\", 2);
	else
	    addtobuf(buf, *arg);
    }
    addtobuf(buf, L'\0');
    mbsbuf = icwcstombs(buf, "utf-8");
    sockqueue(sk, mbsbuf, strlen(mbsbuf));
    free(mbsbuf);
}

static void sendadc(struct socket *sk, int sep, ...)
{
    va_list args;
    wchar_t *arg;
    int i;
    
    va_start(args, sep);
    for(i = 0; (arg = va_arg(args, wchar_t *)) != NULL; i++) {
	if(arg == eoc)
	    sendeoc(sk);
	else
	    sendadc1(sk, (i == 0)?sep:1, arg);
    }
    va_end(args);
}

static struct qcmd *newqcmd(struct qcmd **queue, wchar_t **args)
{
    struct qcmd *new;
    
    while(*queue != NULL)
	queue = &((*queue)->next);
    new = smalloc(sizeof(*new));
    new->next = NULL;
    new->args = args;
    *queue = new;
    return(new);
}

static struct qcmd *ulqcmd(struct qcmd **queue)
{
    struct qcmd *ret;
    
    if((ret = *queue) == NULL)
	return(NULL);
    *queue = ret->next;
    return(ret);
}

static void freeqcmd(struct qcmd *qcmd)
{
    freeparr(qcmd->args);
    free(qcmd);
}

#define ADC_CMDFN(name) static void name(struct fnetnode *fn, wchar_t *command, wchar_t *sender, int argc, wchar_t **argv)
#define ADC_CMDCOM struct socket *sk = fn->sk; struct adchub *hub = fn->data;

ADC_CMDFN(cmd_sup)
{
    ADC_CMDCOM;
    int i, o, f;
    
    for(i = 1; i < argc; i++) {
	if(wcslen(argv[i]) < 3)
	    continue;
	for(o = 0, f = 0; hub->sup[o]; o++) {
	    if(!wcscmp(argv[i] + 2, hub->sup[o])) {
		f = 1;
		break;
	    }
	}
	if(!wcsncmp(argv[i], L"AD", 2)) {
	    if(f)
		continue;
	    hub->sup = srealloc(hub->sup, sizeof(*(hub->sup)) * (o + 1));
	    hub->sup[o] = swcsdup(argv[i] + 2);
	} else if(!wcsncmp(argv[i], L"RM", 2)) {
	    if(!f)
		continue;
	    memmove(hub->sup[o], hub->sup[o + 1], parrlen(hub->sup) - o);
	}
    }
}

ADC_CMDFN(cmd_sid)
{
    ADC_CMDCOM;
    
    if(hub->sid != NULL)
	free(hub->sid);
    hub->sid = swcsdup(argv[1]);
    if(hub->state == ADC_PROTOCOL) {
	hub->state = ADC_IDENTIFY;
    }
}

static struct command hubcmds[] = {
    {L"SUP", 1, 0, cmd_sup},
    {L"SID", 2, 0, cmd_sid},
    {NULL, 0, 0, NULL}
};

static void dispatch(struct qcmd *qcmd, struct fnetnode *fn)
{
    struct command *cmd;
    int argc;
    wchar_t *cmdnm, *sender, **argv;
    
    if((argc = parrlen(argv = qcmd->args)) < 1)
	return;
    cmdnm = *(argv++);
    argc--;
    if(wcslen(cmdnm) < 2)
	return;
    sender = NULL;
    if((*cmdnm == L'B') || (*cmdnm == L'F')) {
	if(argc < 1)
	    return;
	sender = *(argv++);
	argc--;
    } else if((*cmdnm == L'D') || (*cmdnm == L'E')) {
	if(argc < 2)
	    return;
	sender = *argv;
	argv += 2;
	argc -= 2;
    }
    for(cmd = hubcmds; cmd->func != NULL; cmd++) {
	if(!wcscmp(cmd->name, qcmd->args[0] + 1)) {
	    if(argc < cmd->minargs)
		return;
	    cmd->func(fn, cmdnm, sender, argc, qcmd->args);
	    return;
	}
    }
    flog(LOG_DEBUG, "unknown adc command: %ls", qcmd->args[0]);
}

static void hubread(struct socket *sk, struct fnetnode *fn)
{
    int ret;
    struct adchub *hub;
    char *newbuf, *p1, *p2;
    wchar_t *p;
    size_t datalen, cblen;
    
    hub = fn->data;
    if((newbuf = sockgetinbuf(sk, &datalen)) == NULL)
	return;
    if(hub->inbufdata > 1024)
	hub->inbufdata = 0;
    bufcat(hub->inbuf, newbuf, datalen);
    free(newbuf);
    /* Memory eating protection */
    if(hub->cbdata > 65536)
	hub->cbdata = 0;
    while(hub->inbufdata > 0) {
	if(hub->cbdata == hub->cbsize)
	    sizebuf2(hub->cb, hub->cbdata + datalen, 1);
	p1 = hub->inbuf;
	p2 = (char *)(hub->cb + hub->cbdata);
	cblen = sizeof(*(hub->cb)) * (hub->cbsize - hub->cbdata);
	ret = iconv(hub->ich, &p1, &hub->inbufdata, &p2, &cblen);
	memmove(hub->inbuf, p1, hub->inbufdata);
	if(((p2 - ((char *)hub->cb)) % sizeof(*hub->cb)) != 0) {
	    flog(LOG_CRIT, "Spotted a dismembered wchar_t!");
	    abort();
	}
	hub->cbdata = hub->cbsize - (cblen / sizeof(*(hub->cb)));
	if(ret < 0) {
	    if(errno == EILSEQ) {
		flog(LOG_DEBUG, "adc fnetnode %i sent illegal utf-8 sequence", fn->id);
		killfnetnode(fn);
		return;
	    } else if(errno == EINVAL) {
		break;
	    } else if(errno == E2BIG) {
		/* continue; */
	    } else {
		flog(LOG_WARNING, "bug? iconv returned unexpected error: %s", strerror(errno));
		return;
	    }
	}
    }
    while((p = wmemchr(hub->cb, L'\n', hub->cbdata)) != NULL) {
	*(p++) = L'\0';
	newqcmd(&hub->queue, parseadc(hub->cb));
	memmove(hub->cb, p, (hub->cbdata -= (p - hub->cb)) * sizeof(*(hub->cb)));
    }
}

static void huberr(struct socket *sk, int err, struct fnetnode *fn)
{
    killfnetnode(fn);
}

static void hubconnect(struct fnetnode *fn)
{
    struct adchub *hub;
    
    fn->sk->readcb = (void (*)(struct socket *, void *))hubread;
    fn->sk->errcb = (void (*)(struct socket *, int, void *))huberr;
    fn->sk->data = fn;
    getfnetnode(fn);
    
    hub = smalloc(sizeof(*hub));
    memset(hub, 0, sizeof(*hub));
    if((hub->ich = iconv_open("wchar_t", "utf-8")) == (iconv_t)-1) {
	flog(LOG_CRIT, "iconv cannot handle UTF-8: %s", strerror(errno));
	killfnetnode(fn);
	return;
    }
    fn->data = hub;
    sendadc(fn->sk, 0, L"HSUP", L"ADBASE", eoc, NULL);
}

static void hubdestroy(struct fnetnode *fn)
{
    struct adchub *hub;
    
    if((hub = fn->data) == NULL)
	return;
    iconv_close(hub->ich);
    if(hub->inbuf != NULL)
	free(hub->inbuf);
    if(hub->sup != NULL)
	freeparr(hub->sup);
    free(hub);
}

static int hubsetnick(struct fnetnode *fn, wchar_t *newnick)
{
    return(0);
}

static int hubreqconn(struct fnetpeer *peer)
{
    return(0);
}

static struct fnet adcnet_store = {
    .connect = hubconnect,
    .destroy = hubdestroy,
    .setnick = hubsetnick,
    .reqconn = hubreqconn,
    .name = L"adc"
};

static struct fnet *adcnet = &adcnet_store;

static int run(void)
{
    int ret;
    struct fnetnode *fn, *nextfn;
    struct adchub *hub;
    struct qcmd *qcmd;
    
    ret = 0;
    for(fn = fnetnodes; fn != NULL; fn = nextfn) {
	nextfn = fn->next;
	if(fn->fnet != adcnet)
	    continue;
	if((hub = fn->data) == NULL)
	    continue;
	if((qcmd = ulqcmd(&hub->queue)) != NULL) {
	    if((fn->sk != NULL) && (fn->sk->state == SOCK_EST))
		dispatch(qcmd, fn);
	    freeqcmd(qcmd);
	    ret = 1;
	    break;
	}
    }
    return(ret);
}

static void preinit(int hup)
{
    if(hup)
	return;
    regfnet(adcnet);
}

static int init(int hup)
{
    int i;
    char idbuf[24], *id32;
    struct tigerhash th;
    
    if(!hup) {
	eoc = swcsdup(L"");
	
	if((privid = fetchvar("adc.pid", NULL)) == NULL) {
	    for(i = 0; i < sizeof(idbuf); i++)
		idbuf[i] = rand() % 256;
	    id32 = base32encode(idbuf, sizeof(idbuf));
	    id32[39] = 0;
	    privid = icmbstowcs(id32, "us-ascii");
	    free(id32);
	    storevar("adc.pid", privid, sizeof(*privid) * (wcslen(privid) + 1));
	}
	
	id32 = base32decode(icswcstombs(privid, "us-ascii", NULL), NULL);
	inittiger(&th);
	dotiger(&th, id32, 24);
	synctiger(&th);
	free(id32);
	restiger(&th, idbuf);
	id32 = base32encode(idbuf, sizeof(idbuf));
	id32[39] = 0;
	cid = icmbstowcs(id32, "us-ascii");
	free(id32);
    }
    return(0);
}

static void terminate(void)
{
    
}

static struct configvar myvars[] = {
    {CONF_VAR_INT, "udpport", {.num = 0}},
    {CONF_VAR_INT, "tcpport", {.num = 0}},
    {CONF_VAR_END}
};

static struct module me = {
    .conf = {
	.vars = myvars
    },
    .preinit = preinit,
    .init = init,
    .run = run,
    .terminate = terminate,
    .name = "adc"
};

MODULE(me)
