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
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <sys/wait.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "log.h"
#include "utils.h"
#include "sysevents.h"
#include "auth.h"
#include "transfer.h"
#include "module.h"
#include "client.h"

static void killfilter(struct transfer *transfer);

struct transfer *transfers = NULL;
int numtransfers = 0;
GCBCHAIN(newtransfercb, struct transfer *);

void freetransfer(struct transfer *transfer)
{
    if(transfer == transfers)
	transfers = transfer->next;
    if(transfer->next != NULL)
	transfer->next->prev = transfer->prev;
    if(transfer->prev != NULL)
	transfer->prev->next = transfer->next;
    CBCHAINDOCB(transfer, trans_destroy, transfer);
    CBCHAINFREE(transfer, trans_ac);
    CBCHAINFREE(transfer, trans_act);
    CBCHAINFREE(transfer, trans_p);
    CBCHAINFREE(transfer, trans_destroy);
    CBCHAINFREE(transfer, trans_filterout);
    while(transfer->args != NULL)
	freewcspair(transfer->args, &transfer->args);
    if(transfer->filter != -1)
	killfilter(transfer);
    if(transfer->etimer != NULL)
	canceltimer(transfer->etimer);
    if(transfer->auth != NULL)
	authputhandle(transfer->auth);
    if(transfer->peerid != NULL)
	free(transfer->peerid);
    if(transfer->peernick != NULL)
	free(transfer->peernick);
    if(transfer->path != NULL)
	free(transfer->path);
    if(transfer->actdesc != NULL)
	free(transfer->actdesc);
    if(transfer->filterbuf != NULL)
	free(transfer->filterbuf);
    if(transfer->hash != NULL)
	freehash(transfer->hash);
    if(transfer->localend != NULL)
    {
	transfer->localend->readcb = NULL;
	transfer->localend->writecb = NULL;
	transfer->localend->errcb = NULL;
	putsock(transfer->localend);
    }
    if(transfer->filterout != NULL)
    {
	transfer->filterout->readcb = NULL;
	transfer->filterout->writecb = NULL;
	transfer->filterout->errcb = NULL;
	putsock(transfer->filterout);
    }
    if(transfer->fn != NULL)
	putfnetnode(transfer->fn);
    free(transfer);
    numtransfers--;
}

struct transfer *newtransfer(void)
{
    struct transfer *new;
    static int curid = 0;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    new->id = curid++;
    new->size = -1;
    new->endpos = -1;
    new->filter = -1;
    CBCHAININIT(new, trans_ac);
    CBCHAININIT(new, trans_act);
    CBCHAININIT(new, trans_p);
    CBCHAININIT(new, trans_destroy);
    CBCHAININIT(new, trans_filterout);
    new->next = NULL;
    new->prev = NULL;
    time(&new->activity);
    numtransfers++;
    return(new);
}

void transferattach(struct transfer *transfer, struct transferiface *iface, void *data)
{
    if(transfer->iface != NULL)
	transferdetach(transfer);
    transfer->iface = iface;
    transfer->ifacedata = data;
}

void transferdetach(struct transfer *transfer)
{
    if(transfer->iface != NULL)
    {
	transfer->iface->detach(transfer, transfer->ifacedata);
	transfer->iface = NULL;
	transfer->ifacedata = NULL;
    }
}

struct transfer *finddownload(wchar_t *peerid)
{
    struct transfer *transfer;

    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if((transfer->dir == TRNSD_DOWN) && (transfer->iface == NULL) && !wcscmp(peerid, transfer->peerid))
	    break;
    }
    return(transfer);
}

struct transfer *newupload(struct fnetnode *fn, struct fnet *fnet, wchar_t *nickid, struct transferiface *iface, void *data)
{
    struct transfer *transfer;
    
    transfer = newtransfer();
    if(fnet != NULL)
	transfer->fnet = fnet;
    else
	transfer->fnet = fn->fnet;
    transfer->peerid = swcsdup(nickid);
    transfer->state = TRNS_HS;
    transfer->dir = TRNSD_UP;
    if(fn != NULL)
	getfnetnode(transfer->fn = fn);
    transferattach(transfer, iface, data);
    linktransfer(transfer);
    bumptransfer(transfer);
    return(transfer);
}

void linktransfer(struct transfer *transfer)
{
    transfer->next = transfers;
    transfer->prev = NULL;
    if(transfers != NULL)
	transfers->prev = transfer;
    transfers = transfer;
    GCBCHAINDOCB(newtransfercb, transfer);
}

void resettransfer(struct transfer *transfer)
{
    if(transfer->dir == TRNSD_DOWN)
    {
	if(transfer->iface != NULL)
	    transferdetach(transfer);
	killfilter(transfer);
	transfersetstate(transfer, TRNS_WAITING);
	transfersetactivity(transfer, L"reset");
	return;
    }
}

struct transfer *findtransfer(int id)
{
    struct transfer *transfer;
    
    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if(transfer->id == id)
	    break;
    }
    return(transfer);
}

static void transexpire(int cancelled, struct transfer *transfer)
{
    transfer->etimer = NULL;
    if(!cancelled)
	bumptransfer(transfer);
    else
	transfer->timeout = 0;
}

static void transferread(struct socket *sk, struct transfer *transfer)
{
    if(sockgetdatalen(sk) >= 65536)
	sk->ignread = 1;
    if((transfer->iface != NULL) && (transfer->iface->gotdata != NULL))
	transfer->iface->gotdata(transfer, transfer->ifacedata);
}

static void transferwrite(struct socket *sk, struct transfer *transfer)
{
    if((transfer->iface != NULL) && (transfer->iface->wantdata != NULL))
	transfer->iface->wantdata(transfer, transfer->ifacedata);
}

static void transfererr(struct socket *sk, int errno, struct transfer *transfer)
{
    if((transfer->iface != NULL) && (transfer->iface->endofdata != NULL))
	transfer->iface->endofdata(transfer, transfer->ifacedata);
}

void transferputdata(struct transfer *transfer, void *buf, size_t size)
{
    time(&transfer->activity);
    sockqueue(transfer->localend, buf, size);
    transfer->curpos += size;
    CBCHAINDOCB(transfer, trans_p, transfer);
}

void transferendofdata(struct transfer *transfer)
{
    if(transfer->curpos >= transfer->size)
    {
	transfersetstate(transfer, TRNS_DONE);
	transfer->localend->readcb = NULL;
	transfer->localend->writecb = NULL;
	transfer->localend->errcb = NULL;
	putsock(transfer->localend);
	transfer->localend = NULL;
    } else {
	resettransfer(transfer);
    }
}

size_t transferdatasize(struct transfer *transfer)
{
    return(sockqueuesize(transfer->localend));
}

void *transfergetdata(struct transfer *transfer, size_t *size)
{
    void *buf;
    
    if(transfer->localend == NULL)
	return(NULL);
    transfer->localend->ignread = 0;
    time(&transfer->activity);
    if((buf = sockgetinbuf(transfer->localend, size)) == NULL)
	return(NULL);
    if((transfer->endpos >= 0) && (transfer->curpos + *size >= transfer->endpos))
    {
	*size = transfer->endpos - transfer->curpos;
	buf = srealloc(buf, *size);
    }
    transfer->curpos += *size;
    CBCHAINDOCB(transfer, trans_p, transfer);
    return(buf);
}

void transferprepul(struct transfer *transfer, size_t size, size_t start, size_t end, struct socket *lesk)
{
    transfersetsize(transfer, size);
    transfer->curpos = start;
    transfer->endpos = end;
    lesk->ignread = 1;
    transfersetlocalend(transfer, lesk);
}

void transferstartdl(struct transfer *transfer, struct socket *sk)
{
    transfersetstate(transfer, TRNS_MAIN);
    socksettos(sk, confgetint("transfer", "dltos"));
}

void transferstartul(struct transfer *transfer, struct socket *sk)
{
    transfersetstate(transfer, TRNS_MAIN);
    socksettos(sk, confgetint("transfer", "ultos"));
    if(transfer->localend != NULL)
	transfer->localend->ignread = 0;
}

void transfersetlocalend(struct transfer *transfer, struct socket *sk)
{
    if(transfer->localend != NULL)
	putsock(transfer->localend);
    getsock(transfer->localend = sk);
    sk->data = transfer;
    sk->readcb = (void (*)(struct socket *, void *))transferread;
    sk->writecb = (void (*)(struct socket *, void *))transferwrite;
    sk->errcb = (void (*)(struct socket *, int, void *))transfererr;
}

static int tryreq(struct transfer *transfer)
{
    struct fnetnode *fn;
    struct fnetpeer *peer;
    
    if((fn = transfer->fn) != NULL)
    {
	if(fn->state != FNN_EST)
	{
	    transfer->close = 1;
	    return(1);
	}
	peer = fnetfindpeer(fn, transfer->peerid);
    } else {
	peer = NULL;
	for(fn = fnetnodes; fn != NULL; fn = fn->next)
	{
	    if((fn->state == FNN_EST) && (fn->fnet == transfer->fnet) && ((peer = fnetfindpeer(fn, transfer->peerid)) != NULL))
		break;
	}
    }
    if(peer != NULL)
	return(fn->fnet->reqconn(peer));
    return(1);
}

void trytransferbypeer(struct fnet *fnet, wchar_t *peerid)
{
    struct transfer *transfer;
    
    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if((transfer->dir == TRNSD_DOWN) && (transfer->state == TRNS_WAITING))
	{
	    if((transfer->fnet == fnet) && !wcscmp(transfer->peerid, peerid))
	    {
		if(!tryreq(transfer))
		    return;
	    }
	}
    }
}

void bumptransfer(struct transfer *transfer)
{
    time_t now;
    
    if((now = time(NULL)) < transfer->timeout)
    {

	if(transfer->etimer == NULL)
	    transfer->etimer = timercallback(transfer->timeout, (void (*)(int, void *))transexpire, transfer);
	return;
    }
    if(transfer->etimer != NULL)
	canceltimer(transfer->etimer);
    switch(transfer->state)
    {
    case TRNS_WAITING:
	transfer->etimer = timercallback(transfer->timeout = (time(NULL) + 30), (void (*)(int, void *))transexpire, transfer);
	if(now - transfer->lastreq > 30)
	    tryreq(transfer);
	break;
    case TRNS_HS:
	if(transfer->dir == TRNSD_UP)
	{
	    if(now - transfer->activity < 60)
		transfer->etimer = timercallback(transfer->timeout = (time(NULL) + 60), (void (*)(int, void *))transexpire, transfer);
	    else
		transfer->close = 1;
	} else if(transfer->dir == TRNSD_DOWN) {
	    if(now - transfer->activity < 60)
		transfer->etimer = timercallback(transfer->timeout = (time(NULL) + 60), (void (*)(int, void *))transexpire, transfer);
	    else
		resettransfer(transfer);
	}
	break;
    case TRNS_MAIN:
	if(transfer->dir == TRNSD_UP)
	{
	    if(now - transfer->activity < 300)
		transfer->etimer = timercallback(transfer->timeout = (time(NULL) + 300), (void (*)(int, void *))transexpire, transfer);
	    else
		transfer->close = 1;
	}
	break;
    }
}

void transfersetactivity(struct transfer *transfer, wchar_t *desc)
{
    time(&transfer->activity);
    if(desc != NULL)
    {
	if(transfer->actdesc != NULL)
	    free(transfer->actdesc);
	transfer->actdesc = swcsdup(desc);
    }
    bumptransfer(transfer);
    CBCHAINDOCB(transfer, trans_act, transfer);
}

void transfersetstate(struct transfer *transfer, int newstate)
{
    transfer->state = newstate;
    if(transfer->etimer != NULL)
	canceltimer(transfer->etimer);
    transfersetactivity(transfer, NULL);
    CBCHAINDOCB(transfer, trans_ac, transfer, L"state");
}

void transfersetnick(struct transfer *transfer, wchar_t *newnick)
{
    if(transfer->peernick != NULL)
	free(transfer->peernick);
    transfer->peernick = swcsdup(newnick);
    CBCHAINDOCB(transfer, trans_ac, transfer, L"nick");
}

void transfersetsize(struct transfer *transfer, int newsize)
{
    transfer->size = newsize;
    CBCHAINDOCB(transfer, trans_ac, transfer, L"size");
}

void transferseterror(struct transfer *transfer, int error)
{
    transfer->error = error;
    CBCHAINDOCB(transfer, trans_ac, transfer, L"error");
}

void transfersetpath(struct transfer *transfer, wchar_t *path)
{
    if(transfer->path != NULL)
	free(transfer->path);
    transfer->path = swcsdup(path);
    CBCHAINDOCB(transfer, trans_ac, transfer, L"path");
}

void transfersethash(struct transfer *transfer, struct hash *hash)
{
    if(transfer->hash != NULL)
	freehash(transfer->hash);
    transfer->hash = hash;
    CBCHAINDOCB(transfer, trans_ac, transfer, L"hash");
}

int slotsleft(void)
{
    struct transfer *transfer;
    int slots;
    
    slots = confgetint("transfer", "slots");
    for(transfer = transfers; (transfer != NULL) && (slots > 0); transfer = transfer->next)
    {
	if((transfer->dir == TRNSD_UP) && (transfer->state == TRNS_MAIN) && !transfer->flags.b.minislot)
	    slots--;
    }
    return(slots);
}

static void killfilter(struct transfer *transfer)
{
    if(transfer->filter != -1)
    {
	kill(-transfer->filter, SIGHUP);
	transfer->filter = -1;
    }
    if(transfer->localend)
    {
	transfer->localend->readcb = NULL;
	transfer->localend->writecb = NULL;
	transfer->localend->errcb = NULL;
	putsock(transfer->localend);
	transfer->localend = NULL;
    }
    if(transfer->filterout)
    {
	transfer->filterout->readcb = NULL;
	putsock(transfer->filterout);
	transfer->filterout = NULL;
    }
    if(transfer->filterbuf)
    {
	free(transfer->filterbuf);
	transfer->filterbuf = NULL;
    }
    transfer->filterbufsize = transfer->filterbufdata = 0;
}

static char *findfilter(struct passwd *pwd)
{
    char *path, *filtername;

    if((path = sprintf2("%s/.dcdl-filter", pwd->pw_dir)) != NULL)
    {
	if(!access(path, X_OK))
	    return(path);
	free(path);
    }
    if((filtername = icwcstombs(confgetstr("transfer", "filter"), NULL)) == NULL)
    {
	flog(LOG_WARNING, "could not convert filter name into local charset: %s", strerror(errno));
    } else {
	if(strchr(filtername, '/') == NULL)
	{
	    if((path = sprintf2("/etc/%s", filtername)) != NULL)
	    {
		if(!access(path, X_OK))
		{
		    free(filtername);
		    return(path);
		}
		free(path);
	    }
	    if((path = sprintf2("/usr/etc/%s", filtername)) != NULL)
	    {
		if(!access(path, X_OK))
		{
		    free(filtername);
		    return(path);
		}
		free(path);
	    }
	    if((path = sprintf2("/usr/local/etc/%s", filtername)) != NULL)
	    {
		if(!access(path, X_OK))
		{
		    free(filtername);
		    return(path);
		}
		free(path);
	    }
	} else {
	    if(!access(filtername, X_OK))
		return(filtername);
	}
	free(filtername);
    }
    return(NULL);
}

static void filterread(struct socket *sk, struct transfer *transfer)
{
    char *buf, *p, *p2;
    size_t bufsize;
    wchar_t *cmd, *arg;
    
    if((buf = sockgetinbuf(sk, &bufsize)) == NULL)
	return;
    bufcat(transfer->filterbuf, buf, bufsize);
    free(buf);
    if((p = memchr(transfer->filterbuf, '\n', transfer->filterbufdata)) != NULL)
    {
	*(p++) = 0;
	if((p2 = strchr(transfer->filterbuf, ' ')) != NULL)
	    *(p2++) = 0;
	if((cmd = icmbstowcs(transfer->filterbuf, NULL)) != NULL)
	{
	    arg = NULL;
	    if(p2 != NULL)
	    {
		if((arg = icmbstowcs(p2, NULL)) == NULL)
		    flog(LOG_WARNING, "filter sent a string which could not be converted into the local charset: %s: %s", transfer->filterbuf, strerror(errno));
	    }
	    CBCHAINDOCB(transfer, trans_filterout, transfer, cmd, arg);
	    if(arg != NULL)
		free(arg);
	    free(cmd);
	} else {
	    flog(LOG_WARNING, "filter sent a string which could not be converted into the local charset: %s: %s", transfer->filterbuf, strerror(errno));
	}
	memmove(transfer->filterbuf, p, transfer->filterbufdata -= (p - transfer->filterbuf));
    }
}

static void filterexit(pid_t pid, int status, void *data)
{
    struct transfer *transfer;
    
    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if(transfer->filter == pid)
	{
	    transfer->filter = -1;
	    killfilter(transfer);
	    if(WEXITSTATUS(status))
	    {
		resettransfer(transfer);
	    } else {
		freetransfer(transfer);
	    }
	    break;
	}
    }
}

int forkfilter(struct transfer *transfer)
{
    char *filtername, *filename, *peerid, *buf;
    wchar_t *wfilename;
    struct passwd *pwent;
    pid_t pid;
    int inpipe, outpipe;
    char **argv;
    size_t argvsize, argvdata;
    struct socket *insock, *outsock;
    struct wcspair *ta;
    char *rec, *val;

    wfilename = transfer->path;
    if(transfer->fnet->filebasename != NULL)
	wfilename = transfer->fnet->filebasename(wfilename);
    if(transfer->auth == NULL)
    {
	flog(LOG_WARNING, "tried to fork filter for transfer with NULL authhandle (tranfer %i)", transfer->id);
	errno = EACCES;
	return(-1);
    }
    if((pwent = getpwuid(transfer->owner)) == NULL)
    {
	flog(LOG_WARNING, "no passwd entry for uid %i (found in transfer %i)", transfer->owner, transfer->id);
	errno = EACCES;
	return(-1);
    }
    if((filtername = findfilter(pwent)) == NULL)
    {
	flog(LOG_WARNING, "could not find filter for user %s", pwent->pw_name);
	errno = ENOENT;
	return(-1);
    }
    if((filename = icwcstombs(wfilename, NULL)) == NULL)
    {
	if((buf = icwcstombs(wfilename, "UTF-8")) == NULL)
	{
	    flog(LOG_WARNING, "could convert transfer filename to neither local charset nor UTF-8: %s", strerror(errno));
	    return(-1);
	}
	filename = sprintf2("utf8-%s", buf);
	free(buf);
    }
    if((peerid = icwcstombs(transfer->peerid, NULL)) == NULL)
    {
	if((buf = icwcstombs(transfer->peerid, "UTF-8")) == NULL)
	{
	    flog(LOG_WARNING, "could convert transfer peerid to neither local charset nor UTF-8: %s", strerror(errno));
	    free(filename);
	    return(-1);
	}
	peerid = sprintf2("utf8-%s", buf);
	free(buf);
    }
    if((pid = forksess(transfer->owner, transfer->auth, filterexit, NULL, FD_PIPE, 0, O_WRONLY, &inpipe, FD_PIPE, 1, O_RDONLY, &outpipe, FD_FILE, 2, O_RDWR, "/dev/null", FD_END)) < 0)
    {
	flog(LOG_WARNING, "could not fork session for filter for transfer %i: %s", transfer->id, strerror(errno));
	return(-1);
    }
    if(pid == 0)
    {
	argv = NULL;
	argvsize = argvdata = 0;
	buf = sprintf2("%i", transfer->size);
	addtobuf(argv, filtername);
	addtobuf(argv, filename);
	addtobuf(argv, buf);
	addtobuf(argv, peerid);
	if(transfer->hash)
	{
	    if((buf = icwcstombs(unparsehash(transfer->hash), NULL)) != NULL)
	    {
		/* XXX: I am very doubtful of this, but it can just as
		 * well be argued that all data should be presented as
		 * key-value pairs. */
		addtobuf(argv, "hash");
		addtobuf(argv, buf);
	    } else {
		flog(LOG_WARNING, "could not convert hash to local charset");
	    }
	}
	for(ta = transfer->args; ta != NULL; ta = ta->next)
	{
	    if((rec = icwcstombs(ta->key, NULL)) == NULL)
		continue;
	    if((val = icwcstombs(ta->val, NULL)) == NULL)
		continue;
	    addtobuf(argv, rec);
	    addtobuf(argv, val);
	}
	addtobuf(argv, NULL);
	execv(filtername, argv);
	flog(LOG_WARNING, "could not exec filter %s: %s", filtername, strerror(errno));
	exit(127);
    }
    insock = wrapsock(inpipe);
    outsock = wrapsock(outpipe);
    /* Really, really strange thing here - sometimes the kernel would
     * return POLLIN on insock, even though it's a write-side
     * pipe. The corresponding read on the pipe naturally returns
     * EBADF, causing doldacond to think there's something wrong with
     * the fd, and thus it closes it. Until I can find out whyever the
     * kernel gives a POLLIN on the fd (if I can at all...), I'll just
     * set ignread on insock for now. */
    insock->ignread = 1;
    transfer->filter = pid;
    transfersetlocalend(transfer, insock);
    getsock(transfer->filterout = outsock);
    outsock->data = transfer;
    outsock->readcb = (void (*)(struct socket *, void *))filterread;
    putsock(insock);
    putsock(outsock);
    free(filtername);
    free(filename);
    free(peerid);
    return(0);
}

static int run(void)
{
    struct transfer *transfer, *next;
    
    for(transfer = transfers; transfer != NULL; transfer = transfer->next)
    {
	if((transfer->endpos >= 0) && (transfer->state == TRNS_MAIN) && (transfer->localend != NULL) && (transfer->localend->state == SOCK_EST) && (transfer->curpos >= transfer->endpos))
	{
	    if((transfer->iface != NULL) && (transfer->iface->endofdata != NULL))
		transfer->iface->endofdata(transfer, transfer->ifacedata);
	    closesock(transfer->localend);
	}
    }
    for(transfer = transfers; transfer != NULL; transfer = next)
    {
	next = transfer->next;
	if(transfer->close)
	{
	    transferdetach(transfer);
	    freetransfer(transfer);
	    continue;
	}
    }
    return(0);
}

static struct configvar myvars[] =
{
    {CONF_VAR_INT, "slots", {.num = 3}},
    {CONF_VAR_INT, "ultos", {.num = SOCK_TOS_MAXTP}},
    {CONF_VAR_INT, "dltos", {.num = SOCK_TOS_MAXTP}},
    {CONF_VAR_STRING, "filter", {.str = L"dc-filter"}},
    {CONF_VAR_END}
};

static struct module me =
{
    .conf =
    {
	.vars = myvars
    },
    .name = "transfer",
    .run = run
};

MODULE(me);
