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
#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <wctype.h>
#include <time.h>
#include <errno.h>
#include <bzlib.h>
#include <zlib.h>
#include <sys/stat.h>

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
#include <tiger.h>

/*
 * The Direct Connect protocol is extremely ugly. Thus, this code must
 * also be a bit ugly in certain places. Please forgive me. =/
 *
 * This also means that there might be some pieces of the code that
 * look completely illogical to you, and you think that you may make
 * them much neater/better. However, in many cases it might just be
 * that it's required to cope with some oddity of the protocol, such
 * as the amazingly ugly delayed key response in the peer protocol.
 */

/* I assume this is the correct character set to use for DC,
 * considering it was developed without i18n support under Windows */
#define DCCHARSET "windows-1252"

#ifdef DCPP_MASQUERADE
/*
 * I honestly don't want to pretend being a client that I'm not, but
 * there are so many hubs that simply do not accept any clients
 * outside their whitelists, for no obvious reasons, so I feel that I
 * am left with little choice. Anyhow, as long as I actually support
 * all the features that my faked DC++ version does, there should be
 * very little harm done.
 */
#define DCIDTAG "++"
#define DCIDTAGV "0.674"
#define DCIDFULL "DC++ 0.674"
#else
#define DCIDTAG "Dolda"
#define DCIDTAGV VERSION
#define DCIDFULL "DoldaConnect " VERSION
#endif

#define PEER_CMD 0
#define PEER_STOP 1
#define PEER_TRNS 2
#define PEER_SYNC 3
#define PEER_TTHL 4

#define CPRS_NONE 0
#define CPRS_ZLIB 1

struct command
{
    char *name;
    void (*handler)(struct socket *sk, void *data, char *cmd, char *args);
    int stop;
    int limit;
};

struct qcommand
{
    struct qcommand *next;
    char *string;
};

struct qcmdqueue
{
    struct qcommand *f, *l;
    int size;
};

struct dchub
{
    struct socket *sk;
    char *inbuf;
    size_t inbufdata, inbufsize;
    struct qcmdqueue queue;
    int extended, dcppemu;
    char *charset;
    char *nativename;
    char *nativenick;
    char **supports;
};

struct dcexppeer
{
    struct dcexppeer *next, *prev;
    char *nick;
    struct fnetnode *fn;
    struct timer *expire;
};

struct dcpeer
{
    struct dcpeer *next, *prev;
    struct socket *sk;
    struct fnetnode *fn;
    char *inbuf;
    size_t inbufdata, inbufsize;
    off_t curread, totalsize;
    int close;
    struct timer *timeout;
    struct qcmdqueue queue;
    struct transfer *transfer;
    int state;
    int ptclose;      /* Close after transfer is complete */
    int accepted;     /* If false, we connected, otherwise, we accepted */
    int extended, dcppemu;
    int direction;    /* Using the constants from transfer.h */
    int compress;
    int hascurpos, fetchingtthl, notthl;
    struct tigertreehash tth;
    char *charset;
    void *cprsdata;
    char *key;
    char *nativename;
    char **supports;
    wchar_t *wcsname;
};

static struct fnet dcnet;
static struct transferiface dctransfer;
static struct socket *udpsock = NULL;
static struct socket *tcpsock = NULL;
static struct dcpeer *peers = NULL;
int numdcpeers = 0;
static struct dcexppeer *expected = NULL;
static char *hmlistname = NULL;
static char *xmllistname = NULL;
static char *xmlbz2listname = NULL;
static struct timer *listwritetimer = NULL;

static void peerconnect(struct socket *sk, int err, struct fnetnode *fn);
static void freedcpeer(struct dcpeer *peer);
static void transread(struct socket *sk, struct dcpeer *peer);
static void transerr(struct socket *sk, int err, struct dcpeer *peer);
static void transwrite(struct socket *sk, struct dcpeer *peer);
static void updatehmlist(void);
static void updatexmllist(void);
static void updatexmlbz2list(void);
static void requestfile(struct dcpeer *peer);
static void updatelists(int now);

static int reservedchar(unsigned char c)
{
    return((c == 0) || (c == 5) || (c == 124) || (c == 96) || (c == 126) || (c == 36));
}

/* Oh, how I despise having to do this... */
static char *dcmakekey(char *lock)
{
    int i, len, offset;
    char *buf, *key;
    char save;
    
    buf = smalloc(strlen(lock));
    save = 5;
    len = 0;
    for(i = 0; lock[i]; i++)
    {
	buf[i] = lock[i] ^ save;
	buf[i] = ((buf[i] & 0x0F) << 4) | ((buf[i] & 0xF0) >> 4);
	save = lock[i];
	if((i != 0) && reservedchar(buf[i]))
	    len += 10;
	else
	    len++;
    }
    buf[0] ^= buf[i - 1];
    if(reservedchar(buf[0]))
	len += 10;
    else
	len++;
    key = smalloc(len + 1);
    offset = 0;
    for(i = 0; lock[i] != 0; i++)
    {
	if(reservedchar(buf[i]))
	    offset += sprintf(key + offset, "/%%DCN%03i%%/", buf[i]);
	else
	    key[offset++] = buf[i];
    }
    key[offset] = 0;
    free(buf);
    return(key);
}

static wchar_t *nmdc2path(char *nmdc, char *charset)
{
    wchar_t *ret, *p;
    
    if((ret = icmbstowcs(nmdc, charset)) == NULL)
	return(NULL);
    for(p = ret; *p != L'\0'; p++) {
	if(*p == L'\\')
	    *p = L'/';
    }
    return(ret);
}

static char *path2nmdc(wchar_t *path, char *charset)
{
    char *ret, *p;
    
    if((ret = icwcstombs(path, charset)) == NULL)
	return(NULL);
    for(p = ret; *p; p++) {
	if(*p == '/')
	    *p = '\\';
    }
    return(ret);
}

static wchar_t *adc2path(char *adc)
{
    return(icmbstowcs(adc, "UTF-8"));
}

static char *path2adc(wchar_t *path)
{
    return(icwcstombs(path, "UTF-8"));
}

static int isdchash(struct hash *hash)
{
    if(wcscmp(hash->algo, L"TTH"))
	return(0);
    if(hash->len != 24)
	return(0);
    return(1);
}

/*
 * Uncomment when used!
 
static int hubsupports(struct dchub *hub, char *cap)
{
    char **p;
    
    if(hub->supports == NULL)
	return(0);
    for(p = hub->supports; *p != NULL; p++)
    {
	if(!strcasecmp(*p, cap))
	    return(1);
    }
    return(0);
}
*/

static int supports(struct dcpeer *peer, char *cap)
{
    char **p;
    
    if(peer->supports == NULL)
	return(0);
    for(p = peer->supports; *p != NULL; p++)
    {
	if(!strcasecmp(*p, cap))
	    return(1);
    }
    return(0);
}

static void endcompress(struct dcpeer *peer)
{
    if(peer->compress == CPRS_ZLIB)
    {
	deflateEnd(peer->cprsdata);
	free(peer->cprsdata);
    }
    peer->compress = CPRS_NONE;
}

static void initcompress(struct dcpeer *peer, int algo)
{
    int ret;
    
    endcompress(peer);
    peer->compress = algo;
    if(algo == CPRS_ZLIB)
    {
	peer->cprsdata = smalloc(sizeof(z_stream));
	memset(peer->cprsdata, 0, sizeof(z_stream));
	if((ret = deflateInit(peer->cprsdata, 3)) != Z_OK)
	{
	    flog(LOG_CRIT, "Aiya! zlib refuses to init (%i)!", ret);
	    abort();
	}
    }
}

static void unquote(wchar_t *in)
{
    wchar_t *p, *p2, nc;
    
    for(p = in; *p != L'\0'; p++)
    {
	if(*p == L'&')
	{
	    for(p2 = p + 1; (*p2 != L'\0') && (*p2 != L';') && (*p2 != L'&'); p2++);
	    if(*p2 == L'&')
		continue;
	    if(*p2 == L'\0')
		return;
	    *p2 = L'\0';
	    nc = L'\0';
	    if(!wcscmp(p + 1, L"amp"))
	    {
		nc = L'&';
	    } else if(p[1] == L'#') {
		nc = ucptowc(wcstol(p + 2, NULL, 10));
	    }
	    if(nc == L'\0')
	    {
		*p2 = L';';
		p = p2;
		continue;
	    }
	    *p = nc;
	    memmove(p + 1, p2 + 1, (wcslen(p2 + 1) + 1) * sizeof(wchar_t));
	}
    }
}

static void freeexppeer(struct dcexppeer *ep)
{
    if(ep->next != NULL)
	ep->next->prev = ep->prev;
    if(ep->prev != NULL)
	ep->prev->next = ep->next;
    if(ep == expected)
	expected = ep->next;
    free(ep->nick);
    putfnetnode(ep->fn);
    if(ep->expire != NULL)
	canceltimer(ep->expire);
    free(ep);
}

static void exppeerexpire(int cancelled, struct dcexppeer *ep)
{
    ep->expire = NULL;
    if(!cancelled)
	freeexppeer(ep);
}

static struct dcexppeer *expectpeer(char *nick, struct fnetnode *fn)
{
    struct dcexppeer *ep;
    
    ep = smalloc(sizeof(*ep));
    ep->nick = sstrdup(nick);
    getfnetnode(ep->fn = fn);
    ep->expire = timercallback(ntime() + 300, (void (*)(int, void *))exppeerexpire, ep);
    ep->next = expected;
    ep->prev = NULL;
    if(expected != NULL)
	expected->prev = ep;
    expected = ep;
    return(ep);
}

static struct qcommand *newqcmd(struct qcmdqueue *queue, char *string)
{
    struct qcommand *new;
    
    new = smalloc(sizeof(*new));
    new->string = sstrdup(string);
    new->next = NULL;
    if(queue->l == NULL)
	queue->f = new;
    else
	queue->l->next = new;
    queue->l = new;
    queue->size++;
    return(new);
}

static struct qcommand *ulqcmd(struct qcmdqueue *queue)
{
    struct qcommand *qcmd;
    
    if((qcmd = queue->f) == NULL)
	return(NULL);
    if((queue->f = qcmd->next) == NULL)
	queue->l = NULL;
    queue->size--;
    return(qcmd);
}

static void freeqcmd(struct qcommand *qcmd)
{
    free(qcmd->string);
    free(qcmd);
}

static void hubrecvchat(struct socket *sk, struct fnetnode *fn, char *from, char *string)
{
    wchar_t *chat, *wfrom, *wpeer;
    char *p, *end;
    struct fnetpeer *peer;
    struct dchub *hub;
    
    hub = fn->data;
    end = string + strlen(string);
    while((p = strchr(string, 13)) != NULL)
	memmove(p, p + 1, (end-- - p));
    if(from != NULL)
    {
	if((strlen(string) > strlen(from) + 2) && (*string == '<') && !memcmp(string + 1, from, strlen(from)) && (*(string + strlen(from) + 1) == '>'))
	    string += strlen(from) + 2;
	if((wfrom = icmbstowcs(from, hub->charset)) == NULL)
	    return;
	wpeer = swcsdup(wfrom);
    } else {
	wfrom = NULL;
	wpeer = NULL;
	if(*string == '<')
	{
	    for(p = string + 1; *p; p++)
	    {
		if((*p == ' ') || (*p == '>'))
		    break;
	    }
	    if(*p == '>')
	    {
		*(p++) = 0;
		if(*p == ' ')
		    p++;
		if((wpeer = icmbstowcs(string + 1, hub->charset)) == NULL)
		    return;
		string = p;
	    }
	}
	if(wpeer == NULL)
	    wpeer = swcsdup(L"");
    }
    if((chat = icmbstowcs(string, hub->charset)) == NULL)
    {
	if(wfrom != NULL)
	    free(wfrom);
	free(wpeer);
	return;
    }
    unquote(chat);
    if(wfrom != NULL)
    {
	if((peer = fnetfindpeer(fn, wfrom)) == NULL) /* Assume public chat */
	    fnethandlechat(fn, 1, wfrom, wpeer, chat);
	else
	    fnethandlechat(fn, 0, wfrom, wpeer, chat);
    } else {
	fnethandlechat(fn, 1, L"", wpeer, chat);
    }
    if(wfrom != NULL)
	free(wfrom);
    free(wpeer);
    free(chat);
}

static void peertimeout(int cancelled, struct dcpeer *peer)
{
    peer->timeout = NULL;
    if(cancelled)
	return;
    freedcpeer(peer);
}

static void sendadc(struct socket *sk, char *arg)
{
    char *buf;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    addtobuf(buf, ' ');
    for(; *arg; arg++)
    {
	if(*arg == ' ')
	{
	    bufcat(buf, "\\s", 2);
	} else if(*arg == '\n') {
	    bufcat(buf, "\\n", 2);
	} else if(*arg == '\\') {
	    bufcat(buf, "\\\\", 2);
	} else {
	    addtobuf(buf, *arg);
	}
    }
    sockqueue(sk, buf, bufdata);
    free(buf);
}

#if defined(__GNUC__)
static void __attribute__ ((format (printf, 2, 3))) sendadcf(struct socket *sk, char *arg, ...) 
#else
static void sendadcf(struct socket *sk, char *arg, ...) 
#endif
{
    char *buf;
    va_list args;
    
    va_start(args, arg);
    buf = vsprintf2(arg, args);
    va_end(args);
    if(buf == NULL)
	return;
    sendadc(sk, buf);
    free(buf);
}

static char **parseadc(char *args)
{
    char **retbuf;
    size_t retbufsize, retbufdata;
    char *buf;
    size_t bufsize, bufdata;
    int state;
    
    retbuf = NULL;
    buf = NULL;
    retbufsize = retbufdata = bufsize = bufdata = 0;
    state = 0;
    while(state != 3)
    {
	switch(state)
	{
	case 0:
	    if(*args == 0)
		state = 3;
	    else if(*args != ' ')
		state = 1;
	    break;
	case 1:
	    if((*args == ' ') || (*args == 0))
	    {
		addtobuf(buf, 0);
		addtobuf(retbuf, buf);
		buf = NULL;
		bufsize = bufdata = 0;
		if(*args == 0)
		    state = 3;
	    } else if(*args == '\\') {
		state = 2;
	    } else {
		addtobuf(buf, *args);
	    }
	    args++;
	    break;
	case 2:
	    if(*args == 0)
	    {
		if(buf != NULL)
		    free(buf);
		addtobuf(retbuf, NULL);
		freeparr(retbuf);
		return(NULL);
	    } else if((*args == 's') || (*args == ' ')) {
		addtobuf(buf, ' ');
	    } else if(*args == 'n') {
		addtobuf(buf, '\n');
	    } else if(*args == '\\') {
		addtobuf(buf, '\\');
	    }
	    args++;
	    state = 1;
	    break;
	}
    }
    if(buf != NULL)
	free(buf);
    addtobuf(retbuf, NULL);
    return(retbuf);
}

/* Macros useful in command handlers */
#define skipspace(s) ({if(((s) = strchr((s), ' ')) == NULL) return; else (s)++;})
#define qstr(sk, str) sockqueue(sk, str, strlen(str))
#define qstrf(sk, strandargs...) \
do { \
    char *__buf__; \
    if((__buf__ = sprintf2(strandargs)) != NULL) { \
        sockqueue(sk, __buf__, strlen(__buf__)); \
        free(__buf__); \
    } \
} while(0)

static char *tr(char *str, char *trans)
{
    char *p;
    
    for(; *trans; trans += 2)
    {
	for(p = strchr(str, trans[0]); p != NULL; p = strchr(p, trans[0]))
	    *p = trans[1];
    }
    return(str);
}

static char *getadcid(struct dcpeer *peer)
{
    char *buf;
    char *ret;
    int isfilelist;
    
    isfilelist = 0;
    if(!wcscmp(peer->transfer->path, L"files.xml") || !wcscmp(peer->transfer->path, L"files.xml.bz2") || !wcscmp(peer->transfer->path, L"MyList.DcLst"))
	isfilelist = 1;
    if(!isfilelist && (peer->transfer->hash != NULL) && isdchash(peer->transfer->hash) && supports(peer, "tthf"))
    {
	buf = base32encode(peer->transfer->hash->buf, 24);
	ret = sprintf2("TTH/%.39s", buf);
	free(buf);
    } else {
	ret = path2adc(peer->transfer->path);
    }
    return(ret);
}


static int trresumecb(struct transfer *transfer, wchar_t *cmd, wchar_t *arg, struct dcpeer *peer)
{
    if(!wcscmp(cmd, L"resume"))
    {
	if(arg == NULL)
	{
	    flog(LOG_WARNING, "filter returned no position for \"resume\" on transfer %i", transfer->id);
	    freedcpeer(peer);
	} else {
	    transfer->curpos = wcstoll(arg, NULL, 10);
	    peer->hascurpos = 1;
	    requestfile(peer);
	}
	return(1);
    }
    return(0);
}

static void sendmynick(struct dcpeer *peer)
{
    struct fnetnode *fn;
    
    fn = peer->fn;
    if(fn == NULL)
	qstrf(peer->sk, "$MyNick %s|", icswcstombs(confgetstr("cli", "defnick"), peer->charset, "DoldaConnectUser-IN"));
    else
	qstrf(peer->sk, "$MyNick %s|", icswcstombs(fn->mynick, peer->charset, "DoldaConnectUser-IN"));
}

static void sendpeerlock(struct dcpeer *peer)
{
    if(peer->dcppemu)
	qstrf(peer->sk, "$Lock EXTENDEDPROTOCOLABCABCABCABCABCABC Pk=DCPLUSPLUS0.674ABCABC|");
    else
	qstrf(peer->sk, "$Lock EXTENDEDPROTOCOLABCABCABCABCABCABC Pk=DOLDA%sABCABCABC|", VERSION);
}

static void sendsupports(struct dcpeer *peer)
{
    if(peer->dcppemu) {
	qstr(peer->sk, "$Supports MiniSlots XmlBZList ADCGet TTHL TTHF GetZBlock ZLIG |");
    } else {
	qstr(peer->sk, "$Supports MiniSlots XmlBZList ADCGet TTHL TTHF");
	if(!confgetint("dc", "hidedeflate"))
	    qstr(peer->sk, " GetZBlock ZLIG");
	qstr(peer->sk, "|");
    }
}

static void requestfile(struct dcpeer *peer)
{
    char *buf;
    
    if(peer->transfer->size == -1)
    {
	/* Use DCCHARSET for $Get paths until further researched... */
	if((buf = path2nmdc(peer->transfer->path, DCCHARSET)) == NULL)
	{
	    transferseterror(peer->transfer, TRNSE_NOTFOUND);
	    peer->close = 1;
	    return;
	}
	/* The transfer will be restarted later from
	 * cmd_filelength when it detects that the sizes
	 * don't match. */
	qstrf(peer->sk, "$Get %s$1|", buf);
	free(buf);
	return;
    }
    if((peer->transfer->hash == NULL) && !peer->notthl)
    {
	if(supports(peer, "adcget") && supports(peer, "tthl"))
	{
	    qstr(peer->sk, "$ADCGET");
	    sendadc(peer->sk, "tthl");
	    if((buf = getadcid(peer)) == NULL)
	    {
		transferseterror(peer->transfer, TRNSE_NOTFOUND);
		peer->close = 1;
		return;
	    }
	    sendadc(peer->sk, buf);
	    free(buf);
	    sendadc(peer->sk, "0");
	    sendadc(peer->sk, "-1");
	    qstr(peer->sk, "|");
	    peer->fetchingtthl = 1;
	    return;
	}
    }
    if(!peer->hascurpos)
    {
	if(forkfilter(peer->transfer))
	{
	    flog(LOG_WARNING, "could not fork filter for transfer %i: %s", peer->transfer->id, strerror(errno));
	    peer->close = 1;
	    return;
	}
	CBREG(peer->transfer, trans_filterout, (int (*)(struct transfer *, wchar_t *, wchar_t *, void *))trresumecb, NULL, peer);
	return;
    }
    if(supports(peer, "adcget"))
    {
	qstr(peer->sk, "$ADCGET");
	sendadc(peer->sk, "file");
	if((buf = getadcid(peer)) == NULL)
	{
	    transferseterror(peer->transfer, TRNSE_NOTFOUND);
	    peer->close = 1;
	    return;
	}
	sendadc(peer->sk, buf);
	free(buf);
	sendadcf(peer->sk, "%ji", (intmax_t)peer->transfer->curpos);
	sendadcf(peer->sk, "%ji", (intmax_t)(peer->transfer->size - peer->transfer->curpos));
	qstr(peer->sk, "|");
    } else if(supports(peer, "xmlbzlist")) {
	if((buf = path2nmdc(peer->transfer->path, "UTF-8")) == NULL)
	{
	    transferseterror(peer->transfer, TRNSE_NOTFOUND);
	    peer->close = 1;
	    return;
	}
	qstrf(peer->sk, "$UGetBlock %ji %ji %s|", (intmax_t)peer->transfer->curpos, (intmax_t)(peer->transfer->size - peer->transfer->curpos), buf);
	free(buf);
    } else {
	/* Use DCCHARSET for $Get paths until further researched... */
	if((buf = path2nmdc(peer->transfer->path, DCCHARSET)) == NULL)
	{
	    transferseterror(peer->transfer, TRNSE_NOTFOUND);
	    peer->close = 1;
	    return;
	}
	qstrf(peer->sk, "$Get %s$%ji|", buf, (intmax_t)peer->transfer->curpos + 1);
	free(buf);
    }
}

static void sendmyinfo(struct socket *sk, struct fnetnode *fn)
{
    struct dchub *hub;
    char *buf;
    struct fnetnode *cfn;
    int hn1, hn2, hn3;
    
    hub = fn->data;
    qstrf(sk, "$MyINFO $ALL %s ", hub->nativenick);
    buf = tr(icswcstombs(confgetstr("dc", "desc"), hub->charset, "Charset_conv_failure"), "$_|_");
    qstrf(sk, "%s", buf);
    hn1 = hn2 = hn3 = 0;
    for(cfn = fnetnodes; cfn != NULL; cfn = cfn->next)
    {
	if((cfn->state == FNN_EST) || (cfn->state == FNN_HS))
	{
	    if(cfn->regstatus == FNNS_OP)
		hn3++;
	    else if(cfn->regstatus == FNNS_REG)
		hn2++;
	    else
		hn1++;
	}
    }
    qstrf(sk, "<%s V:%s,M:%c,H:%i/%i/%i,S:%i>",
	  (hub->dcppemu)?"++":"Dolda",
	  (hub->dcppemu)?"0.674":VERSION,
	  (tcpsock == NULL)?'P':'A',
	  hn1, hn2, hn3,
	  confgetint("transfer", "slots")
	  );
    qstrf(sk, "$ $");
    buf = tr(icswcstombs(confgetstr("dc", "speedstring"), hub->charset, "Charset_conv_failure"), "$_|_");
    qstrf(sk, "%s\x01$", buf);
    buf = tr(icswcstombs(confgetstr("dc", "email"), hub->charset, "Charset_conv_failure"), "$_|_");
    qstrf(sk, "%s$", buf);
    qstrf(sk, "%llu$|", sharesize);
}

static void hubhandleaction(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    
    hub = fn->data;
    if(!strcmp(cmd, "$Lock"))
    {
	qstrf(sk, "$ValidateNick %s|", hub->nativenick);
    } else if(!strcmp(cmd, "$Hello")) {
	if(fn->state == FNN_HS)
	{
	    qstrf(sk, "$Version 1,0091|");
	    qstrf(sk, "$GetNickList|");
	    sendmyinfo(sk, fn);
	    fnetsetstate(fn, FNN_EST);
	} else {
	    qstrf(sk, "$GetINFO %s %s|", args, hub->nativenick);
	}
    }
}

static void cmd_lock(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *key;
    char *p;
    
    hub = fn->data;
    if(!strncmp(args, "EXTENDEDPROTOCOL", 16))
	hub->extended = 1;
    if((p = strchr(args, ' ')) != NULL)
	*(p++) = 0;
    if(hub->extended)
    {
	if(hub->dcppemu) {
	    qstrf(sk, "$Supports UserCommand NoGetINFO NoHello UserIP2 TTHSearch GetZBlock |");
	} else {
	    qstrf(sk, "$Supports UserCommand NoGetINFO NoHello UserIP2 TTHSearch");
	    if(!confgetint("dc", "hidedeflate"))
		qstr(sk, " GetZBlock");
	    qstr(sk, "|");
	}
    }
    key = dcmakekey(args);
    qstrf(sk, "$Key %s|", key);
    free(key);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_hubname(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    wchar_t *buf;
    struct dchub *hub;
    
    hub = fn->data;
    if(hub->nativename == NULL)
	free(hub->nativename);
    hub->nativename = sstrdup(args);
    buf = icmbstowcs(args, hub->charset);
    fnetsetname(fn, (buf == NULL)?L"Hubname conv error":buf);
    if(buf != NULL)
	free(buf);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_hello(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    wchar_t *nick;
    struct dchub *hub;
    
    hub = fn->data;
    if((nick = icmbstowcs(args, hub->charset)) == NULL)
	return;
    if(strcmp(args, hub->nativenick) && (fnetfindpeer(fn, nick) == NULL))
	fnetaddpeer(fn, nick, nick);
    free(nick);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_quit(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    wchar_t *nick;
    struct fnetpeer *peer;
    struct dchub *hub;
    
    hub = fn->data;
    if((nick = icmbstowcs(args, hub->charset)) == NULL)
	return;
    if((peer = fnetfindpeer(fn, nick)) != NULL)
	fnetdelpeer(peer);
    free(nick);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_nicklist(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *p;
    wchar_t *buf;
    struct fnetpeer *peer;
    
    hub = fn->data;
    for(peer = btreeiter(fn->peers); peer != NULL; peer = btreeiter(NULL))
	peer->flags.b.delete = 1;
    while((p = strstr(args, "$$")) != NULL)
    {
	*p = 0;
	if((buf = icmbstowcs(args, hub->charset)) != NULL)
	{
	    if((peer = fnetfindpeer(fn, buf)) == NULL)
		peer = fnetaddpeer(fn, buf, buf);
	    else
		peer->flags.b.delete = 0;
	    free(buf);
	    qstrf(sk, "$GetINFO %s %s|", args, hub->nativenick);
	}
	args = p + 2;
    }
    fnetpeerdm(fn);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_oplist(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *p;
    wchar_t *buf;
    struct fnetpeer *peer;
    
    hub = fn->data;
    for(peer = btreeiter(fn->peers); peer != NULL; peer = btreeiter(NULL))
	peer->flags.b.op = 0;
    while((p = strstr(args, "$$")) != NULL)
    {
	*p = 0;
	if((buf = icmbstowcs(args, hub->charset)) != NULL)
	{
	    if((peer = fnetfindpeer(fn, buf)) != NULL)
		peer->flags.b.op = 1;
	    free(buf);
	}
	args = p + 2;
    }
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_myinfo(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    char *p, *p2;
    wchar_t *buf, *wp, *wp2;
    wchar_t abuf[10];
    struct fnetpeer *peer;
    struct dchub *hub;
    
    hub = fn->data;
    p = args;
    if(strncmp(p, "$ALL ", 5))
	return;
    p += 5;
    if((p2 = strchr(p, ' ')) == NULL)
	return;
    *p2 = 0;
    if((buf = icmbstowcs(p, hub->charset)) == NULL)
	return;
    if((peer = fnetfindpeer(fn, buf)) == NULL)
	peer = fnetaddpeer(fn, buf, buf);
    free(buf);
    p = p2 + 1;
    if((p2 = strstr(p, "$ $")) == NULL)
	return;
    *p2 = 0;
    if((buf = icmbstowcs(p, hub->charset)) == NULL)
	return;
    if((wcslen(buf) > 0) && (buf[wcslen(buf) - 1] == L'>') && ((wp = wcsrchr(buf, L'<')) != NULL))
    {
	buf[wcslen(buf) - 1] = L'\0';
	*(wp++) = L'\0';
	if((wp2 = wcschr(wp, L' ')) != NULL)
	{
	    *(wp2++) = L'\0';
	    fnetpeersetstr(peer, L"dc-client", wp);
	    wp = wp2;
	    do
	    {
		if((wp2 = wcschr(wp, L',')) != NULL)
		    *(wp2++) = L'\0';
		if(wp[1] != L':')
		    continue;
		swprintf(abuf, 10, L"dc-tag-%lc", wp[0]);
		fnetpeersetstr(peer, abuf, wp + 2);
		wp = wp2;
	    } while(wp2 != NULL);
	}
    }
    fnetpeersetstr(peer, L"descr", buf);
    free(buf);
    p = p2 + 3;
    if((p2 = strchr(p, '$')) == NULL)
	return;
    *(p2 - 1) = 0;
    if((buf = icmbstowcs(p, hub->charset)) == NULL)
	return;
    fnetpeersetstr(peer, L"dc-speed", buf);
    free(buf);
    p = p2 + 1;
    if((p2 = strchr(p, '$')) == NULL)
	return;
    *p2 = 0;
    if((buf = icmbstowcs(p, hub->charset)) == NULL)
	return;
    fnetpeersetstr(peer, L"email", buf);
    free(buf);
    p = p2 + 1;
    if(strlen(p) < 1)
	return;
    fnetpeersetlnum(peer, L"share", strtoll(p, NULL, 10));
    hubhandleaction(sk, fn, cmd, args);
}

/* I do not implement the fully in that I do not disconnect from the
 * old hub. I do this since I believe that if the hub owner really
 * wants to get rid of us, then it is his responsibility to disconnect
 * us. */
static void cmd_forcemove(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    struct fnetnode *newfn;
    int freeargs;
    
    hub = fn->data;
    if(strchr(args, ':') == NULL)
    {
	args = strcpy(smalloc(strlen(args) + 5), args);
	strcat(args, ":411");
	freeargs = 1;
    } else {
	freeargs = 0;
    }
    if((newfn = fnetinitconnect(L"dc", fn->owner, args, NULL)) != NULL)
    {
	linkfnetnode(newfn);
	putfnetnode(newfn);
    }
    hubhandleaction(sk, fn, cmd, args);
    if(freeargs)
	free(args);
}

static char *getdcpath(struct sharecache *node, size_t *retlen, char *charset)
{
    char *buf, *buf2;
    size_t len, len2;
    
    if(node->parent == NULL)
	return(NULL);
    if(node->parent == shareroot)
    {
	if((buf = icwcstombs(node->name, charset)) == NULL)
	    return(NULL);
	if(retlen != NULL)
	    *retlen = strlen(buf);
	return(buf);
    } else {
	if((buf2 = icwcstombs(node->name, charset)) == NULL)
	    return(NULL);
	if((buf = getdcpath(node->parent, &len, charset)) == NULL)
	{
	    free(buf2);
	    return(NULL);
	}
	len2 = strlen(buf2);
	buf = srealloc(buf, len + 1 + len2 + 1);
	buf[len++] = '\\';
	strcpy(buf + len, buf2);
	buf[len + len2] = 0;
	free(buf2);
	if(retlen != NULL)
	    *retlen = len + len2;
	return(buf);
    }
}

/*
 * This is the main share searching function for Direct Connect
 * peers. Feel free to optimize it if you feel the need for it. I
 * haven't ever seen it take any noticable CPU time anyway, so I
 * haven't felt that need.
 *
 * It may feel dubious to just return the directory when all terms are
 * satisfied rather than all the files in it, but it really benefits
 * everyone: Less cycles used for us, less UDP squelching for active
 * searchers, and less bandwidth waste for hubs when serving passive
 * searchers.
 */
static void cmd_search(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    int i, done;
    struct dchub *hub;
    char *p, *p2;
    char *prefix, *infix, *postfix, *buf, *buf2;
    struct socket *dsk;
    struct sockaddr_in addr;
    struct sharecache *node;
    int minsize, maxsize;
    int dotth;
    size_t buflen;
    int termnum, satisfied, skipcheck;
    int level, tersat[32];
    wchar_t *terms[32];
    char hashtth[24];
    
    hub = fn->data;
    if((p = strchr(args, ' ')) == NULL)
	return;
    *(p++) = 0;
    
    memset(terms, 0, sizeof(terms));
    prefix = infix = postfix = NULL;
    dsk = NULL;
    dotth = 0;
    
    if(!strncmp(args, "Hub:", 4))
    {
	if(!strcmp(cmd, "$MultiSearch"))
	    goto out;
	if(!strcmp(args + 4, hub->nativenick))
	    goto out;
	prefix = sprintf2("$SR %s ", hub->nativenick);
	infix = sprintf2(" %i/%i\005", slotsleft(), confgetint("transfer", "slots"));
	postfix = sprintf2(" (%s)\005%s|", formataddress(hub->sk->remote, hub->sk->remotelen), args + 4);
	dsk = sk;
	getsock(dsk);
    } else {
	if((p2 = strchr(args, ':')) == NULL)
	    goto out;
	*(p2++) = 0;
	addr.sin_family = AF_INET;
	if(!inet_aton(args, &addr.sin_addr))
	    goto out;
	addr.sin_port = htons(atoi(p2));
	prefix = sprintf2("$SR %s ", hub->nativenick);
	infix = sprintf2(" %i/%i\005", slotsleft(), confgetint("transfer", "slots"));
	postfix = sprintf2(" (%s)|", formataddress(hub->sk->remote, hub->sk->remotelen));
	netdgramconn(dsk = netdupsock(udpsock), (struct sockaddr *)&addr, sizeof(addr));
    }
    
    minsize = maxsize = -1;
    if(*p == 0)
	goto out;
    if(*(p++) == 'T')
	minsize = 0;
    if(*(p++) != '?')
	goto out;
    if(*p == 0)
	goto out;
    if((*(p++) == 'T') && (minsize == 0))
    {
	maxsize = 0;
	minsize = -1;
    }
    if(*(p++) != '?')
	goto out;
    if((p2 = strchr(p, '?')) == NULL)
	goto out;
    *(p2++) = 0;
    if(minsize == 0)
	minsize = atoi(p);
    if(maxsize == 0)
	maxsize = atoi(p);
    p = p2 + 1;
    if(*(p++) != '?')
	goto out;
    termnum = 0;
    p2 = p;
    done = 0;
    while(!done)
    {
	if((*p2 == 0) || (*p2 == '$'))
	{
	    if(*p2 == 0)
		done = 1;
	    else
		*p2 = 0;
	    if(*p)
	    {
		if(!dotth && !strncmp(p, "TTH:", 4))
		{
		    dotth = 1;
		    if(((buf = base32decode(p + 4, &buflen)) == NULL) || (buflen != 24))
		    {
			free(buf);
			goto out;
		    }
		    memcpy(hashtth, buf, 24);
		    free(buf);
		} else {
		    if((terms[termnum] = icmbstowcs(p, hub->charset)) != NULL)
			termnum++;
		}
	    }
	    p = p2 + 1;
	    if(termnum == 32)
		break;
	}
	p2++;
    }
    
    node = shareroot->child;
    level = 0;
    for(i = 0; i < termnum; i++)
	tersat[i] = -1;
    satisfied = 0;
    while(1)
    {
	skipcheck = 0;
	if(node->f.b.type == FILE_REG)
	{
	    if((minsize >= 0) && (node->size < minsize))
		skipcheck = 1;
	    if((maxsize >= 0) && (node->size > maxsize))
		skipcheck = 1;
	}
	if(!skipcheck && dotth)
	{
	    if((node->f.b.type != FILE_REG) || (node->f.b.hastth && memcmp(hashtth, node->hashtth, 24)))
		skipcheck = 1;
	}
	if(!skipcheck)
	{
	    for(i = 0; i < termnum; i++)
	    {
		if(tersat[i] >= 0)
		    continue;
		if(wcsexists(node->name, terms[i]))
		{
		    tersat[i] = level;
		    satisfied++;
		} else if(node->child == NULL) {
		    break;
		}
	    }
	}
	if(!skipcheck && (satisfied == termnum))
	{
	    /* Use DCCHARSET in $Get paths until further researched... */
	    if((buf = getdcpath(node, NULL, DCCHARSET)) != NULL)
	    {
		if(node->f.b.hastth)
		{
		    buf2 = base32encode(node->hashtth, 24);
		    qstrf(dsk, "%s%s\005%ji%sTTH:%.39s%s", prefix, buf, (intmax_t)node->size, infix, buf2, postfix);
		    free(buf2);
		} else {
		    qstrf(dsk, "%s%s\005%ji%s%s%s", prefix, buf, (intmax_t)node->size, infix, hub->nativename, postfix);
		}
		free(buf);
	    }
	}
	if((!skipcheck && (satisfied == termnum)) || (node->child == NULL))
	{
	    while(node->next == NULL)
	    {
		if((node = node->parent) == shareroot)
		    break;
		level--;
	    }
	    if(node == shareroot)
		break;
	    for(i = 0; i < termnum; i++)
	    {
		if(tersat[i] >= level)
		{
		    tersat[i] = -1;
		    satisfied--;
		}
	    }
	    node = node->next;
	} else {
	    node = node->child;
	    level++;
	}
    }

    hubhandleaction(sk, fn, cmd, args);
    
 out:
    if(dsk != NULL)
	putsock(dsk);
    if(prefix != NULL)
	free(prefix);
    if(infix != NULL)
	free(infix);
    if(postfix != NULL)
	free(postfix);
    for(i = 0; (i < 32) && (terms[i] != NULL); i++)
	free(terms[i]);
}

static void cmd_connecttome(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    char *p;
    struct dchub *hub;
    struct socket *newsk;
    struct sockaddr_in addr;
    
    hub = fn->data;
    if((p = strchr(args, ' ')) == NULL)
	return;
    *(p++) = 0;
    if(strcmp(args, hub->nativenick))
	return;
    addr.sin_family = AF_INET;
    args = p;
    if((p = strchr(args, ':')) == NULL)
	return;
    *(p++) = 0;
    addr.sin_port = htons(atoi(p));
    if(!inet_aton(args, &addr.sin_addr))
	return;
    newsk = netcsconn((struct sockaddr *)&addr, sizeof(addr), (void (*)(struct socket *, int, void *))peerconnect, fn);
    getfnetnode(fn);
    hubhandleaction(sk, fn, cmd, args);
}

static void sendctm(struct socket *sk, char *nick)
{
    struct sockaddr *addr;
    socklen_t addrlen;
    
    if(tcpsock == NULL)
	return;
    if(sockgetremotename2(tcpsock, sk, &addr, &addrlen) < 0)
	return;
    if(addr->sa_family == AF_INET)
	qstrf(sk, "$ConnectToMe %s %s|", nick, formataddress(addr, addrlen));
    else
	flog(LOG_WARNING, "Direct Connect TCP socket is suddenly not AF_INET, but %i", addr->sa_family);
    free(addr);
}

static void cmd_revconnecttome(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *p;
    
    hub = fn->data;
    if((p = strchr(args, ' ')) == NULL)
	return;
    *(p++) = 0;
    if(strcmp(p, hub->nativenick))
	return;
    sendctm(sk, args);
    expectpeer(args, fn);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_getnetinfo(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    struct fnetnode *node;
    int numhubs;
    
    hub = fn->data;
    numhubs = 0;
    for(node = fnetnodes; node != NULL; node = node->next)
    {
	if(node->state == FNN_EST)
	    numhubs++;
    }
    qstrf(sk, "$NetInfo %i$%i$%c$0|", confgetint("transfer", "slots"), numhubs, (tcpsock == NULL)?'P':'A');
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_to(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *p, *p2;
    
    hub = fn->data;
    p = args;
    if((p2 = strchr(p, ' ')) == NULL)
	return;
    *(p2++) = 0;
    p = p2;
    if((p2 = strchr(p, ' ')) == NULL)
	return;
    *(p2++) = 0;
    if(strcmp(p, "From:"))
	return;
    p = p2;
    if((p2 = strstr(p, " $")) == NULL)
	return;
    *p2 = 0;
    p2 += 2;
    hubrecvchat(hub->sk, fn, p, p2);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_sr(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    char *p, *p2, *buf;
    char *nick, *filename, *hubname;
    off_t size;
    int slots;
    size_t buflen;
    struct srchres *sr;
    wchar_t *wnick, *wfile;
    
    hub = fn->data;
    nick = p = args;
    if((p2 = strchr(p, ' ')) == NULL)
	return;
    *p2 = 0;
    p = p2 + 1;
    filename = p;
    if((p2 = strchr(p, 5)) == NULL)
	return;
    *p2 = 0;
    p = p2 + 1;
    if((p2 = strchr(p, ' ')) == NULL)
	return;
    *p2 = 0;
    size = strtoll(p, NULL, 10);
    p = p2 + 1;
    if((p2 = strchr(p, '/')) == NULL)
	return;
    *p2 = 0;
    slots = atoi(p);
    p = p2 + 1;
    if((p2 = strchr(p, 5)) == NULL)
	return;
    p = p2 + 1;
    hubname = p;
    if((p2 = strstr(p, " (")) == NULL)
	return;
    *p2 = 0;
    if((wnick = icmbstowcs(nick, hub->charset)) == NULL)
	return;
    /* Use DCCHARSET in $Get paths until further researched... */
    if((wfile = nmdc2path(filename, DCCHARSET)) == NULL)
    {
	free(wnick);
	return;
    }
    sr = newsrchres(&dcnet, wfile, wnick);
    if(sr->peernick != NULL)
	free(sr->peernick);
    sr->peernick = swcsdup(wnick);
    sr->size = size;
    sr->slots = slots;
    free(wfile);
    free(wnick);
    if(!strncmp(hubname, "TTH:", 4))
    {
	if((buf = base32decode(hubname + 4, &buflen)) != NULL)
	{
	    if(buflen == 24)
		sr->hash = newhash(L"TTH", 24, buf);
	    free(buf);
	}
    }
    getfnetnode(sr->fn = fn);
    submitsrchres(sr);
    freesrchres(sr);
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_usercommand(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    /* Do nothing for now. */
}

static void cmd_getpass(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    wchar_t *pw;
    char *mbspw;
    
    hub = fn->data;
    pw = wpfind(fn->args, L"password");
    if((pw == NULL) || ((mbspw = icwcstombs(pw, hub->charset)) == NULL))
    {
	killfnetnode(fn);
	return;
    }
    qstrf(sk, "$MyPass %s|", mbspw);
    free(mbspw);
    fn->regstatus = FNNS_REG;
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_logedin(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    
    hub = fn->data;
    fn->regstatus = FNNS_OP;
    hubhandleaction(sk, fn, cmd, args);
}

static void cmd_hubsupports(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    int i;
    char *p, *p2;
    char **arr;
    size_t arrsize, arrdata;
    
    hub = fn->data;
    if(hub->supports != NULL)
    {
	for(i = 0; hub->supports[i] != NULL; i++)
	    free(hub->supports[i]);
	free(hub->supports);
    }
    arr = NULL;
    arrsize = arrdata = 0;
    p = args;
    do
    {
	if((p2 = strchr(p, ' ')) != NULL)
	    *(p2++) = 0;
	if(*p == 0)
	    continue;
	addtobuf(arr, sstrdup(p));
    } while((p = p2) != NULL);
    addtobuf(arr, NULL);
    hub->supports = arr;
}

static void cmd_mynick(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    struct dcexppeer *expect;
    struct dchub *hub;

    if(peer->nativename != NULL)
	free(peer->nativename);
    peer->nativename = sstrdup(args);
    if(peer->wcsname != NULL)
	free(peer->wcsname);
    if((peer->wcsname = icmbstowcs(peer->nativename, peer->charset)) == NULL)
    {
	peer->close = 1;
	return;
    }
    if(peer->accepted)
    {
	for(expect = expected; expect != NULL; expect = expect->next)
	{
	    if(!strcmp(expect->nick, args))
		break;
	}
	if(expect == NULL)
	{
	    peer->fn = NULL;
	} else {
	    hub = expect->fn->data;
	    peer->fn = expect->fn;
	    getfnetnode(peer->fn);
	    peer->dcppemu = hub->dcppemu;
	    freeexppeer(expect);
	}
    }
}

static void cmd_direction(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    char *p;
    int mydir;
    struct transfer *transfer;
    
    if((p = strchr(args, ' ')) == NULL)
	return;
    *p = 0;
    if(!strcmp(args, "Upload"))
	mydir = TRNSD_DOWN;
    if(!strcmp(args, "Download"))
	mydir = TRNSD_UP;
    if(peer->accepted)
    {
	if((peer->transfer == NULL) || (mydir != peer->direction))
	{
	    peer->close = 1;
	    return;
	}
	if(peer->direction == TRNSD_DOWN)
	    requestfile(peer);
    } else {
	if(peer->wcsname == NULL)
	{
	    peer->close = 1;
	    return;
	}
	peer->direction = mydir;
	if(peer->direction == TRNSD_UP)
	{
	    if(confgetint("transfer", "ulquota") && hasupload(&dcnet, peer->wcsname))
	    {
		peer->close = 1;
		return;
	    }
	    transfer = newupload(peer->fn, &dcnet, peer->wcsname, &dctransfer, peer);
	} else {
	    if((transfer = finddownload(peer->wcsname)) == NULL)
	    {
		peer->close = 1;
		return;
	    }
	    transferattach(transfer, &dctransfer, peer);
	    transfersetstate(transfer, TRNS_HS);
	}
	transfersetnick(transfer, peer->wcsname);
	peer->transfer = transfer;
	if(peer->extended)
	    sendsupports(peer);
	qstrf(sk, "$Direction %s %i|", (peer->direction == TRNSD_UP)?"Upload":"Download", rand() % 10000);
	if(peer->key != NULL)
	    qstrf(sk, "$Key %s|", peer->key);
	if(peer->direction == TRNSD_DOWN)
	    requestfile(peer);
    }
}

static void cmd_peerlock(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    char *p, *key;
    struct transfer *transfer;
    
    if((p = strchr(args, ' ')) == NULL)
	return;
    *p = 0;
    if(!strncmp(args, "EXTENDEDPROTOCOL", 16))
	peer->extended = 1;
    key = dcmakekey(args);
    if(peer->accepted)
    {
	if(peer->wcsname == NULL)
	{
	    peer->close = 1;
	    return;
	}
	sendmynick(peer);
	sendpeerlock(peer);
	if(peer->extended)
	    sendsupports(peer);
	if((transfer = finddownload(peer->wcsname)) == NULL)
	{
	    if(confgetint("transfer", "ulquota") && hasupload(&dcnet, peer->wcsname))
	    {
		peer->close = 1;
		return;
	    }
	    peer->direction = TRNSD_UP;
	    transfer = newupload(peer->fn, &dcnet, peer->wcsname, &dctransfer, peer);
	} else {
	    peer->direction = TRNSD_DOWN;
	    transferattach(transfer, &dctransfer, peer);
	    transfersetstate(transfer, TRNS_HS);
	}
	transfersetnick(transfer, peer->wcsname);
	peer->transfer = transfer;
	qstrf(sk, "$Direction %s %i|", (peer->direction == TRNSD_UP)?"Upload":"Download", rand() % 10000);
	qstrf(sk, "$Key %s|", key);
	free(key);
    } else {
	if(peer->key != NULL)
	    free(peer->key);
	peer->key = key;
    }
}

static void cmd_key(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    /* NOP */
}

static void startdl(struct dcpeer *peer)
{
    if(peer->timeout != NULL)
	canceltimer(peer->timeout);
    peer->state = PEER_TRNS;
    transferstartdl(peer->transfer, peer->sk);
    peer->sk->readcb = (void (*)(struct socket *, void *))transread;
    peer->sk->errcb = (void (*)(struct socket *, int, void *))transerr;
}

static void startul(struct dcpeer *peer)
{
    if(peer->timeout != NULL)
	canceltimer(peer->timeout);
    peer->state = PEER_TRNS;
    transferstartul(peer->transfer, peer->sk);
    peer->sk->writecb = (void (*)(struct socket *, void *))transwrite;
}

static void cmd_filelength(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    off_t size;
    struct transfer *transfer;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    size = strtoll(args, NULL, 10);
    if(peer->transfer->size != size)
    {
	transfersetsize(peer->transfer, size);
	transfer = peer->transfer;
	peer->close = 1;
	resettransfer(transfer);
	trytransferbypeer(transfer->fnet, transfer->peerid);
	return;
    }
    startdl(peer);
    qstr(peer->sk, "$Send|");
}

static void cmd_error(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    if(peer->fetchingtthl)
    {
	peer->fetchingtthl = 0;
	peer->notthl = 1;
	requestfile(peer);
	return;
    }
    if((peer->transfer != NULL) && (peer->transfer->dir == TRNSD_DOWN))
    {
	transferseterror(peer->transfer, TRNSE_NOTFOUND);
	resettransfer(peer->transfer);
	return;
    }
    peer->close = 1;
}

static void cmd_maxedout(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    if((peer->transfer != NULL) && (peer->transfer->dir == TRNSD_DOWN))
    {
	transferseterror(peer->transfer, TRNSE_NOSLOTS);
	resettransfer(peer->transfer);
	return;
    }
    peer->close = 1;
}

static struct
{
    char *name;
    char **file;
    void (*func)(void);
} lists[] =
{
    {"MyList.DcLst", &hmlistname, updatehmlist},
    {"files.xml", &xmllistname, updatexmllist},
    {"files.xml.bz2", &xmlbz2listname, updatexmlbz2list},
    {NULL, NULL}
};

static int openfilelist(char *name)
{
    int i, fd;
    int errnobak;
    
    for(i = 0; lists[i].name != NULL; i++)
    {
	if(!strcmp(name, lists[i].name))
	    break;
    }
    errno = 0;
    if(lists[i].name == NULL)
	return(-1);
    fd = -1;
    if((*lists[i].file == NULL) || ((fd = open(*lists[i].file, O_RDONLY)) < 0))
	lists[i].func();
    if((fd < 0) && ((*lists[i].file == NULL) || ((fd = open(*lists[i].file, O_RDONLY)) < 0)))
    {
	errnobak = errno;
	flog(LOG_ERR, "could not open filelist tempfile: %s", strerror(errno));
	errno = errnobak;
	return(-1);
    }
    return(fd);
}

static struct sharecache *findbytth(char *tth32)
{
    char *buf;
    size_t buflen;
    struct sharecache *node;
    
    if((buf = base32decode(tth32, &buflen)) == NULL)
	return(NULL);
    if(buflen != 24)
    {
	free(buf);
	return(NULL);
    }
    for(node = shareroot->child; node != NULL; node = nextscnode(node))
    {
	if(node->f.b.hastth && !memcmp(node->hashtth, buf, 24))
	    break;
    }
    free(buf);
    return(node);
}

static struct sharecache *resdcpath(char *path, char *charset, char sep)
{
    struct sharecache *node;
    char *p, *p2;

    node = shareroot;
    p = path;
    while(p != NULL)
    {
	if((p2 = strchr(p, sep)) != NULL)
	    *(p2++) = 0;
	if((node = findcache(node, icsmbstowcs(p, charset, L""))) == NULL)
	    return(NULL);
	p = p2;
    }
    return(node);
}

static void cmd_get(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    off_t offset;
    char *p, *buf;
    wchar_t *buf2;
    struct sharecache *node;
    struct socket *lesk;
    int fd;
    struct stat sb;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    if((p = strchr(args, '$')) == NULL)
    {
	peer->close = 1;
	return;
    }
    *(p++) = 0;
    if((offset = (strtoll(p, NULL, 10) - 1)) < 0)
    {
	peer->close = 1;
	return;
    }
    if(((fd = openfilelist(args)) < 0) && (errno != 0))
    {
	qstr(sk, "$Error Could not send file list|");
	peer->close = 1;
	return;
    } else if(fd >= 0) {
	if((buf2 = nmdc2path(args, DCCHARSET)) != NULL) {
	    transfersetpath(peer->transfer, buf2);
	    free(buf2);
	}
	peer->transfer->flags.b.minislot = 1;
    }
    if(fd < 0)
    {
	/* Use DCCHARSET in $Get paths until further researched... */
	if((node = resdcpath(args, DCCHARSET, '\\')) == NULL)
	{
	    qstrf(sk, "$Error File not in share|");
	    peer->close = 1;
	    return;
	}
	if((fd = opensharecache(node)) < 0)
	{
	    qstrf(sk, "$Error %s|", strerror(errno));
	    peer->close = 1;
	    return;
	}
	buf = getfspath(node);
	if((buf2 = icsmbstowcs(buf, NULL, NULL)) == NULL)
	    flog(LOG_WARNING, "could not convert native path into a wcs (%s): %s", buf, strerror(errno));
	else
	    transfersetpath(peer->transfer, buf2);
	free(buf);
    }
    if(fstat(fd, &sb) < 0)
    {
	close(fd);
	flog(LOG_WARNING, "could not stat file %ls: %s", node->name, strerror(errno));
	qstrf(sk, "$Error|");
	peer->close = 1;
	return;
    }
    if(sb.st_size < 65536)
	peer->transfer->flags.b.minislot = 1;
    if(!peer->transfer->flags.b.minislot && (slotsleft() < 1)) {
	close(fd);
	qstr(sk, "$MaxedOut|");
	peer->close = 1;
	return;
    }
    if((offset != 0) && (lseek(fd, offset, SEEK_SET) < 0))
    {
	close(fd);
	qstrf(sk, "$Error Offset out of range|");
	peer->close = 1;
	return;
    }
    lesk = wrapsock(fd);
    transferprepul(peer->transfer, sb.st_size, offset, -1, lesk);
    putsock(lesk);
    qstrf(sk, "$FileLength %ji|", (intmax_t)peer->transfer->size);
}

static void cmd_send(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    if(peer->transfer->localend == NULL)
    {
	peer->close = 1;
	return;
    }
    peer->ptclose = 1;
    startul(peer);
}

static void cmd_supports(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    int i;
    char *p, *p2;
    char **arr;
    size_t arrsize, arrdata;
    
    if(peer->supports != NULL)
    {
	for(i = 0; peer->supports[i] != NULL; i++)
	    free(peer->supports[i]);
	free(peer->supports);
    }
    arr = NULL;
    arrsize = arrdata = 0;
    p = args;
    do
    {
	if((p2 = strchr(p, ' ')) != NULL)
	    *(p2++) = 0;
	if(*p == 0)
	    continue;
	addtobuf(arr, sstrdup(p));
    } while((p = p2) != NULL);
    addtobuf(arr, NULL);
    peer->supports = arr;
}

static void cmd_getblock(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    int fd;
    char *p, *p2;
    off_t start, numbytes;
    char *charset, *buf;
    wchar_t *buf2;
    struct sharecache *node;
    struct stat sb;
    struct socket *lesk;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    p = args;
    if((p2 = strchr(p, ' ')) == NULL)
    {
	peer->close = 1;
	return;
    }
    *(p2++) = 0;
    start = strtoll(p, NULL, 10);
    p = p2;
    if((p2 = strchr(p, ' ')) == NULL)
    {
	peer->close = 1;
	return;
    }
    *(p2++) = 0;
    numbytes = strtoll(p, NULL, 10);
    p = p2;
    if(!strcmp(cmd, "$UGetBlock") || !strcmp(cmd, "$UGetZBlock"))
	charset = "UTF-8";
    else
	/* Use DCCHARSET in $Get paths until further researched... */
	charset = DCCHARSET;
    if(!strcmp(cmd, "$GetZBlock") || !strcmp(cmd, "$UGetZBlock"))
	initcompress(peer, CPRS_ZLIB);
    if(((fd = openfilelist(p)) < 0) && (errno != 0))
    {
	qstr(sk, "$Error Could not send file list|");
	return;
    } else if(fd >= 0) {
	if((buf2 = nmdc2path(args, charset)) != NULL) {
	    transfersetpath(peer->transfer, buf2);
	    free(buf2);
	}
	peer->transfer->flags.b.minislot = 1;
    }
    if(fd < 0)
    {
	if((node = resdcpath(p, charset, '\\')) == NULL)
	{
	    qstr(sk, "$Error File not in cache|");
	    return;
	}
	if((fd = opensharecache(node)) < 0)
	{
	    qstrf(sk, "$Error %s|", strerror(errno));
	    return;
	}
	buf = getfspath(node);
	if((buf2 = icsmbstowcs(buf, NULL, NULL)) == NULL)
	    flog(LOG_WARNING, "could not convert native path into a wcs (%s): %s", buf, strerror(errno));
	else
	    transfersetpath(peer->transfer, buf2);
	free(buf);
    }
    if(fstat(fd, &sb) < 0)
    {
	close(fd);
	flog(LOG_WARNING, "could not stat file %ls: %s", node->name, strerror(errno));
	qstr(sk, "$Error|");
	return;
    }
    if(sb.st_size < 65536)
	peer->transfer->flags.b.minislot = 1;
    if(!peer->transfer->flags.b.minislot && (slotsleft() < 1)) {
	close(fd);
	qstr(sk, "$MaxedOut|");
	return;
    }
    if((start != 0) && ((start >= sb.st_size) || (lseek(fd, start, SEEK_SET) < 0)))
    {
	close(fd);
	qstr(sk, "$Error Offset out of range|");
	return;
    }
    if((numbytes < 0) || (start + numbytes > sb.st_size))
	numbytes = sb.st_size - start;
    lesk = wrapsock(fd);
    transferprepul(peer->transfer, sb.st_size, start, start + numbytes, lesk);
    putsock(lesk);
    qstrf(sk, "$Sending %ji|", (intmax_t)numbytes);
    startul(peer);
}

static void cmd_adcget(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    int i;
    char **argv, *buf;
    off_t start, numbytes;
    struct sharecache *node;
    struct stat sb;
    struct socket *lesk;
    wchar_t *wbuf;
    int fd;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    if((argv = parseadc(args)) == NULL)
    {
	peer->close = 1;
	return;
    }
    if(parrlen(argv) < 4)
    {
	peer->close = 1;
	goto out;
    }
    start = strtoll(argv[2], NULL, 10);
    numbytes = strtoll(argv[3], NULL, 10);
    node = NULL;
    fd = -1;
    if(((fd = openfilelist(argv[1])) < 0) && (errno != 0))
    {
	qstr(sk, "$Error Could not send file list|");
	goto out;
    } else if(fd >= 0) {
	if((wbuf = adc2path(argv[1])) != NULL)
	    transfersetpath(peer->transfer, wbuf);
	peer->transfer->flags.b.minislot = 1;
    }
    if(fd < 0)
    {
	if(!strncmp(argv[1], "TTH/", 4))
	{
	    if((node = findbytth(argv[1] + 4)) == NULL)
	    {
		qstr(sk, "$Error File not in cache|");
		goto out;
	    }
	} else {
	    if((node = resdcpath((argv[1][0] == '/')?(argv[1] + 1):(argv[1]), "UTF-8", '/')) == NULL)
	    {
		qstr(sk, "$Error File not in cache|");
		goto out;
	    }
	}
	if((fd = opensharecache(node)) < 0)
	{
	    qstrf(sk, "$Error %s|", strerror(errno));
	    goto out;
	}
	buf = getfspath(node);
	if((wbuf = icsmbstowcs(buf, NULL, NULL)) == NULL)
	    flog(LOG_WARNING, "could not convert native path into a wcs (%s): %s", buf, strerror(errno));
	else
	    transfersetpath(peer->transfer, wbuf);
	free(buf);
    }
    if(!strcmp(argv[0], "file"))
    {
	for(i = 4; argv[i] != NULL; i++)
	{
	    if(!strcmp(argv[i], "ZL1"))
		initcompress(peer, CPRS_ZLIB);
	}
	if(fstat(fd, &sb) < 0)
	{
	    flog(LOG_WARNING, "could not stat file %ls: %s", node->name, strerror(errno));
	    qstr(sk, "$Error|");
	    goto out;
	}
	if(sb.st_size < 65536)
	    peer->transfer->flags.b.minislot = 1;
	if(!peer->transfer->flags.b.minislot && (slotsleft() < 1)) {
	    qstr(sk, "$MaxedOut|");
	    goto out;
	}
	if((start != 0) && ((start >= sb.st_size) || (lseek(fd, start, SEEK_SET) < 0)))
	{
	    qstr(sk, "$Error Offset out of range|");
	    goto out;
	}
	if((numbytes < 0) || (start + numbytes > sb.st_size))
	    numbytes = sb.st_size - start;
	lesk = wrapsock(fd);
	transferprepul(peer->transfer, sb.st_size, start, start + numbytes, lesk);
	putsock(lesk);
	fd = -1;
	qstr(sk, "$ADCSND");
	sendadc(sk, "file");
	sendadc(sk, argv[1]);
	sendadcf(sk, "%ji", (intmax_t)start);
	sendadcf(sk, "%ji", (intmax_t)numbytes);
	if(peer->compress == CPRS_ZLIB)
	    sendadc(sk, "ZL1");
	qstr(sk, "|");
	startul(peer);
    } else if(!strcmp(argv[0], "tthl")) {
	/*
	 * XXX: Implement full TTHL support.
	 *
	 * In the meantime, this is better than nothing, since it at
	 * least allows fetching of the TTH root if it isn't already
	 * known.
	 */
	if(node == NULL)
	{
	    qstr(sk, "$Error no TTHL data for virtual files|");
	    goto out;
	}
	qstr(sk, "$ADCSND");
	sendadc(sk, "tthl");
	sendadc(sk, argv[1]);
	sendadc(sk, "0");
	sendadc(sk, "24");
	qstr(sk, "|");
	sockqueue(sk, node->hashtth, 24);
    } else {
	qstr(sk, "$Error Namespace not implemented|");
	goto out;
    }
    
 out:
    if(fd >= 0)
	close(fd);
    freeparr(argv);
}

static void handletthl(struct dcpeer *peer)
{
    char buf[24];
    
    while(peer->inbufdata >= 24)
    {
	pushtigertree(&peer->tth, peer->inbuf);
	memmove(peer->inbuf, peer->inbuf + 24, peer->inbufdata -= 24);
	peer->curread += 24;
    }
    if(peer->curread >= peer->totalsize)
    {
	if(peer->timeout == NULL)
	    peer->timeout = timercallback(ntime() + 180, (void (*)(int, void *))peertimeout, peer);
	peer->state = PEER_CMD;
	synctigertree(&peer->tth);
	restigertree(&peer->tth, buf);
	transfersethash(peer->transfer, newhash(L"TTH", 24, buf));
	requestfile(peer);
    }
}

static void cmd_adcsnd(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    char **argv;
    off_t start, numbytes;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    if((argv = parseadc(args)) == NULL)
    {
	peer->close = 1;
	return;
    }
    if(parrlen(argv) < 4)
    {
	peer->close = 1;
	goto out;
    }
    start = strtoll(argv[2], NULL, 10);
    numbytes = strtoll(argv[3], NULL, 10);
    if(!strcmp(argv[0], "tthl"))
    {
	if((start != 0) || (numbytes % 24 != 0))
	{
	    /* Weird. Bail out. */
	    peer->close = 1;
	    goto out;
	}
	if(peer->timeout != NULL)
	    canceltimer(peer->timeout);
	peer->state = PEER_TTHL;
	peer->totalsize = numbytes;
	peer->curread = 0;
	peer->fetchingtthl = 0;
	inittigertree(&peer->tth);
	handletthl(peer);
    } else if(!strcmp(argv[0], "file")) {
	if(start != peer->transfer->curpos)
	{
	    peer->close = 1;
	    goto out;
	}
	if(start + numbytes != peer->transfer->size)
	{
	    transfersetsize(peer->transfer, start + numbytes);
	    peer->close = 1;
	    goto out;
	}
	startdl(peer);
	if(peer->inbufdata > 0)
	{
	    sockpushdata(sk, peer->inbuf, peer->inbufdata);
	    peer->inbufdata = 0;
	    transread(sk, peer);
	}
    } else {
	/* We certainly didn't request this...*/
	peer->close = 1;
	goto out;
    }
    
 out:
    freeparr(argv);
}

static void cmd_sending(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
    off_t numbytes;
    
    if(peer->transfer == NULL)
    {
	peer->close = 1;
	return;
    }
    numbytes = strtoll(args, NULL, 10);
    if(peer->transfer->size - peer->transfer->curpos != numbytes)
    {
	transfersetsize(peer->transfer, peer->transfer->curpos + numbytes);
	peer->close = 1;
	return;
    }
    startdl(peer);
    if(peer->inbufdata > 0)
    {
	sockpushdata(sk, peer->inbuf, peer->inbufdata);
	peer->inbufdata = 0;
	transread(sk, peer);
    }
}

/*
Hub skeleton:
static void cmd_(struct socket *sk, struct fnetnode *fn, char *cmd, char *args)
{
    struct dchub *hub;
    
    hub = fn->data;
    hubhandleaction(sk, fn, cmd, args);
}

Peer skeleton:
static void cmd_(struct socket *sk, struct dcpeer *peer, char *cmd, char *args)
{
}

*/

static int hubreqconn(struct fnetpeer *peer)
{
    struct dchub *hub;
    char *mbsnick;
    
    if((peer->fn->state != FNN_EST) || (peer->fn->fnet != &dcnet))
    {
	errno = EINVAL;
	return(1);
    }
    if((hub = peer->fn->data) == NULL)
    {
	errno = EFAULT;
	return(1);
    }
    if((mbsnick = icwcstombs(peer->id, hub->charset)) == NULL)
	return(1); /* Shouldn't happen, of course, but who knows... */
    if(tcpsock != NULL)
    {
	sendctm(hub->sk, mbsnick);
	expectpeer(mbsnick, peer->fn);
    } else {
	qstrf(hub->sk, "$RevConnectToMe %s %s|", hub->nativenick, mbsnick);
    }
    free(mbsnick);
    return(0);
}

static int hubsendchat(struct fnetnode *fn, int public, wchar_t *to, wchar_t *string)
{
    struct dchub *hub;
    char *mbsstring, *mbsto;
    
    hub = fn->data;
    if((mbsto = icwcstombs(to, hub->charset)) == NULL)
    {
	errno = EILSEQ;
	return(1);
    }
    if((mbsstring = icwcstombs(string, hub->charset)) == NULL)
    {
	errno = EILSEQ;
	return(1);
    }
    if((strchr(mbsto, '|') != NULL) || (strchr(mbsto, ' ') != NULL))
    {
	free(mbsto);
	free(mbsstring);
	errno = ESRCH;
	return(1);
    }
    if(strchr(mbsstring, '|') != NULL)
    {
	free(mbsto);
	free(mbsstring);
	errno = EILSEQ;
	return(1);
    }
    if(public)
    {
	if(*to == L'\0')
	{
	    qstrf(hub->sk, "<%s> %s|", hub->nativenick, mbsstring);
	} else {
	    qstrf(hub->sk, "$To: %s From: %s $<%s> %s|", mbsto, hub->nativenick, hub->nativenick, mbsstring);
	}
    } else {
	qstrf(hub->sk, "$To: %s From: %s $<%s> %s|", mbsto, hub->nativenick, hub->nativenick, mbsstring);
    }
    free(mbsto);
    free(mbsstring);
    return(0);
}

static void findsizelimit(struct sexpr *sexpr, int *min, int *max)
{
    int minl, maxl, minr, maxr, retmin, retmax;
    
    switch(sexpr->op)
    {
    case SOP_AND:
	findsizelimit(sexpr->l, &minl, &maxl);
	findsizelimit(sexpr->r, &minr, &maxr);
	retmin = (minl > minr)?minl:minr;
	if((maxl != -1) && (maxr != -1))
	    retmax = (maxl < maxr)?maxl:maxr;
	else if(maxl != -1)
	    retmax = maxl;
	else if(maxr != -1)
	    retmax = maxr;
	else
	    retmax = -1;
	break;
    case SOP_OR:
	findsizelimit(sexpr->l, &minl, &maxl);
	findsizelimit(sexpr->r, &minr, &maxr);
	retmin = (minl < minr)?minl:minr;
	if((maxl == -1) || (maxr == -1))
	    retmax = -1;
	else
	    retmax = (maxl > maxr)?maxl:maxr;
	break;
    case SOP_NOT:
	findsizelimit(sexpr->l, &minl, &maxl);
	if((minl == 0) && (maxl == -1)) /* Interval is unspecified */
	{
	    retmin = 0;
	    retmax = -1;
	} else if((minl == 0) && (maxl != -1)) {
	    retmin = maxl + 1;
	    retmax = -1;
	} else if((minl != 0) && (maxl == -1)) {
	    retmin = 0;
	    retmax = minl - 1;
	} else { /* This would yield two seperate intervals, which DC cannot handle */
	    retmin = 0;
	    retmax = -1;
	}
    case SOP_SIZELT:
	retmin = 0;
	retmax = sexpr->d.n - 1;
	break;
    case SOP_SIZEEQ:
	retmin = sexpr->d.n;
	retmax = sexpr->d.n;
	break;
    case SOP_SIZEGT:
	retmin = sexpr->d.n + 1;
	retmax = -1;
	break;
    default:
	retmin = 0;
	retmax = -1;
	break;
    }
    if(min != NULL)
	*min = retmin;
    if(max != NULL)
	*max = retmax;
}

static struct hash *findsehash(struct sexpr *sexpr)
{
    struct hash *h1, *h2;
    
    switch(sexpr->op)
    {
    case SOP_AND:
	if((h1 = findsehash(sexpr->l)) != NULL)
	    return(h1);
	if((h1 = findsehash(sexpr->r)) != NULL)
	    return(h1);
	break;
    case SOP_OR:
	if((h1 = findsehash(sexpr->l)) == NULL)
	    return(NULL);
	if((h2 = findsehash(sexpr->r)) == NULL)
	    return(NULL);
	if(hashcmp(h1, h2))
	    return(h1);
	break;
    case SOP_HASHIS:
	if(!wcscmp(sexpr->d.hash->algo, L"TTH"))
	    return(sexpr->d.hash);
    default:
	break;
    }
    return(NULL);
}

static int hubsearch(struct fnetnode *fn, struct search *srch, struct srchfnnlist *ln)
{
    struct dchub *hub;
    struct wcslist *list, *cur;
    char *sstr, *buf, *p;
    size_t sstrsize, sstrdata;
    struct sockaddr *name;
    socklen_t namelen;
    int minsize, maxsize;
    struct hash *hash;
    
    hub = fn->data;
    if((fn->state != FNN_EST) || (hub->sk == NULL) || (hub->sk->state != SOCK_EST))
	return(1);
    list = findsexprstrs(srch->sexpr);
    findsizelimit(srch->sexpr, &minsize, &maxsize);
    hash = findsehash(srch->sexpr);
    if((minsize != 0) && (maxsize != -1))
    {
	/* Choose either minsize or maxsize by trying to determine
	 * which choice will be most restrictive. The result can be
	 * approximative at best anyway, so don't try too hard... */
	if((50000000 - maxsize) > (minsize - 50000000))
	    minsize = 0;
	else
	    maxsize = -1;
    }
    sstr = NULL;
    sstrsize = sstrdata = 0;
    if((hash != NULL) && (hash->len == 24))
    {
	/* Prioritize hash searches above all else */
	bufcat(sstr, "F?T?0?9?TTH:", 12);
	buf = base32encode(hash->buf, hash->len);
	
	bufcat(sstr, buf, 39);
	free(buf);
    } else {
	if(minsize != 0)
	{
	    sizebuf2(sstr, sstrdata + 32, 1);
	    sstrdata += snprintf(sstr + sstrdata, sstrsize - sstrdata, "T?F?%i?1?", minsize);
	} else if(maxsize != -1) {
	    sizebuf2(sstr, sstrdata + 32, 1);
	    sstrdata += snprintf(sstr + sstrdata, sstrsize - sstrdata, "T?T?%i?1?", maxsize);
	} else {
	    bufcat(sstr, "F?F?0?1?", 8);
	}
	if(list != NULL)
	{
	    for(cur = list; cur != NULL; cur = cur->next)
	    {
		/* Use DCCHARSET in $Get paths until further researched... */
		if((buf = icwcstombs(cur->str, DCCHARSET)) == NULL)
		{
		    /* Can't find anything anyway if the search expression
		     * requires characters outside DC's charset. There's
		     * nothing technically wrong with the search itself,
		     * however, so return success. This should be
		     * considered as an optimization. */
		    freesl(&list);
		    if(sstr != NULL)
			free(sstr);
		    return(0);
		}
		if(cur != list)
		    addtobuf(sstr, '$');
		/*
		 * It probably doesn't hurt if buf contains any extra
		 * dollar signs - it will just result in extra search
		 * terms, and the extraneous results will be filtered by
		 * the search layer anyway. It hurts if it contains any
		 * pipes, though, so let's sell them for money.
		 */
		for(p = buf; *p; p++)
		{
		    if(*p == '|')
			*p = '$';
		}
		bufcat(sstr, buf, strlen(buf));
		free(buf);
	    }
	} else {
	    /* Will match all files... :-/ */
	    addtobuf(sstr, '.');
	}
    }
    addtobuf(sstr, 0);
    if(tcpsock != NULL)
    {
	if(sockgetremotename2(udpsock, hub->sk, &name, &namelen) < 0)
	{
	    flog(LOG_WARNING, "cannot get address of UDP socket");
	} else {
	    qstrf(hub->sk, "$Search %s %s|", formataddress(name, namelen), sstr);
	    free(name);
	}
    } else {
	qstrf(hub->sk, "$Search Hub:%s %s|", hub->nativenick, sstr);
    }
    free(sstr);
    freesl(&list);
    fn->lastsrch = time(NULL);
    return(0);
}

#undef skipcmd
#undef qstr
#undef qstrf

#define cc(c) ((void (*)(struct socket *, void *, char *, char *))(c))
static struct command hubcmds[] =
{
    {"$Lock", cc(cmd_lock)},
    {"$HubName", cc(cmd_hubname)},
    {"$Hello", cc(cmd_hello)},
    {"$Quit", cc(cmd_quit)},
    {"$NickList", cc(cmd_nicklist)},
    {"$OpList", cc(cmd_oplist)},
    {"$MyINFO", cc(cmd_myinfo)},
    {"$ForceMove", cc(cmd_forcemove)},
    {"$Search", cc(cmd_search), .limit = 100},
    {"$MultiSearch", cc(cmd_search), .limit = 50},
    {"$ConnectToMe", cc(cmd_connecttome), .limit = 200},
    {"$RevConnectToMe", cc(cmd_revconnecttome), .limit = 500},
    {"$GetNetInfo", cc(cmd_getnetinfo)},
    {"$To:", cc(cmd_to)},
    {"$SR", cc(cmd_sr)},
    {"$UserCommand", cc(cmd_usercommand)},
    {"$GetPass", cc(cmd_getpass)},
    {"$LogedIn", cc(cmd_logedin)}, /* sic */
    {"$Supports", cc(cmd_hubsupports)},
    {NULL, NULL}
};

static struct command peercmds[] =
{
    {"$MyNick", cc(cmd_mynick)},
    {"$Lock", cc(cmd_peerlock)},
    {"$Direction", cc(cmd_direction)},
    {"$Key", cc(cmd_key)},
    {"$FileLength", cc(cmd_filelength)},
    {"$Error", cc(cmd_error)},
    {"$MaxedOut", cc(cmd_maxedout)},
    {"$Get", cc(cmd_get)},
    {"$Send", cc(cmd_send)},
    {"$Supports", cc(cmd_supports)},
    {"$GetBlock", cc(cmd_getblock)},
    {"$UGetBlock", cc(cmd_getblock)},
    {"$GetZBlock", cc(cmd_getblock)},
    {"$UGetZBlock", cc(cmd_getblock)},
    {"$ADCGET", cc(cmd_adcget)},
    {"$ADCSND", cc(cmd_adcsnd), .stop = 1},
    {"$Sending", cc(cmd_sending), .stop = 1},
    {NULL, NULL}
};
#undef cc

static void dctransdetach(struct transfer *transfer, struct dcpeer *peer)
{
    CBUNREG(transfer, trans_filterout, peer);
    peer->transfer = NULL;
    peer->close = 1;
}

static void dctransgotdata(struct transfer *transfer, struct dcpeer *peer)
{
    int ret;
    void *buf;
    unsigned char outbuf[1024];
    z_stream *cstr;
    size_t bufsize;
    
    if((peer->state == PEER_TRNS) || (peer->state == PEER_SYNC))
    {
	if(sockqueuesize(peer->sk) < 65536)
	{
	    if((buf = transfergetdata(transfer, &bufsize)) != NULL)
	    {
		if(peer->compress == CPRS_NONE)
		{
		    sockqueue(peer->sk, buf, bufsize);
		} else if(peer->compress == CPRS_ZLIB) {
		    cstr = peer->cprsdata;
		    cstr->next_in = buf;
		    cstr->avail_in = bufsize;
		    while(cstr->avail_in > 0)
		    {
			cstr->next_out = outbuf;
			cstr->avail_out = sizeof(outbuf);
			if((ret = deflate(cstr, 0)) != Z_OK)
			{
			    flog(LOG_WARNING, "bug? deflate() did not return Z_OK (but rather %i)", ret);
			    freedcpeer(peer);
			    return;
			}
			sockqueue(peer->sk, outbuf, sizeof(outbuf) - cstr->avail_out);
		    }
		}
		free(buf);
	    }
	    if(peer->state == PEER_SYNC)
	    {
		if(peer->compress == CPRS_ZLIB)
		{
		    cstr = peer->cprsdata;
		    cstr->next_in = NULL;
		    cstr->avail_in = 0;
		    do
		    {
			cstr->next_out = outbuf;
			cstr->avail_out = sizeof(outbuf);
			ret = deflate(cstr, Z_FINISH);
			if((ret != Z_OK) && (ret != Z_STREAM_END))
			{
			    flog(LOG_WARNING, "bug? deflate(Z_FINISH) did not return Z_OK (but rather %i)", ret);
			    freedcpeer(peer);
			    return;
			}
			sockqueue(peer->sk, outbuf, sizeof(outbuf) - cstr->avail_out);
		    } while(ret != Z_STREAM_END);
		}
		if(peer->ptclose)
		{
		    freedcpeer(peer);
		} else {
		    if(peer->timeout == NULL)
			peer->timeout = timercallback(ntime() + 180, (void (*)(int, void *))peertimeout, peer);
		    peer->state = PEER_CMD;
		    endcompress(peer);
		    transfersetstate(transfer, TRNS_HS);
		    socksettos(peer->sk, confgetint("fnet", "fnptos"));
		    transfer->flags.b.minislot = 0;
		    peer->sk->writecb = NULL;
		}
	    }
	}
    }
}

static void dctransendofdata(struct transfer *transfer, struct dcpeer *peer)
{
    peer->state = PEER_SYNC;
    dctransgotdata(transfer, peer);
}

static void dcwantdata(struct transfer *transfer, struct dcpeer *peer)
{
    if(transferdatasize(transfer) < 65536)
	peer->sk->ignread = 0;
}

static void transread(struct socket *sk, struct dcpeer *peer)
{
    void *buf;
    size_t bufsize;
    struct transfer *transfer;
    
    if((buf = sockgetinbuf(sk, &bufsize)) == NULL)
	return;
    if(peer->transfer == NULL)
    {
	free(buf);
	freedcpeer(peer);
	return;
    }
    transferputdata(peer->transfer, buf, bufsize);
    free(buf);
    if(peer->transfer->curpos >= peer->transfer->size)
    {
	transfer = peer->transfer;
	transferdetach(transfer);
	transferendofdata(transfer);
	return;
    }
    if(transferdatasize(peer->transfer) > 65535)
	sk->ignread = 1;
}

static void transerr(struct socket *sk, int err, struct dcpeer *peer)
{
    struct transfer *transfer;

    if((transfer = peer->transfer) == NULL)
    {
	freedcpeer(peer);
	return;
    }
    transferdetach(transfer);
    transferendofdata(transfer);
}

static void transwrite(struct socket *sk, struct dcpeer *peer)
{
    if((peer->state != PEER_TRNS) && (peer->state != PEER_SYNC))
	return;
    if(peer->transfer == NULL)
    {
	freedcpeer(peer);
	return;
    }
    dctransgotdata(peer->transfer, peer);
}

static void udpread(struct socket *sk, void *data)
{
    char *buf, *p, *p2, *hashbuf;
    size_t buflen, hashlen;
    char *nick, *filename, *hubname;
    struct sockaddr_in hubaddr;
    off_t size;
    int slots;
    struct fnetnode *fn, *myfn;
    struct dchub *hub;
    struct srchres *sr;
    wchar_t *wnick, *wfile;
    struct hash *hash;
    
    if((buf = sockgetinbuf(sk, &buflen)) == NULL)
	return;
    buf = srealloc(buf, buflen + 1);
    buf[buflen] = 0;
    if(!strncmp(buf, "$SR ", 4))
    {
	p = buf + 4;
	nick = p;
	if((p2 = strchr(p, ' ')) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	p = p2 + 1;
	filename = p;
	if((p2 = strchr(p, 5)) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	p = p2 + 1;
	if((p2 = strchr(p, ' ')) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	size = strtoll(p, NULL, 10);
	p = p2 + 1;
	if((p2 = strchr(p, '/')) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	slots = atoi(p);
	p = p2 + 1;
	if((p2 = strchr(p, 5)) == NULL)
	{
	    free(buf);
	    return;
	}
	p = p2 + 1;
	hubname = p;
	if((p2 = strstr(p, " (")) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	p = p2 + 2;
	if((p2 = strchr(p, ':')) == NULL)
	{
	    free(buf);
	    return;
	}
	*(p2++) = 0;
	hubaddr.sin_family = AF_INET;
	if(!inet_aton(p, &hubaddr.sin_addr))
	{
	    free(buf);
	    return;
	}
	p = p2;
	if((p2 = strchr(p, ')')) == NULL)
	{
	    free(buf);
	    return;
	}
	*p2 = 0;
	hubaddr.sin_port = htons(atoi(p));
	/* Use DCCHARSET in $Get paths until further researched... */
	if((wfile = nmdc2path(filename, DCCHARSET)) == NULL)
	{
	    free(buf);
	    return;
	}
	myfn = NULL;
	hash = NULL;
	if(!strncmp(hubname, "TTH:", 4))
	{
	    if((hashbuf = base32decode(hubname + 4, &hashlen)) != NULL)
	    {
		if(hashlen == 24)
		    hash = newhash(L"TTH", 24, hashbuf);
		free(hashbuf);
	    }
	} else {
	    for(fn = fnetnodes; fn != NULL; fn = fn->next)
	    {
		if((fn->fnet == &dcnet) && ((hub = fn->data) != NULL))
		{
		    if((hub->nativename != NULL) && !strcmp(hub->nativename, hubname))
		    {
			if(myfn == NULL)
			{
			    myfn = fn;
			} else {
			    myfn = NULL;
			    break;
			}
		    }
		}
	    }
	}
	if(myfn == NULL)
	{
	    for(fn = fnetnodes; fn != NULL; fn = fn->next)
	    {
		if((fn->fnet == &dcnet) && ((hub = fn->data) != NULL))
		{
		    if((hub->sk != NULL) && addreq(hub->sk->remote, (struct sockaddr *)&hubaddr))
		    {
			myfn = fn;
			break;
		    }
		}
	    }
	}
	hub = NULL;
	if(myfn != NULL)
	    hub = myfn->data;
	if((wnick = icmbstowcs(nick, (hub == NULL)?DCCHARSET:(hub->charset))) == NULL)
	{
	    free(buf);
	    return;
	}
	sr = newsrchres(&dcnet, wfile, wnick);
	if(sr->peernick != NULL)
	    free(sr->peernick);
	sr->peernick = swcsdup(wnick);
	sr->size = size;
	sr->slots = slots;
	free(wfile);
	free(wnick);
	if(myfn != NULL)
	    getfnetnode(sr->fn = myfn);
	if(hash != NULL)
	    sr->hash = hash;
	submitsrchres(sr);
	freesrchres(sr);
    }
    free(buf);
}

static void hubread(struct socket *sk, struct fnetnode *fn)
{
    struct dchub *hub;
    struct command *cmd;
    char *newbuf;
    size_t datalen, cnlen;
    char *p, *p2;
    
    hub = (struct dchub *)fn->data;
    if((newbuf = sockgetinbuf(sk, &datalen)) == NULL)
	return;
    if(hub->inbufdata > 500000) /* Discard possible malicious data */
	hub->inbufdata = 0;
    sizebuf2(hub->inbuf, hub->inbufdata + datalen, 1);
    memcpy(hub->inbuf + hub->inbufdata, newbuf, datalen);
    free(newbuf);
    p = hub->inbuf;
    hub->inbufdata += datalen;
    while((p - hub->inbuf < hub->inbufdata) && ((p2 = memchr(p, '|', hub->inbufdata - (p - hub->inbuf))) != NULL))
    {
	*(p2++) = 0;
	for(cmd = hubcmds; cmd->handler != NULL; cmd++)
	{
	    cnlen = strlen(cmd->name);
	    if(!strncmp(p, cmd->name, cnlen) && ((p[cnlen] == ' ') || (p[cnlen] == 0)))
		break;
	}
	if((cmd->limit == 0) || (hub->queue.size < cmd->limit))
	    newqcmd(&hub->queue, p);
	p = p2;
    }
    memmove(hub->inbuf, p, hub->inbufdata -= p - hub->inbuf);
    if(hub->queue.size > 1000)
	sk->ignread = 1;
}

static void huberr(struct socket *sk, int err, struct fnetnode *fn)
{
    killfnetnode(fn);
}

static int hubsetnick(struct fnetnode *fn, wchar_t *newnick)
{
    struct dchub *hub;
    char *buf;
    
    hub = fn->data;
    if((buf = icwcstombs(newnick, (hub == NULL)?DCCHARSET:(hub->charset))) == NULL)
	return(1);
    if((strchr(buf, ' ') != NULL) || (strchr(buf, '|') != NULL) || (strchr(buf, '$') != NULL))
    {
	free(buf);
	return(1);
    }
    if(hub == NULL) /* Not yet connected */
    {
	free(buf);
	return(0);
    }
    if(hub->nativenick != NULL)
	free(hub->nativenick);
    hub->nativenick = buf;
    return(0);
}

static struct dchub *newdchub(struct fnetnode *fn)
{
    struct dchub *new;
    wchar_t *emu;
    wchar_t *wcharset;
    char *charset;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    fn->data = new;
    if(confgetint("dc", "dcppemu"))
	new->dcppemu = 1;
    if((emu = wpfind(fn->args, L"dcppemu")) != NULL)
    {
	if(*emu == L'y')
	    new->dcppemu = 1;
	if(*emu == L'n')
	    new->dcppemu = 0;
    }
    charset = NULL;
    if((wcharset = wpfind(fn->args, L"charset")) != NULL)
    {
	if((charset = icwcstombs(wcharset, "US-ASCII")) != NULL)
	{
	    if(!havecharset(charset))
	    {
		free(charset);
		charset = NULL;
	    }
	}
    }
    if(charset != NULL)
	new->charset = charset;
    else
	new->charset = sstrdup(DCCHARSET);
    if(hubsetnick(fn, fn->mynick))
	fnetsetnick(fn, L"DoldaConnectUser-IN");
    /* IN as in Invalid Nick */
    return(new);
}

static struct dcpeer *newdcpeer(struct socket *sk)
{
    struct dcpeer *new;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    new->transfer = NULL;
    getsock(sk);
    new->sk = sk;
    if(confgetint("dc", "dcppemu"))
	new->dcppemu = 1;
    new->next = peers;
    new->prev = NULL;
    if(peers != NULL)
	peers->prev = new;
    peers = new;
    numdcpeers++;
    return(new);
}

static void freedcpeer(struct dcpeer *peer)
{
    int i;
    struct qcommand *qcmd;
    
    if(peers == peer)
	peers = peer->next;
    if(peer->next != NULL)
	peer->next->prev = peer->prev;
    if(peer->prev != NULL)
	peer->prev->next = peer->next;
    if(peer->transfer != NULL)
    {
	if(peer->transfer->dir == TRNSD_UP)
	    peer->transfer->close = 1;
	if(peer->transfer->dir == TRNSD_DOWN)
	    resettransfer(peer->transfer);
    }
    if(peer->timeout != NULL)
	canceltimer(peer->timeout);
    if(peer->sk->data == peer)
	peer->sk->data = NULL;
    peer->sk->readcb = NULL;
    peer->sk->writecb = NULL;
    peer->sk->errcb = NULL;
    putsock(peer->sk);
    endcompress(peer);
    if(peer->supports != NULL)
    {
	for(i = 0; peer->supports[i] != NULL; i++)
	    free(peer->supports[i]);
	free(peer->supports);
    }
    if(peer->inbuf != NULL)
	free(peer->inbuf);
    if(peer->key != NULL)
	free(peer->key);
    if(peer->wcsname != NULL)
	free(peer->wcsname);
    if(peer->nativename != NULL)
	free(peer->nativename);
    if(peer->charset != NULL)
	free(peer->charset);
    if(peer->fn != NULL)
	putfnetnode(peer->fn);
    while((qcmd = ulqcmd(&peer->queue)) != NULL)
	freeqcmd(qcmd);
    free(peer);
    numdcpeers--;
}

static void hubconnect(struct fnetnode *fn, struct socket *sk)
{
    struct dchub *hub;
    
    sk->readcb = (void (*)(struct socket *, void *))hubread;
    sk->errcb = (void (*)(struct socket *, int, void *))huberr;
    fn->data = hub = newdchub(fn);
    sk->data = fn;
    getsock(hub->sk = sk);
    return;
}

static void hubdestroy(struct fnetnode *fn)
{
    int i;
    struct dchub *hub;
    struct qcommand *qcmd;
    
    hub = (struct dchub *)fn->data;
    putsock(hub->sk);
    while((qcmd = ulqcmd(&hub->queue)) != NULL)
	freeqcmd(qcmd);
    if(hub->supports != NULL)
    {
	for(i = 0; hub->supports[i] != NULL; i++)
	    free(hub->supports[i]);
	free(hub->supports);
    }
    if(hub->nativename != NULL)
	free(hub->nativename);
    if(hub->nativenick != NULL)
	free(hub->nativenick);
    if(hub->charset != NULL)
	free(hub->charset);
    if(hub->inbuf != NULL)
	free(hub->inbuf);
    free(hub);
}

static void hubkill(struct fnetnode *fn)
{
    struct dchub *hub;
    
    hub = (struct dchub *)fn->data;
    hub->sk->close = 1;
}

static struct transferiface dctransfer =
{
    .detach = (void (*)(struct transfer *, void *))dctransdetach,
    .gotdata = (void (*)(struct transfer *, void *))dctransgotdata,
    .endofdata = (void (*)(struct transfer *, void *))dctransendofdata,
    .wantdata = (void (*)(struct transfer *, void *))dcwantdata
};

static struct fnet dcnet =
{
    .name = L"dc",
    .connect = hubconnect,
    .destroy = hubdestroy,
    .kill = hubkill,
    .setnick = hubsetnick,
    .reqconn = hubreqconn,
    .sendchat = hubsendchat,
    .search = hubsearch,
};

static void peerread(struct socket *sk, struct dcpeer *peer)
{
    char *newbuf, *p;
    size_t datalen, cnlen;
    struct command *cmd;

    if((newbuf = sockgetinbuf(sk, &datalen)) == NULL)
	return;
    sizebuf2(peer->inbuf, peer->inbufdata + datalen, 1);
    memcpy(peer->inbuf + peer->inbufdata, newbuf, datalen);
    free(newbuf);
    peer->inbufdata += datalen;
    if(peer->state == PEER_CMD)
    {
	p = peer->inbuf;
	while((peer->inbufdata > 0) && (p = memchr(peer->inbuf, '|', peer->inbufdata)) != NULL)
	{
	    *(p++) = 0;
	    for(cmd = peercmds; cmd->handler != NULL; cmd++)
	    {
		cnlen = strlen(cmd->name);
		if(!strncmp(peer->inbuf, cmd->name, cnlen) && ((peer->inbuf[cnlen] == ' ') || (peer->inbuf[cnlen] == 0)))
		    break;
	    }
	    if((cmd->limit == 0) || (peer->queue.size < cmd->limit))
		newqcmd(&peer->queue, peer->inbuf);
	    memmove(peer->inbuf, p, peer->inbufdata -= p - peer->inbuf);
	    if(cmd->stop)
	    {
		peer->state = PEER_STOP;
		break;
	    } else {
		if(peer->queue.size > 50)
		    sk->ignread = 1;
	    }
	}
    } else if(peer->state == PEER_TTHL) {
	handletthl(peer);
    }
    if(peer->inbufdata > 500000)
	sk->ignread = 1;
}

static void peererror(struct socket *sk, int err, struct dcpeer *peer)
{
    freedcpeer(peer);
}

static void peerconnect(struct socket *sk, int err, struct fnetnode *fn)
{
    struct dcpeer *peer;
    struct dchub *hub;
    
    if(err != 0)
    {
	putfnetnode(fn);
	putsock(sk);
	return;
    }
    hub = fn->data;
    peer = newdcpeer(sk);
    peer->fn = fn;
    peer->accepted = 0;
    peer->dcppemu = hub->dcppemu;
    sk->readcb = (void (*)(struct socket *, void *))peerread;
    sk->errcb = (void (*)(struct socket *, int, void *))peererror;
    sk->data = peer;
    socksettos(sk, confgetint("fnet", "fnptos"));
    putsock(sk);
    peer->timeout = timercallback(ntime() + 180, (void (*)(int, void *))peertimeout, peer);
    sendmynick(peer);
    sendpeerlock(peer);
}

static void peeraccept(struct socket *sk, struct socket *newsk, void *data)
{
    struct dcpeer *peer;
    
    peer = newdcpeer(newsk);
    peer->accepted = 1;
    newsk->readcb = (void (*)(struct socket *, void *))peerread;
    newsk->errcb = (void (*)(struct socket *, int, void *))peererror;
    newsk->data = peer;
    socksettos(newsk, confgetint("fnet", "fnptos"));
    peer->timeout = timercallback(ntime() + 180, (void (*)(int, void *))peertimeout, peer);
}

static void updatehmlist(void)
{
    int i, lev, ic, ret;
    struct sharecache *node;
    char *buf, *buf2, numbuf[32];
    size_t bufsize, bufdata;
    int fd, ibuf;
    
    bufdata = 0;
    buf = smalloc(bufsize = 65536);
    node = shareroot->child;
    lev = 0;
    while(1)
    {
	ic = 0;
	/* Use DCCHARSET in $Get paths until further researched... */
	if((buf2 = icwcstombs(node->name, DCCHARSET)) != NULL)
	{
	    for(i = 0; i < lev; i++)
		addtobuf(buf, 9);
	    bufcat(buf, buf2, strlen(buf2));
	    free(buf2);
	    if(node->f.b.type == FILE_REG)
	    {
		addtobuf(buf, '|');
		sprintf(numbuf, "%ji", (intmax_t)node->size);
		bufcat(buf, numbuf, strlen(numbuf));
	    }
	    addtobuf(buf, 13);
	    addtobuf(buf, 10);
	} else {
	    ic = 1;
	}
	if((node->child != NULL) && !ic)
	{
	    lev++;
	    node = node->child;
	} else if(node->next != NULL) {
	    node = node->next;
	} else {
	    while(node->next == NULL)
	    {
		lev--;
		node = node->parent;
		if(node == shareroot)
		    break;
	    }
	    if(node == shareroot)
		break;
	    node = node->next;
	}
    }
    if(hmlistname != NULL)
    {
	unlink(hmlistname);
	free(hmlistname);
    }
    hmlistname = sstrdup("/tmp/dc-filelist-hm-XXXXXX");
    if((fd = mkstemp(hmlistname)) < 0)
    {
	flog(LOG_WARNING, "could not create HM file list tempfile: %s", strerror(errno));
	free(hmlistname);
	hmlistname = NULL;
    } else {
	/*
	 * I do not want to implement a good Huffman encoder, and it's not
	 * like Huffman encoding actually yields any impressive results
	 * for DC file lists anyway, so I'll just output a bogus
	 * tree. Implement a good encoder if you want to.
	 */
	write(fd, "HE3\r\0", 5);
	write(fd, &bufdata, 4);
	ibuf = 256;
	write(fd, &ibuf, 2);
	ibuf = 8;
	for(i = 0; i < 256; i++)
	{
	    write(fd, &i, 1);
	    write(fd, &ibuf, 1);
	}
	for(i = 0; i < 256; i++)
	    write(fd, &i, 1);
	for(buf2 = buf; bufdata > 0;)
	{
	    if((ret = write(fd, buf2, bufdata)) < 0)
	    {
		flog(LOG_WARNING, "could not write file list: %s", strerror(errno));
		break;
	    }
	    bufdata -= ret;
	    buf2 += ret;
	}
	close(fd);
    }
    free(buf);
}

static struct xmlent
{
    wchar_t c;
    wchar_t *ent;
} entities[] = {
    {L'&', L"&amp;"},
    {L'\"', L"&#34;"},
/*  {L'\'', L"&quot;"},  Emulate DC++ escaping by omitting this. */
    {L'\0', NULL}
};

static wchar_t *escapexml(wchar_t *src)
{
    static wchar_t *buf = NULL;;
    int ret, c;
    wchar_t nbuf[32];
    size_t bufsize, bufdata;
    struct xmlent *ent;
    
    if(buf != NULL)
	free(buf);
    buf = NULL;
    bufsize = bufdata = 0;
    for(; *src != L'\0'; src++)
    {
	c = wctob(*src);
	if((c > 0) && (c < 32))
	{
	    bufcat(buf, L"&#", 2);
	    ret = swprintf(nbuf, (sizeof(nbuf) / sizeof(*nbuf)), L"%i", c);
	    bufcat(buf, nbuf, ret);
	    addtobuf(buf, L';');
	} else {
	    for(ent = entities; ent->ent != NULL; ent++)
	    {
		if(ent->c == *src)
		{
		    bufcat(buf, ent->ent, wcslen(ent->ent));
		    break;
		}
	    }
	    if(ent->ent == NULL)
		addtobuf(buf, *src);
	}
    }
    addtobuf(buf, L'\0');
    return(buf);
}

static void updatexmllist(void)
{
    int i, fd, lev;
    FILE *fs;
    char cidbuf[14], *namebuf;
    char *hashbuf;
    struct sharecache *node;
    
    if(xmllistname != NULL)
    {
	unlink(xmllistname);
	free(xmllistname);
    }
    xmllistname = sstrdup("/tmp/dc-filelist-dcxml-XXXXXX");
    if((fd = mkstemp(xmllistname)) < 0)
    {
	flog(LOG_WARNING, "could not create XML file list tempfile: %s", strerror(errno));
	free(xmllistname);
	xmllistname = NULL;
	return;
    }
    if((fs = fdopen(fd, "w")) == NULL)
    {
	flog(LOG_WARNING, "could not fdopen XML list fd %i: %s", fd, strerror(errno));
	unlink(xmllistname);
	free(xmllistname);
	xmllistname = NULL;
	close(fd);
	return;
    }
    fprintf(fs, "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\r\n");
    for(i = 0; i < sizeof(cidbuf) - 1; i++)
	cidbuf[i] = (rand() % ('Z' - 'A' + 1)) + 'A';
    cidbuf[i] = 0;
    if(confgetint("dc", "dcppemu"))
	fprintf(fs, "<FileListing Version=\"1\" CID=\"%s\" Base=\"/\" Generator=\"DC++ 0.674\">\r\n", cidbuf);
    else
	fprintf(fs, "<FileListing Version=\"1\" CID=\"%s\" Base=\"/\" Generator=\"%s\">\r\n", cidbuf, "DoldaConnect" VERSION);
    
    node = shareroot->child;
    lev = 0;
    while(1)
    {
	if((namebuf = icswcstombs(escapexml(node->name), "UTF-8", NULL)) != NULL)
	{
	    for(i = 0; i < lev; i++)
		fputc('\t', fs);
	    if(node->child != NULL)
	    {
		fprintf(fs, "<Directory Name=\"%s\">\r\n", namebuf);
		node = node->child;
		lev++;
		continue;
	    } else {
		fprintf(fs, "<File Name=\"%s\" Size=\"%ji\"", namebuf, (intmax_t)node->size);
		if(node->f.b.hastth)
		{
		    hashbuf = base32encode(node->hashtth, 24);
		    fprintf(fs, " TTH=\"%.39s\"", hashbuf);
		    free(hashbuf);
		}
		fprintf(fs, "/>\r\n");
	    }
	    while(node->next == NULL)
	    {
		node = node->parent;
		if(node == shareroot)
		{
		    break;
		} else {
		    lev--;
		    for(i = 0; i < lev; i++)
			fputc('\t', fs);
		    fprintf(fs, "</Directory>\r\n");
		}
	    }
	    if(node == shareroot)
		break;
	    node = node->next;
	}
    }
    
    if(confgetint("dc", "dcppemu"))
	fprintf(fs, "</FileListing>");
    else
	fprintf(fs, "</FileListing>\r\n");
    fclose(fs);
}

static void updatexmlbz2list(void)
{
    int err, fd;
    FILE *in;
    FILE *real;
    BZFILE *out;
    char buf[1024];
    size_t bufdata;
    
    if(xmllistname == NULL)
	return;
    if((in = fopen(xmllistname, "r")) == NULL)
    {
	flog(LOG_WARNING, "could not open XML file list for bzipping: %s", strerror(errno));
	return;
    }
    if(xmlbz2listname != NULL)
    {
	unlink(xmlbz2listname);
	free(xmlbz2listname);
    }
    xmlbz2listname = sstrdup("/tmp/dc-filelist-dcxmlbz2-XXXXXX");
    if((fd = mkstemp(xmlbz2listname)) < 0)
    {
	flog(LOG_WARNING, "could not create bzipped XML file list tempfile: %s", strerror(errno));
	free(xmlbz2listname);
	xmlbz2listname = NULL;
	fclose(in);
	return;
    }
    if((real = fdopen(fd, "w")) == NULL)
    {
	flog(LOG_WARNING, "could not fdopen bzipped XML list fd %i: %s", fd, strerror(errno));
	close(fd);
	unlink(xmlbz2listname);
	free(xmlbz2listname);
	xmlbz2listname = NULL;
	fclose(in);
	return;
    }
    out = BZ2_bzWriteOpen(&err, real, 9, 0, 0);
    if(err != BZ_OK)
    {
	flog(LOG_WARNING, "could not open bzip2 stream from XML list");
	fclose(real);
	unlink(xmlbz2listname);
	free(xmlbz2listname);
	xmlbz2listname = NULL;
	fclose(in);
	return;
    }
    while(!feof(in))
    {
	bufdata = fread(buf, 1, sizeof(buf), in);
	BZ2_bzWrite(&err, out, buf, bufdata);
    }
    fclose(in);
    BZ2_bzWriteClose(&err, out, 0, NULL, NULL);
    fclose(real);
}

static void listtimercb(int cancelled, void *uudata)
{
    listwritetimer = NULL;
    if(!cancelled)
	updatelists(1);
}

static void updatelists(int now)
{
    if((hmlistname == NULL) || (xmllistname == NULL) || (xmlbz2listname == NULL))
	now = 1;
    if(!now)
    {
	if(listwritetimer == NULL)
	    listwritetimer = timercallback(ntime() + confgetint("cli", "hashwritedelay"), listtimercb, NULL);
	return;
    }
    if(listwritetimer != NULL)
	canceltimer(listwritetimer);
    updatehmlist();
    updatexmllist();
    updatexmlbz2list();
}

static int shareupdate(unsigned long long uusharesize, void *data)
{
    updatelists(0);
    return(0);
}

static char *quotestr(char *str)
{
    unsigned char *buf;
    unsigned char *p;
    size_t bufsize, bufdata;
    wchar_t *wbuf;
    static char *enc = NULL;
    size_t encsize, encdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(p = (unsigned char *)str; *p; p++)
    {
	if(*p == '\b')
	    bufcat(buf, "\\b", 2);
	else if(*p == '\t')
	    bufcat(buf, "\\t", 2);
	else if(*p == '\n')
	    bufcat(buf, "\\n", 2);
	else if(*p == '\r')
	    bufcat(buf, "\\r", 2);
	else if(*p == '\\')
	    bufcat(buf, "\\\\", 2);
	else if(*p >= 32)
	    addtobuf(buf, *p);
	else
	    bprintf(buf, "\\x%02x", *p);
    }
    addtobuf(buf, 0);
    if(enc != NULL)
	free(enc);
    enc = NULL;
    if((wbuf = icmbstowcs((char *)buf, DCCHARSET)) != NULL)
    {
	enc = icwcstombs(wbuf, NULL);
	free(wbuf);
    }
    if(enc == NULL)
    {
	encsize = encdata = 0;
	for(p = buf; *p; p++) {
	    if(*p < 128)
		addtobuf(enc, *p);
	    else
		bprintf(buf, "\\x%x", *p);
	}
    }
    free(buf);
    return(enc);
}

static void logunimpl(char *list, char *cmd, char *args)
{
    FILE *log;
    
    if((log = fopen("/tmp/dc-unimpl", "a")) == NULL)
    {
	flog(LOG_WARNING, "could not open unimpl log: %s", strerror(errno));
	return;
    }
    fputs(list, log);
    fputc('\t', log);
    fputs(quotestr(cmd), log);
    if(args != NULL)
    {
	fputc('\t', log);
	fputs(quotestr(args), log);
    }
    fputc('\n', log);
    fclose(log);
}

static void dispatchcommand(struct qcommand *qcmd, struct command *cmdlist, struct socket *sk, void *data)
{
    char *p;
    struct command *cmd;
    
    if((p = strchr(qcmd->string, ' ')) != NULL)
	*(p++) = 0;
    for(cmd = cmdlist; cmd->handler != NULL; cmd++)
    {
	if(!strcmp(cmd->name, qcmd->string))
	    break;
    }
    if(cmd->handler != NULL)
    {
	cmd->handler(sk, data, qcmd->string, p);
    } else if(confgetint("dc", "logunimpl")) {
	if(cmdlist == hubcmds)
	    logunimpl("hub", qcmd->string, p);
	else if(cmdlist == peercmds)
	    logunimpl("peer", qcmd->string, p);
	else
	    logunimpl("other?!", qcmd->string, p);
    }
}

static int run(void)
{
    struct fnetnode *fn, *nextfn;
    struct dchub *hub;
    struct dcpeer *peer, *nextpeer;
    struct qcommand *qcmd;
    int ret, quota;
    
    ret = 0;
    quota = 20;
    for(fn = fnetnodes; fn != NULL; fn = nextfn)
    {
	nextfn = fn->next;
	if(fn->fnet != &dcnet)
	    continue;
	if(fn->data == NULL)
	    continue;
	hub = (struct dchub *)fn->data;
	while((quota > 0) && ((qcmd = ulqcmd(&hub->queue)) != NULL))
	{
	    if(*qcmd->string == '$')
	    {
		if((hub->sk != NULL) && (hub->sk->state == SOCK_EST))
		    dispatchcommand(qcmd, hubcmds, hub->sk, fn);
	    } else if(*qcmd->string != 0) {
		hubrecvchat(hub->sk, fn, NULL, qcmd->string);
	    }
	    freeqcmd(qcmd);
	    ret = 1;
	    quota--;
	}
	if(hub->queue.size < 1000)
	    hub->sk->ignread = 0;
	if(quota < 1)
	    break;
    }
    quota = 20;
    for(peer = peers; peer != NULL; peer = peer->next)
    {
	while(!peer->close && (quota > 0) && ((qcmd = ulqcmd(&peer->queue)) != NULL))
	{
	    if(peer->timeout != NULL)
		canceltimer(peer->timeout);
	    peer->timeout = timercallback(ntime() + 180, (void (*)(int, void *))peertimeout, peer);
	    if(*qcmd->string == '$')
		dispatchcommand(qcmd, peercmds, peer->sk, peer);
	    freeqcmd(qcmd);
	    ret = 1;
	    quota--;
	}
	if((peer->queue.size < 50) && (peer->inbufdata < 500000))
	    peer->sk->ignread = 0;
	if(quota < 1)
	    break;
    }
    for(peer = peers; peer != NULL; peer = nextpeer)
    {
	nextpeer = peer->next;
	if(peer->close)
	    freedcpeer(peer);
    }
    return(ret);
}

static void preinit(int hup)
{
    if(hup)
	return;
    regfnet(&dcnet);
}

static int updateudpport(struct configvar *var, void *uudata)
{
    struct sockaddr_in addr;
    struct socket *newsock;
    
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(var->val.num);
    if((newsock = netcsdgram((struct sockaddr *)&addr, sizeof(addr))) == NULL)
    {
	flog(LOG_WARNING, "could not create new DC UDP socket, reverting to old: %s", strerror(errno));
	return(0);
    }
    newsock->readcb = udpread;
    if(udpsock != NULL)
	putsock(udpsock);
    udpsock = newsock;
    return(0);
}

static int updatetcpport(struct configvar *var, void *uudata)
{
    struct sockaddr_in addr;
    struct socket *newsock;
    
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(var->val.num);
    if((newsock = netcslisten(SOCK_STREAM, (struct sockaddr *)&addr, sizeof(addr), peeraccept, NULL)) == NULL)
	flog(LOG_INFO, "could not listen to a remote address, going into passive mode");
    if(tcpsock != NULL)
	putsock(tcpsock);
    tcpsock = newsock;
    return(0);
}

static int init(int hup)
{
    struct sockaddr_in addr;
    
    if(!hup)
    {
	GCBREG(sharechangecb, shareupdate, NULL);
	if(udpsock != NULL)
	    putsock(udpsock);
	if(tcpsock != NULL)
	    putsock(tcpsock);
	addr.sin_family = AF_INET;
	memset(&addr.sin_addr, 0, sizeof(addr.sin_addr));
	addr.sin_port = htons(confgetint("dc", "udpport"));
	if((udpsock = netcsdgram((struct sockaddr *)&addr, sizeof(addr))) == NULL)
	{
	    flog(LOG_CRIT, "could not create DC UDP socket: %s", strerror(errno));
	    return(1);
	}
	udpsock->readcb = udpread;
	addr.sin_port = htons(confgetint("dc", "tcpport"));
	if((tcpsock = netcslisten(SOCK_STREAM, (struct sockaddr *)&addr, sizeof(addr), peeraccept, NULL)) == NULL)
	    flog(LOG_INFO, "could not listen to a remote address, going into passive mode");
	CBREG(confgetvar("dc", "udpport"), conf_update, updateudpport, NULL, NULL);
	CBREG(confgetvar("dc", "tcpport"), conf_update, updatetcpport, NULL, NULL);
	CBREG(confgetvar("net", "mode"), conf_update, updatetcpport, NULL, NULL);
    }
    return(0);
}

static void terminate(void)
{
    if(hmlistname != NULL)
    {
	unlink(hmlistname);
	free(hmlistname);
    }
    if(xmllistname != NULL)
    {
	unlink(xmllistname);
	free(xmllistname);
    }
    if(xmlbz2listname != NULL)
    {
	unlink(xmlbz2listname);
	free(xmlbz2listname);
    }
}

static struct configvar myvars[] =
{
    /** Specifies the share description reported to other DC users. */
    {CONF_VAR_STRING, "desc", {.str = L""}},
    /** Specifies the speed reported to other DC users. Normal values
     * are 28.8Kbps, 33.6Kbps, 56Kbps, Satellite, ISDN, DSL, Cable,
     * LAN(T1) or LAN(T3)*/
    {CONF_VAR_STRING, "speedstring", {.str = L"LAN(T1)"}},
    /** The e-mail address to report to other DC users. */
    {CONF_VAR_STRING, "email", {.str = L"spam@spam.org"}},
    /** Specifies a specific UDP port to use for DC search results. If
     * left unspecified, a port is allocated dynamically. Useful for
     * NAT routers (see also the net.visibleipv4 address for those
     * cases). */
    {CONF_VAR_INT, "udpport", {.num = 0}},
    /** Specifies a specific TCP port to use for DC peer
     * connections. If left unspecified, a port is allocated
     * dynamically. Useful for NAT routers (see also the
     * net.visibleipv4 address for those cases). */
    {CONF_VAR_INT, "tcpport", {.num = 0}},
    /** If set to true, doldacond will do its best to emulate DC++
     * (currently v0.674). This should be left off if at all possible,
     * since turning it on will violate the rules of most hubs and
     * thus give hub owners an actual reason to kick you if it is
     * detected. It might be needed for some of the more bone-headed
     * hub owners, though. Note that DC++ emulation can also be turned
     * on or off for individual hubs, overriding this setting. */
    {CONF_VAR_BOOL, "dcppemu", {.num = 0}},
    /** Use for debugging. If set to true, doldacond will log all
     * unknown commands it receives, and their arguments, to
     * /tmp/dc-unimpl. */
    {CONF_VAR_BOOL, "logunimpl", {.num = 0}},
    /** If set to true, doldacond will hide its support for deflate
     * compression of transfers from other clients, so that they will
     * not request compressed uploads. Compressed transfers may
     * consume a non-trivial amount of CPU time on slower machines. */
    {CONF_VAR_BOOL, "hidedeflate", {.num = 0}},
    {CONF_VAR_END}
};

static struct module me =
{
    .conf =
    {
	.vars = myvars
    },
    .preinit = preinit,
    .init = init,
    .run = run,
    .terminate = terminate,
    .name = "dc"
};

MODULE(me)
