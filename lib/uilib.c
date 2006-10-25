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

/*
 * Note: This code is still ugly, since I copied it almost verbatim
 * from the daemon's parser. It would need serious cleanups, but at
 * least it works for now.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <iconv.h>
#include <wchar.h>
#include <wctype.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/poll.h>
#ifdef HAVE_RESOLVER
#include <arpa/nameser.h>
#include <resolv.h>
#endif

#include <doldaconnect/uilib.h>
#include <doldaconnect/utils.h>

#define RESP_END -1
#define RESP_DSC 0
#define RESP_STR 1
#define RESP_INT 2
#define RESP_FLOAT 3

struct respclass
{
    struct respclass *next;
    int code;
    int nwords;
    int *wordt;
};

struct command
{
    struct command *next;
    wchar_t *name;
    struct respclass *classes;
};

struct qcmd
{
    struct qcmd *next;
    struct command *cmd;
    int tag;
    char *buf;
    size_t buflen;
    int (*callback)(struct dc_response *resp);
    void *data;
};

void dc_uimisc_disconnected(void);

/* The first command must be the nameless connect command and second
 * the notification command. */
static struct command *commands = NULL;
static struct qcmd *queue = NULL, *queueend = NULL;
static struct dc_response *respqueue = NULL, *respqueueend = NULL;
static int state = -1;
static int fd = -1;
static iconv_t ichandle;
static int resetreader = 1;
static char *dchostname = NULL;
static struct addrinfo *hostlist = NULL, *curhost = NULL;
static int servport;

static struct dc_response *makeresp(void)
{
    struct dc_response *new;
    
    new = smalloc(sizeof(*new));
    new->next = NULL;
    new->prev = NULL;
    new->code = 0;
    new->cmdname = NULL;
    new->rlines = NULL;
    new->linessize = 0;
    new->numlines = 0;
    new->curline = 0;
    new->data = NULL;
    return(new);
}

static void freeqcmd(struct qcmd *qcmd)
{
    if(qcmd->buf != NULL)
	free(qcmd->buf);
    free(qcmd);
}

static void unlinkqueue(void)
{
    struct qcmd *qcmd;
    
    if((qcmd = queue) == NULL)
	return;
    queue = qcmd->next;
    if(queue == NULL)
	queueend = NULL;
    freeqcmd(qcmd);
}

static struct qcmd *makeqcmd(wchar_t *name)
{
    struct qcmd *new;
    struct command *cmd;
    static int tag = 0;
    
    if(name == NULL)
    {
	cmd = commands;
    } else {
	for(cmd = commands; cmd != NULL; cmd = cmd->next)
	{
	    if((cmd->name != NULL) && !wcscmp(cmd->name, name))
		break;
	}
	if(cmd == NULL)
	    return(NULL);
    }
    new = smalloc(sizeof(*new));
    new->tag = tag++;
    new->cmd = cmd;
    new->buf = NULL;
    new->buflen = 0;
    new->callback = NULL;
    new->next = NULL;
    new->data = NULL;
    if(queueend == NULL)
    {
	queue = queueend = new;
    } else {
	queueend->next = new;
	queueend = new;
    }
    return(new);
}

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

static struct command *makecmd(wchar_t *name)
{
    struct command *new;
    
    new = smalloc(sizeof(*new));
    new->name = name;
    new->classes = NULL;
    new->next = commands;
    commands = new;
    return(new);
}

static struct respclass *addresp(struct command *cmd, int code, ...)
{
    struct respclass *new;
    int i;
    int resps[128];
    va_list args;
        
    va_start(args, code);
    i = 0;
    while((resps[i++] = va_arg(args, int)) != RESP_END);
    i--;
    va_end(args);
    new = smalloc(sizeof(*new));
    new->code = code;
    new->nwords = i;
    if(i > 0)
    {
	new->wordt = smalloc(sizeof(int) * i);
	memcpy(new->wordt, resps, sizeof(int) * i);
    } else {
	new->wordt = NULL;
    }
    new->next = cmd->classes;
    cmd->classes = new;
    return(new);
}

#include "initcmds.h"

int dc_init(void)
{
    if((ichandle = iconv_open("wchar_t", "utf-8")) == (iconv_t)-1)
	return(-1);
    initcmds();
    return(0);
}

void dc_cleanup(void)
{
    iconv_close(ichandle);
}

void dc_disconnect(void)
{
    struct dc_response *resp;
    
    state = -1;
    if(fd >= 0)
	close(fd);
    fd = -1;
    while(queue != NULL)
	unlinkqueue();
    while((resp = dc_getresp()) != NULL)
	dc_freeresp(resp);
    dc_uimisc_disconnected();
    if(dchostname != NULL)
	free(dchostname);
    dchostname = NULL;
}

void dc_freeresp(struct dc_response *resp)
{
    int i, o;
    
    for(i = 0; i < resp->numlines; i++)
    {
	for(o = 0; o < resp->rlines[i].argc; o++)
	    free(resp->rlines[i].argv[o]);
	free(resp->rlines[i].argv);
    }
    free(resp->rlines);
    free(resp);
}

struct dc_response *dc_getresp(void)
{
    struct dc_response *ret;
    
    if((ret = respqueue) == NULL)
	return(NULL);
    respqueue = ret->next;
    if(respqueue == NULL)
	respqueueend = NULL;
    else
	respqueue->prev = NULL;
    return(ret);
}

struct dc_response *dc_gettaggedresp(int tag)
{
    struct dc_response *resp;
    
    for(resp = respqueue; resp != NULL; resp = resp->next)
    {
	if(resp->tag == tag)
	{
	    if(resp->prev != NULL)
		resp->prev->next = resp->next;
	    if(resp->next != NULL)
		resp->next->prev = resp->prev;
	    if(resp == respqueue)
		respqueue = resp->next;
	    if(resp == respqueueend)
		respqueueend = resp->prev;
	    return(resp);
	}
    }
    return(NULL);
}

struct dc_response *dc_gettaggedrespsync(int tag)
{
    struct pollfd pfd;
    struct dc_response *resp;
    
    while((resp = dc_gettaggedresp(tag)) == NULL)
    {
	pfd.fd = fd;
	pfd.events = POLLIN;
	if(dc_wantwrite())
	    pfd.events |= POLLOUT;
	if(poll(&pfd, 1, -1) < 0)
	    return(NULL);
	if((pfd.revents & POLLIN) && dc_handleread())
	    return(NULL);
	if((pfd.revents & POLLOUT) && dc_handlewrite())
	    return(NULL);
    }
    return(resp);
}

int dc_wantwrite(void)
{
    switch(state)
    {
    case 1:
	if((queue != NULL) && (queue->buflen > 0))
	    return(1);
	break;
    }
    return(0);
}

int dc_getstate(void)
{
    return(state);
}

int dc_queuecmd(int (*callback)(struct dc_response *), void *data, ...)
{
    struct qcmd *qcmd;
    int num, freepart;
    va_list al;
    char *final;
    wchar_t **toks;
    wchar_t *buf;
    wchar_t *part, *tpart;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    num = 0;
    va_start(al, data);
    while((part = va_arg(al, wchar_t *)) != NULL)
    {
	if(!wcscmp(part, L"%%a"))
	{
	    for(toks = va_arg(al, wchar_t **); *toks != NULL; toks++)
	    {
		part = *toks;
		freepart = 0;
		if((tpart = quoteword(part)) != NULL)
		{
		    freepart = 1;
		    part = tpart;
		}
		addtobuf(buf, L' ');
		bufcat(buf, part, wcslen(part));
		num++;
		if(freepart)
		    free(part);
	    }
	} else {
	    if(*part == L'%')
	    {
		/* This demands that all arguments that are passed to the
		 * function are of equal length, that of an int. I know
		 * that GCC does that on IA32 platforms, but I do not know
		 * which other platforms and compilers that it applies
		 * to. If this breaks your platform, please mail me about
		 * it.
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
	    if(num > 0)
		addtobuf(buf, L' ');
	    if(num == 0)
	    {
		if((qcmd = makeqcmd(part)) == NULL)
		{
		    if(freepart)
			free(part);
		    if(buf != NULL)
			free(buf);
		    return(-1);
		} else {
		    qcmd->callback = callback;
		    qcmd->data = data;
		}
	    }
	    bufcat(buf, part, wcslen(part));
	    num++;
	    if(freepart)
		free(part);
	}
    }
    bufcat(buf, L"\r\n\0", 3);
    if((final = icwcstombs(buf, "utf-8")) == NULL)
    {
	free(buf);
	return(-1);
    }
    va_end(al);
    free(buf);
    qcmd->buf = final;
    qcmd->buflen = strlen(final);
    return(qcmd->tag);
}

int dc_handleread(void)
{
    int ret, done;
    char *p1, *p2;
    size_t len;
    int errnobak;
    /* Ewww... this really is soo ugly. I need to clean this up some day. */
    static int pstate = 0;
    static char inbuf[128];
    static size_t inbufdata = 0;
    static wchar_t *cbuf = NULL;
    static size_t cbufsize = 0, cbufdata = 0;
    static wchar_t *pptr = NULL;
    static wchar_t **argv = NULL;
    static int argc = 0, args = 0;
    static wchar_t *cw = NULL;
    static size_t cwsize = 0, cwdata = 0;
    static struct dc_response *curresp = NULL;
    static int cont = 0;
    static int unlink = 0;
    
    switch(state)
    {
    case -1:
	return(-1);
    case 0:
	len = sizeof(ret);
	getsockopt(fd, SOL_SOCKET, SO_ERROR, &ret, &len);
	if(ret)
	{
	    int newfd;
	    struct sockaddr_storage addr;
	    struct sockaddr_in *ipv4;
	    struct sockaddr_in6 *ipv6;
	    
	    for(curhost = curhost->ai_next; curhost != NULL; curhost = curhost->ai_next)
	    {
		if((newfd = socket(curhost->ai_family, curhost->ai_socktype, curhost->ai_protocol)) < 0)
		{
		    errnobak = errno;
		    dc_disconnect();
		    errno = errnobak;
		    return(-1);
		}
		dup2(newfd, fd);
		close(newfd);
		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
		memcpy(&addr, curhost->ai_addr, curhost->ai_addrlen);
		if(addr.ss_family == AF_INET)
		{
		    ipv4 = (struct sockaddr_in *)&addr;
		    ipv4->sin_port = htons(servport);
		}
#ifdef HAVE_IPV6
		if(addr.ss_family == AF_INET6)
		{
		    ipv6 = (struct sockaddr_in6 *)&addr;
		    ipv6->sin6_port = htons(servport);
		}
#endif
		if(connect(fd, (struct sockaddr *)&addr, curhost->ai_addrlen))
		{
		    if(errno == EINPROGRESS)
			return(0);
		} else {
		    break;
		}
	    }
	    if(curhost == NULL)
	    {
		dc_disconnect();
		errno = ret;
		return(-1);
	    }
	}
	state = 1;
	resetreader = 1;
	break;
    case 1:
	if(resetreader)
	{
	    inbufdata = 0;
	    cbufdata = 0;
	    pptr = NULL;
	    pstate = 0;
	    resetreader = 0;
	    cont = 0;
	    if(curresp != NULL)
		dc_freeresp(curresp);
	    curresp = NULL;
	}
	if(inbufdata == 128)
	    inbufdata = 0;
	ret = read(fd, inbuf + inbufdata, 128 - inbufdata);
	if(ret < 0)
	{
	    if((errno == EAGAIN) || (errno == EINTR))
		return(0);
	    errnobak = errno;
	    dc_disconnect();
	    errno = errnobak;
	    return(-1);
	} else if(ret == 0) {
	    dc_disconnect();
	    errno = 0;
	    return(-1);
	}
	inbufdata += ret;
	done = 0;
	while(!done)
	{
	    if(cbufsize == cbufdata)
	    {
		if(pptr != NULL)
		    len = pptr - cbuf;
		if((cbuf = realloc(cbuf, sizeof(wchar_t) * (cbufsize += 256))) == NULL)
		{
		    dc_disconnect();
		    errno = ENOMEM;
		    return(-1);
		}
		if(pptr != NULL)
		    pptr = cbuf + len;
	    }
	    p1 = inbuf;
	    p2 = (char *)(cbuf + cbufdata);
	    len = sizeof(wchar_t) * (cbufsize - cbufdata);
	    ret = iconv(ichandle, &p1, &inbufdata, &p2, &len);
	    memmove(inbuf, p1, inbufdata);
	    cbufdata = cbufsize - (len / sizeof(wchar_t));
	    if(ret < 0)
	    {
		switch(errno)
		{
		case EILSEQ:
		    /* XXX Is this really OK? */
		    inbufdata = 0;
		    done = 1;
		    break;
		case EINVAL:
		    done = 1;
		    break;
		case E2BIG:
		    break;
		default:
		    errnobak = errno;
		    dc_disconnect();
		    errno = errnobak;
		    return(-1);
		}
	    } else {
		done = 1;
	    }
	}
	if(pptr == NULL)
	    pptr = cbuf;
	done = 0;
	while(!done && (pptr - cbuf < cbufdata))
	{
	    switch(pstate)
	    {
	    case 0:
		if(iswspace(*pptr))
		{
		    if(*pptr == L'\r')
		    {
			if(pptr == cbuf + cbufdata - 1)
			{
			    done = 1;
			    break;
			}
			if(*(++pptr) == L'\n')
			{
			    if(curresp == NULL)
			    {
				curresp = makeresp();
				if((argc > 0) && ((curresp->code = wcstol(argv[0], NULL, 10)) >= 600))
				{
				    curresp->cmdname = L".notify";
				    curresp->internal = commands->next;
				    curresp->tag = -1;
				    unlink = 0;
				} else {
				    if((curresp->cmdname = queue->cmd->name) == NULL)
					curresp->cmdname = L".connect";
				    curresp->data = queue->data;
				    curresp->tag = queue->tag;
				    curresp->internal = (void *)(queue->cmd);
				    unlink = 1;
				}
			    }
			    sizebuf(&curresp->rlines, &curresp->linessize, curresp->numlines + 1, sizeof(*(curresp->rlines)), 1);
			    curresp->rlines[curresp->numlines].argc = argc;
			    curresp->rlines[curresp->numlines].argv = argv;
			    argv = NULL;
			    argc = args = 0;
			    curresp->numlines++;
			    if(!cont)
			    {
				if((curresp->code >= 600) || (queue == NULL) || (queue->callback == NULL))
				    ret = 0;
				else
				    ret = queue->callback(curresp);
				if(ret == 0)
				{
				    if(respqueue == NULL)
				    {
					respqueue = respqueueend = curresp;
				    } else {
					curresp->next = NULL;
					curresp->prev = respqueueend;
					respqueueend->next = curresp;
					respqueueend = curresp;
				    }
				} else if(ret == 1) {
				    dc_freeresp(curresp);
				}
				curresp = NULL;
				if(unlink)
				    unlinkqueue();
			    }
			    wmemmove(cbuf, pptr, cbufdata -= (pptr - cbuf));
			    pptr = cbuf;
			} else {
			    pptr++;
			}
		    } else {
			pptr++;
		    }
		} else {
		    pstate = 1;
		    cwdata = 0;
		}
		break;
	    case 1:
		if(iswspace(*pptr) || ((argc == 0) && (*pptr == L'-')))
		{
		    if(argc == 0)
		    {
			if(*pptr == L'-')
			{
			    cont = 1;
			    pptr++;
			} else {
			    cont = 0;
			}
		    }
		    addtobuf(cw, L'\0');
		    sizebuf(&argv, &args, argc + 1, sizeof(*argv), 1);
		    argv[argc++] = cw;
		    cw = NULL;
		    cwsize = cwdata = 0;
		    pstate = 0;
		} else if(*pptr == L'\"') {
		    pstate = 2;
		    pptr++;
		} else if(*pptr == L'\\') {
		    if(pptr == cbuf + cbufdata - 1)
		    {
			done = 1;
			break;
		    }
		    addtobuf(cw, *(++pptr));
		    pptr++;
		} else {
		    addtobuf(cw, *(pptr++));
		}
		break;
	    case 2:
		if(*pptr == L'\"')
		{
		    pstate = 1;
		} else if(*pptr == L'\\') {
		    addtobuf(cw, *(++pptr));
		} else {
		    addtobuf(cw, *pptr);
		}
		pptr++;
		break;
	    }
	}
	break;
    }
    return(0);
}

int dc_handlewrite(void)
{
    int ret;
    int errnobak;
    
    switch(state)
    {
    case 1:
	if(queue->buflen > 0)
	{
	    ret = send(fd, queue->buf, queue->buflen, MSG_NOSIGNAL | MSG_DONTWAIT);
	    if(ret < 0)
	    {
		if((errno == EAGAIN) || (errno == EINTR))
		    return(0);
		errnobak = errno;
		dc_disconnect();
		errno = errnobak;
		return(-1);
	    }
	    if(ret > 0)
		memmove(queue->buf, queue->buf + ret, queue->buflen -= ret);
	}
	break;
    }
    return(0);
}

#ifdef HAVE_RESOLVER
static char *readname(unsigned char *msg, unsigned char *eom, unsigned char **p)
{
    char *name, *tname;
    unsigned char *tp;
    size_t namesize, namedata, len;
    
    name = NULL;
    namesize = namedata = 0;
    while(1)
    {
	len = *((*p)++);
	if(len == 0)
	{
	    addtobuf(name, 0);
	    return(name);
	} else if(len == 0xc0) {
	    tp = msg + *((*p)++);
	    if((tname = readname(msg, eom, &tp)) == NULL)
	    {
		if(name != NULL)
		    free(name);
		return(NULL);
	    }
	    bufcat(name, tname, strlen(tname));
	    addtobuf(name, 0);
	    free(tname);
	    return(name);
	} else if(*p + len >= eom) {
	    if(name != NULL)
		free(name);
	    return(NULL);
	}
	bufcat(name, *p, len);
	*p += len;
	addtobuf(name, '.');
    }
}

static int skipname(unsigned char *msg, unsigned char *eom, unsigned char **p)
{
    size_t len;
    
    while(1)
    {
	len = *((*p)++);
	if(len == 0)
	{
	    return(0);
	} else if(len == 0xc0) {
	    (*p)++;
	    return(0);
	} else if(*p + len >= eom) {
	    return(-1);
	}
	*p += len;
    }
}

static int getsrvrr(char *name, char **host, int *port)
{
    int i;
    char *name2, *rrname;
    unsigned char *eom, *p;
    unsigned char buf[1024];
    int flags, num, class, type;
    size_t len;
    int ret;
    
    if(!(_res.options & RES_INIT))
    {
	if(res_init() < 0)
	    return(-1);
    }
    /* res_querydomain doesn't work for some reason */
    name2 = smalloc(strlen("_dolcon._tcp.") + strlen(name) + 2);
    strcpy(name2, "_dolcon._tcp.");
    strcat(name2, name);
    len = strlen(name2);
    if(name2[len - 1] != '.')
    {
	name2[len] = '.';
	name2[len + 1] = 0;
    }
    ret = res_query(name2, C_IN, T_SRV, buf, sizeof(buf));
    if(ret < 0)
    {
	free(name2);
	return(-1);
    }
    eom = buf + ret;
    flags = (buf[2] << 8) + buf[3];
    if((flags & 0xfa0f) != 0x8000)
    {
	free(name2);
	return(-1);
    }
    num = (buf[4] << 8) + buf[5];
    p = buf + 12;
    for(i = 0; i < num; i++)
    {
	if(skipname(buf, eom, &p))
	{
	    free(name2);
	    return(-1);
	}
	p += 4;
    }
    num = (buf[6] << 8) + buf[7];
    for(i = 0; i < num; i++)
    {
	if((rrname = readname(buf, eom, &p)) == NULL)
	{
	    free(name2);
	    return(-1);
	}
	type = *(p++) << 8;
	type += *(p++);
	class = *(p++) << 8;
	class += *(p++);
	p += 4;
	len = *(p++) << 8;
	len += *(p++);
	if((class == C_IN) && (type == T_SRV) && !strcmp(rrname, name2))
	{
	    free(rrname);
	    free(name2);
	    /* Noone will want to have alternative DC servers, so
	     * don't care about priority and weigth */
	    p += 4;
	    if(port == NULL)
	    {
		p += 2;
	    } else {
		*port = *(p++) << 8;
		*port += *(p++);
	    }
	    if(host != NULL)
	    {
		if((rrname = readname(buf, eom, &p)) == NULL)
		    return(-1);
		*host = rrname;
	    }
	    return(0);
	}
	p += len;
	free(rrname);
    }
    free(name2);
    return(-1);
}
#else
static int getsrvrr(char *name, char **host, int *port)
{
    errno = EOPNOTSUPP;
    return(-1);
}
#endif

int dc_connect(char *host, int port)
{
    struct addrinfo hint;
    struct sockaddr_storage addr;
    struct sockaddr_in *ipv4;
#ifdef HAVE_IPV6
    struct sockaddr_in6 *ipv6;
#endif
    struct qcmd *qcmd;
    char *newhost;
    int getsrv, freehost;
    int errnobak;
    
    if(fd >= 0)
	dc_disconnect();
    state = -1;
    freehost = 0;
    if(port < 0)
    {
	port = 1500;
	getsrv = 1;
    } else {
	getsrv = 0;
    }
    memset(&hint, 0, sizeof(hint));
    hint.ai_socktype = SOCK_STREAM;
    if(getsrv)
    {
	if(!getsrvrr(host, &newhost, &port))
	{
	    host = newhost;
	    freehost = 1;
	}
    }
    servport = port;
    if(hostlist != NULL)
	freeaddrinfo(hostlist);
    if(getaddrinfo(host, NULL, &hint, &hostlist))
    {
	errno = ENONET;
	if(freehost)
	    free(host);
	return(-1);
    }
    for(curhost = hostlist; curhost != NULL; curhost = curhost->ai_next)
    {
	if((fd = socket(curhost->ai_family, curhost->ai_socktype, curhost->ai_protocol)) < 0)
	{
	    errnobak = errno;
	    if(freehost)
		free(host);
	    errno = errnobak;
	    return(-1);
	}
	fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
	memcpy(&addr, curhost->ai_addr, curhost->ai_addrlen);
	if(addr.ss_family == AF_INET)
	{
	    ipv4 = (struct sockaddr_in *)&addr;
	    ipv4->sin_port = htons(port);
	}
#ifdef HAVE_IPV6
	if(addr.ss_family == AF_INET6)
	{
	    ipv6 = (struct sockaddr_in6 *)&addr;
	    ipv6->sin6_port = htons(port);
	}
#endif
	if(connect(fd, (struct sockaddr *)&addr, curhost->ai_addrlen))
	{
	    if(errno == EINPROGRESS)
	    {
		state = 0;
		break;
	    }
	    close(fd);
	    fd = -1;
	} else {
	    state = 1;
	    break;
	}
    }
    qcmd = makeqcmd(NULL);
    resetreader = 1;
    if(dchostname != NULL)
	free(dchostname);
    dchostname = sstrdup(host);
    if(freehost)
	free(host);
    return(fd);
}

struct dc_intresp *dc_interpret(struct dc_response *resp)
{
    int i;
    struct dc_intresp *iresp;
    struct command *cmd;
    struct respclass *cls;
    int code;
    int args;
    
    if((resp->numlines == 0) || (resp->rlines[0].argc == 0) || (resp->curline >= resp->numlines))
	return(NULL);
    code = wcstol(resp->rlines[0].argv[0], NULL, 10);
    cmd = (struct command *)(resp->internal);
    for(cls = cmd->classes; cls != NULL; cls = cls->next)
    {
	if(cls->code == code)
	    break;
    }
    if(cls == NULL)
	return(NULL);
    if(cls->nwords >= resp->rlines[resp->curline].argc)
	return(NULL);
    iresp = smalloc(sizeof(*iresp));
    iresp->code = code;
    iresp->argv = NULL;
    iresp->argc = 0;
    args = 0;
    for(i = 0; i < cls->nwords; i++)
    {
	switch(cls->wordt[i])
	{
	case RESP_DSC:
	    break;
	case RESP_STR:
	    sizebuf(&(iresp->argv), &args, iresp->argc + 1, sizeof(*(iresp->argv)), 1);
	    iresp->argv[iresp->argc].val.str = swcsdup(resp->rlines[resp->curline].argv[i + 1]);
	    iresp->argv[iresp->argc].type = cls->wordt[i];
	    iresp->argc++;
	    break;
	case RESP_INT:
	    sizebuf(&(iresp->argv), &args, iresp->argc + 1, sizeof(*(iresp->argv)), 1);
	    iresp->argv[iresp->argc].val.num = wcstol(resp->rlines[resp->curline].argv[i + 1], NULL, 0);
	    iresp->argv[iresp->argc].type = cls->wordt[i];
	    iresp->argc++;
	    break;
	case RESP_FLOAT:
	    sizebuf(&(iresp->argv), &args, iresp->argc + 1, sizeof(*(iresp->argv)), 1);
	    iresp->argv[iresp->argc].val.flnum = wcstod(resp->rlines[resp->curline].argv[i + 1], NULL);
	    iresp->argv[iresp->argc].type = cls->wordt[i];
	    iresp->argc++;
	    break;
	}
    }
    resp->curline++;
    return(iresp);
}

void dc_freeires(struct dc_intresp *ires)
{
    int i;
    
    for(i = 0; i < ires->argc; i++)
    {
	if(ires->argv[i].type == RESP_STR)
	    free(ires->argv[i].val.str);
    }
    free(ires->argv);
    free(ires);
}

const char *dc_gethostname(void)
{
    return(dchostname);
}
