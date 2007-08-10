/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2007 Fredrik Tolf <fredrik@dolda2000.com>
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
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/poll.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <utils.h>
#include <http.h>

#if 0
#define HTDEBUG(fmt...) fprintf(stderr, "httplib: " fmt)
#else
#define HTDEBUG(fmt...)
#endif

#define STATE_SYN 0
#define STATE_TXREQ 1
#define STATE_RXRES 2
#define STATE_RXBODY 3
#define STATE_RXCHLEN 4
#define STATE_RXCHUNK 5
#define STATE_DONE 6

void freeurl(struct hturlinfo *ui)
{
    free(ui->host);
    free(ui->path);
    free(ui->query);
    free(ui->fragment);
    free(ui);
}

struct hturlinfo *parseurl(char *url)
{
    char *p, *p2, *p3;
    struct hturlinfo *ui;
    
    if(strncmp(url, "http://", 7))
	return(NULL);
    ui = memset(smalloc(sizeof(*ui)), 0, sizeof(*ui));
    p = url + 7;
    if((p2 = strchr(p, '/')) != NULL)
	*(p2++) = 0;
    if((p3 = strrchr(p, ':')) != NULL) {
	*(p3++) = 0;
	ui->port = atoi(p3);
    } else {
	ui->port = 80;
    }
    ui->host = sstrdup(p);
    if(p2 == NULL) {
        ui->path = sstrdup("/");
    } else {
	p = p2;
	if((p2 = strchr(p, '?')) != NULL)
	    *(p2++) = 0;
	p[-1] = '/';
	ui->path = sstrdup(p - 1);
    }
    if(p2 == NULL) {
	ui->query = sstrdup("");
    } else {
	p = p2;
	if((p2 = strchr(p, '#')) != NULL)
	    *(p2++) = 0;
	ui->query = sstrdup(p);
    }
    if(p2 == NULL) {
	ui->fragment = sstrdup("");
    } else {
	ui->fragment = sstrdup(p2);
    }
    return(ui);
}

static struct hturlinfo *dupurl(struct hturlinfo *ui)
{
    struct hturlinfo *new;
    
    new = memset(smalloc(sizeof(*new)), 0, sizeof(*new));
    new->host = sstrdup(ui->host);
    new->port = ui->port;
    new->path = sstrdup(ui->path);
    new->query = sstrdup(ui->query);
    new->fragment = sstrdup(ui->fragment);
    return(new);
}

static struct addrinfo *resolvtcp(char *name, int port)
{
    struct addrinfo hint, *ret;
    char tmp[32];
    
    memset(&hint, 0, sizeof(hint));
    hint.ai_socktype = SOCK_STREAM;
    hint.ai_flags = AI_NUMERICSERV | AI_CANONNAME;
    snprintf(tmp, sizeof(tmp), "%i", port);
    if(!getaddrinfo(name, tmp, &hint, &ret))
	return(ret);
    return(NULL);
}

void freehtconn(struct htconn *cn)
{
    if(cn->outbuf != NULL)
	free(cn->outbuf);
    if(cn->inbuf != NULL)
	free(cn->inbuf);
    if(cn->databuf != NULL)
	free(cn->databuf);
    if(cn->resstr != NULL)
	free(cn->resstr);
    freeurl(cn->url);
    freeaddrinfo(cn->ailist);
    if(cn->fd != -1)
	close(cn->fd);
    free(cn);
}

struct htconn *htconnect(struct hturlinfo *ui)
{
    struct htconn *cn;
    
    cn = memset(smalloc(sizeof(*cn)), 0, sizeof(*cn));
    cn->fd = -1;
    cn->tlen = -1;
    cn->url = dupurl(ui);
    cn->ailist = resolvtcp(ui->host, ui->port);
    if(htprocess(cn, 0) < 0) {
	freehtconn(cn);
	return(NULL);
    }
    return(cn);
}

int htpollflags(struct htconn *cn)
{
    int ret;
    
    if(cn->fd == -1)
	return(0);
    ret = POLLIN;
    if((cn->state == STATE_SYN) || (cn->outbufdata > 0))
	ret |= POLLOUT;
    return(ret);
}

static char safechars[128] = {
 /* x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0,
};

static void consreq(struct htconn *cn)
{
    char *p;
    
    bufcat(cn->outbuf, "GET ", 4);
    for(p = cn->url->path; *p; p++) {
	if(!(*p & 0x80) && safechars[(int)*p])
	    addtobuf(cn->outbuf, *p);
	else
	    bprintf(cn->outbuf, "%%%02X", *p);
    }
    if(*(cn->url->query)) {
	addtobuf(cn->outbuf, '?');
	for(p = cn->url->path; *p; p++) {
	    if(!(*p & 0x80) && (safechars[(int)*p] || (*p == '&')))
		addtobuf(cn->outbuf, *p);
	    else
		bprintf(cn->outbuf, "%%%02X", *p);
	}
    }
    bufcat(cn->outbuf, " HTTP/1.1\r\n", 11);
    if(cn->url->port != 80)
	bprintf(cn->outbuf, "Host: %s:%i\r\n", cn->url->host, cn->url->port);
    else
	bprintf(cn->outbuf, "Host: %s\r\n", cn->url->host);
    bprintf(cn->outbuf, "User-Agent: DoldaConnect/%s\r\n", VERSION);
    bufcat(cn->outbuf, "\r\n", 2);
}

static void trimcr(char *buf)
{
    for(; *buf; buf++);
    if(*(--buf) == '\r')
	*buf = 0;
}

static int parseheaders(struct htconn *cn)
{
    char *p, *p2, *p3, *p4;
    
    while((p = memchr(cn->inbuf, '\n', cn->inbufdata)) != NULL) {
	*(p++) = 0;
	trimcr(cn->inbuf);
	if(!*(cn->inbuf)) {
	    memmove(cn->inbuf, p, cn->inbufdata -= (p - cn->inbuf));
	    return(1);
	}
	if((p2 = strchr(cn->inbuf, ':')) == NULL)
	    goto skip;
	*(p2++) = 0;
	for(p3 = cn->inbuf; isspace(*p3); p3++);
	if(!*p3)
	    goto skip;
	for(p4 = p2 - 2; isspace(*p4); p4--) {
	}
	*(++p4) = 0;
	for(; isspace(*p2); p2++);
	for(p4 = p3; *p4; p4++)
	    *p4 = tolower(*p4);
	newstrpair(p3, p2, &cn->headers);
	if(!strcmp(p3, "content-length"))
	    cn->tlen = atoi(p2);
    skip:
	memmove(cn->inbuf, p, cn->inbufdata -= (p - cn->inbuf));
    }
    return(0);
}

int htprocess(struct htconn *cn, int pollflags)
{
    int ret, done;
    socklen_t optlen;
    char rxbuf[1024];
    char *p, *p2, *p3;
    
    if(cn->state == STATE_SYN) {
	if(cn->fd != -1) {
	    optlen = sizeof(ret);
	    getsockopt(cn->fd, SOL_SOCKET, SO_ERROR, &ret, &optlen);
	    if(ret) {
		cn->fd = -1;
	    } else {
		consreq(cn);
		cn->state = STATE_TXREQ;
	    }
	}
	if(cn->fd == -1) {
	    if(cn->curai == NULL)
		cn->curai = cn->ailist;
	    else
		cn->curai = cn->curai->ai_next;
	    if(cn->curai == NULL) {
		/* Bleh! Linux and BSD don't share any good
		 * errno for this. */
		errno = ENOENT;
		return(-1);
	    }
	    if((cn->fd = socket(cn->curai->ai_family, cn->curai->ai_socktype, cn->curai->ai_protocol)) < 0)
		return(-1);
	    fcntl(cn->fd, F_SETFL, fcntl(cn->fd, F_GETFL) | O_NONBLOCK);
	    if(connect(cn->fd, cn->curai->ai_addr, cn->curai->ai_addrlen) < 0) {
		if(errno != EINPROGRESS)
		    return(-1);
	    } else {
		consreq(cn);
		cn->state = STATE_TXREQ;
	    }
	}
    }
    if(cn->state == STATE_TXREQ) {
	HTDEBUG("connected, sending request\n");
	if(pollflags & POLLIN) {
	    close(cn->fd);
	    cn->fd = -1;
	    return(-1);
	}
	if(pollflags & POLLOUT) {
	    if((ret = send(cn->fd, cn->outbuf, cn->outbufdata, MSG_DONTWAIT)) < 0) {
		if(errno != EAGAIN) {
		    close(cn->fd);
		    cn->fd = -1;
		    return(-1);
		}
	    } else {
		memmove(cn->outbuf, cn->outbuf + ret, cn->outbufdata -= ret);
		if(cn->outbufdata == 0)
		    cn->state = STATE_RXRES;
	    }
	}
    }
    /*
     * All further states will do receiving
     */
    if(pollflags & POLLIN) {
	if(cn->fd == -1) {
	    ret = 0;
	} else {
	    if((ret = recv(cn->fd, rxbuf, sizeof(rxbuf), MSG_DONTWAIT)) < 0) {
		HTDEBUG("error in recv: %s\n", strerror(errno));
		if(errno != EAGAIN) {
		    close(cn->fd);
		    cn->fd = -1;
		    return(-1);
		}
		return(0);
	    } else if(ret == 0) {
		HTDEBUG("EOF received\n");
		close(cn->fd);
		cn->fd = -1;
	    } else {
		bufcat(cn->inbuf, rxbuf, ret);
		HTDEBUG("received %i bytes of raw data, %i bytes in buffer\n", ret, cn->inbufdata);
	    }
	}
    }
    /* We need to loop until all processable data has been processed,
     * or we won't get called again */
    do {
	done = 1;
	if(cn->state == STATE_RXRES) {
	    if(ret == 0) {
		if(cn->rescode == 0) {
		    HTDEBUG("received EOF before response, flaggin EPROTO\n");
		    errno = EPROTO;
		    return(-1);
		}
		HTDEBUG("EOF after headers, no body\n");
		cn->state = STATE_DONE;
	    } else {
		/* Headers shouldn't be this long! */
		if(cn->inbufdata >= 65536) {
		    HTDEBUG("got suspiciously long headers, flagging ENOMEM\n");
		    close(cn->fd);
		    cn->fd = -1;
		    errno = ENOMEM;
		    return(-1);
		}
		HTDEBUG("received some header data\n");
	    }
	    if(cn->rescode == 0) {
		if((p = memchr(cn->inbuf, '\n', cn->inbufdata)) != NULL) {
		    HTDEBUG("received response line\n");
		    *(p++) = 0;
		    trimcr(cn->inbuf);
		    p2 = cn->inbuf;
		    if((p3 = strchr(p2, ' ')) == NULL) {
			close(cn->fd);
			cn->fd = -1;
			errno = EPROTO;
			return(-1);
		    }
		    *(p3++) = 0;
		    if(strncmp(p2, "HTTP/", 5)) {
			close(cn->fd);
			cn->fd = -1;
			errno = EPROTO;
			return(-1);
		    }
		    p2 = p3;
		    if((p3 = strchr(p2, ' ')) == NULL) {
			close(cn->fd);
			cn->fd = -1;
			errno = EPROTO;
			return(-1);
		    }
		    *(p3++) = 0;
		    cn->rescode = atoi(p2);
		    if((cn->rescode < 100) || (cn->rescode >= 1000)) {
			close(cn->fd);
			cn->fd = -1;
			errno = EPROTO;
			return(-1);
		    }
		    cn->resstr = sstrdup(p3);
		    memmove(cn->inbuf, p, cn->inbufdata -= (p - cn->inbuf));
		    HTDEBUG("parsed response line (%i, %s)\n", cn->rescode, cn->resstr);
		}
	    }
	    if(cn->rescode != 0) {
		HTDEBUG("parsing some headers\n");
		if(parseheaders(cn)) {
		    HTDEBUG("all headers received\n");
		    if(((p = spfind(cn->headers, "transfer-encoding")) != NULL) && !strcasecmp(p, "chunked")) {
			HTDEBUG("doing chunky decoding\n");
			cn->chl = -1;
			cn->state = STATE_RXCHLEN;
		    } else {
			HTDEBUG("receiving normally\n");
			cn->state = STATE_RXBODY;
		    }
		}
	    }
	}
	if(cn->state == STATE_RXBODY) {
	    if(ret == 0) {
		HTDEBUG("EOF in body, flagging as done\n");
		cn->state = STATE_DONE;
	    } else {
		bufcat(cn->databuf, cn->inbuf, cn->inbufdata);
		HTDEBUG("transferred %i bytes from inbuf to databuf, %i bytes now in databuf\n", cn->inbufdata, cn->databufdata);
		cn->rxd += cn->inbufdata;
		cn->inbufdata = 0;
		if((cn->tlen != -1) && (cn->rxd >= cn->tlen)) {
		    HTDEBUG("received Content-Length, flagging as done\n");
		    cn->state = STATE_DONE;
		}
	    }
	}
	if(cn->state == STATE_RXCHLEN) {
	    HTDEBUG("trying to parse chunk length\n");
	    while(((p = memchr(cn->inbuf, '\n', cn->inbufdata)) != NULL) && (cn->chl == -1)) {
		*(p++) = 0;
		trimcr(cn->inbuf);
		HTDEBUG("trimmed chunk line: %s\n", cn->inbuf);
		if(!*cn->inbuf)
		    goto skip;
		cn->chl = strtol(cn->inbuf, NULL, 16);
		HTDEBUG("parsed chunk length: %i\n", cn->chl);
	    skip:
		memmove(cn->inbuf, p, cn->inbufdata -= (p - cn->inbuf));
	    }
	    if(cn->chl == 0) {
		HTDEBUG("zero chunk length, looking for CRLF\n");
		if((cn->inbuf[0] == '\r') && (cn->inbuf[1] == '\n')) {
		    HTDEBUG("ending CRLF gotten, flagging as done\n");
		    cn->state = STATE_DONE;
		}
	    } else {
		HTDEBUG("will read chunk\n");
		cn->state = STATE_RXCHUNK;
	    }
	}
	if(cn->state == STATE_RXCHUNK) {
	    if(cn->inbufdata >= cn->chl) {
		bufcat(cn->databuf, cn->inbuf, cn->chl);
		memmove(cn->inbuf, cn->inbuf + cn->chl, cn->inbufdata -= cn->chl);
		HTDEBUG("received final %i bytes of chunk, inbuf %i bytes, databuf %i bytes\n", cn->chl, cn->inbufdata, cn->databufdata);
		cn->rxd += cn->chl;
		cn->chl = 0;
		cn->state = STATE_RXCHLEN;
		done = 0;
	    } else {
		bufcat(cn->databuf, cn->inbuf, cn->inbufdata);
		cn->chl -= cn->inbufdata;
		cn->rxd += cn->inbufdata;
		HTDEBUG("received %i bytes of chunk, %i bytes remaining, %i bytes in databuf\n", cn->inbufdata, cn->chl, cn->databufdata);
		cn->inbufdata = 0;
	    }
	}
    } while(!done);
    return((cn->state == STATE_DONE)?1:0);
}
