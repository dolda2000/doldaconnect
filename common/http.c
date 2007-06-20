/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2007 Fredrik Tolf (fredrik@dolda2000.com)
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
#include <sys/socket.h>
#include <sys/poll.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <utils.h>
#include <http.h>

#define STATE_SYN 0
#define STATE_TXREQ 1

void freeurl(struct hturlinfo *ui)
{
    free(ui->host);
    free(ui->path);
    free(ui->query);
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
	ui->path = sstrdup(p);
    }
    if(p2 == NULL) {
	ui->query = sstrdup("");
    } else {
	ui->query = sstrdup(p2);
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
    cn->url = dupurl(ui);
    cn->ailist = resolvtcp(ui->host, ui->port);
    return(cn);
}

int htpollflags(struct htconn *hc)
{
    int ret;
    
    ret = POLLIN;
    if(hc->outbufdata > 0)
	ret |= POLLOUT;
    return(ret);
}

int htprocess(struct htconn *hc)
{
    int ret;
    socklen_t optlen;
    
    if(hc->state == STATE_SYN) {
	if(hc->fd != -1) {
	    optlen = sizeof(ret);
	    getsockopt(hc->fd, SOL_SOCKET, SO_ERROR, &ret, &optlen);
	    if(ret) {
		hc->fd = -1;
	    } else {
		hc->state = STATE_TXREQ;
	    }
	}
	if(hc->fd == -1) {
	    if(hc->curai == NULL)
		hc->curai = hc->ailist;
	    else
		hc->curai = hc->curai->ai_next;
	    if(hc->curai == NULL) {
		/* Bleh! Linux and BSD don't share any good
		 * errno for this. */
		errno = ENOENT;
		return(-1);
	    }
	}
    }
    return(0);
}
