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
#include <sys/poll.h>
#include <errno.h>

#include <utils.h>
#include <http.h>

void parse(char *url)
{
    struct hturlinfo *u;
    
    if((u = parseurl(url)) == NULL) {
	fprintf(stderr, "httest: %s: invalid url\n", url);
	return;
    }
    printf("host: %s\n", u->host);
    printf("port: %i\n", u->port);
    printf("path: %s\n", u->path);
    printf("query: %s\n", u->query);
    printf("fragment: %s\n", u->fragment);
    freeurl(u);
}

void head(char *url)
{
    struct hturlinfo *u;
    struct htconn *c;
    struct pollfd pfd;
    struct strpair *p;
    int ret;
    
    if((u = parseurl(url)) == NULL) {
	fprintf(stderr, "httest: %s: invalid url\n", url);
	return;
    }
    c = htconnect(u);
    freeurl(u);
    while(1) {
	pfd.fd = c->fd;
	pfd.events = htpollflags(c);
	if(poll(&pfd, 1, -1) < 0) {
	    fprintf(stderr, "httest: %s: %s\n", url, strerror(errno));
	    freehtconn(c);
	    return;
	}
	if((ret = htprocess(c, pfd.revents)) < 0) {
	    fprintf(stderr, "httest: %s: %s\n", url, strerror(errno));
	    freehtconn(c);
	    return;
	}
	c->databufdata = 0;
	if(ret)
	    break;
    }
    printf("%i %s\n", c->rescode, c->resstr);
    for(p = c->headers; p != NULL; p = p->next)
	printf("%s: %s\n", p->key, p->val);
    freehtconn(c);
}

void get(char *url)
{
    struct hturlinfo *u;
    struct htconn *c;
    struct pollfd pfd;
    struct strpair *p;
    int ret, ret2;
    
    if((u = parseurl(url)) == NULL) {
	fprintf(stderr, "httest: %s: invalid url\n", url);
	return;
    }
    c = htconnect(u);
    freeurl(u);
    while(1) {
	pfd.fd = c->fd;
	pfd.events = htpollflags(c);
	if(poll(&pfd, 1, -1) < 0) {
	    fprintf(stderr, "httest: %s: %s\n", url, strerror(errno));
	    freehtconn(c);
	    return;
	}
	if((ret = htprocess(c, pfd.revents)) < 0) {
	    fprintf(stderr, "httest: %s: %s\n", url, strerror(errno));
	    freehtconn(c);
	    return;
	}
	while(c->databufdata > 0) {
	    ret2 = write(1, c->databuf, c->databufdata);
	    memmove(c->databuf, c->databuf + ret2, c->databufdata -= ret2);
	}
	if(ret)
	    break;
    }
    printf("%i %s\n", c->rescode, c->resstr);
    for(p = c->headers; p != NULL; p = p->next)
	printf("%s: %s\n", p->key, p->val);
    freehtconn(c);
}

int main(int argc, char **argv) {
    int i;
    
    for(i = 1; i < argc; i++) {
	if(!strcmp(argv[i], "parse")) {
	    parse(argv[++i]);
	} else if(!strcmp(argv[i], "head")) {
	    head(argv[++i]);
	} else if(!strcmp(argv[i], "get")) {
	    get(argv[++i]);
	}
    }
    return(0);
}
