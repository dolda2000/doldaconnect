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
#include <regex.h>
#include <signal.h>
#include <string.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include <errno.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "dolcon.h"
#include "hublist.h"

static regex_t *filter = NULL;
static pid_t fetchpid = 0;
static int listfd = -1;
static int iotag = -1;
static int (*handler)(int, char *, size_t) = NULL;
static char readbuf[65536];
static off_t bufpos = 0;

void aborthublist(void)
{
    if(fetchpid > 0) {
	gdk_input_remove(iotag);
	iotag = -1;
	close(listfd);
	listfd = -1;
	kill(fetchpid, SIGINT);
	fetchpid = 0;
	if(filter != NULL) {
	    regfree(filter);
	    free(filter);
	    filter = NULL;
	}
	if(handler != NULL) {
	    handler(PHO_FINI, NULL, 0);
	    handler = NULL;
	}
    }
}

int validhub(char *field, ...)
{
    int match;
    va_list args;
    
    if(filter == NULL)
	return(1);
    match = 0;
    va_start(args, field);
    do {
	if(!regexec(filter, field, 0, NULL, 0)) {
	    match = 1;
	    break;
	}
    } while((field = va_arg(args, char *)) != NULL);
    va_end(args);
    return(match);
}

static void readcb(gpointer data, gint source, GdkInputCondition cond)
{
    int ret;
    
    if(!(cond & GDK_INPUT_READ))
	return;
    if(bufpos == sizeof(readbuf))
	bufpos = 0;
    ret = read(listfd, readbuf + bufpos, sizeof(readbuf) - bufpos);
    if(ret <= 0) {
	if(ret < 0)
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not read from public hub listing process: %s"), strerror(errno));
	else
	    handler(PHO_EOF, NULL, 0);
	aborthublist();
    } else {
	bufpos += ret;
	if((ret = handler(PHO_DATA, readbuf, (size_t)bufpos)) < 0)
	    aborthublist();
	else
	    memmove(readbuf, readbuf + ret, bufpos -= ret);
    }
}

void fetchhublist(char *url, regex_t *flt)
{
    int pfd[2];
    int len;
    char *p;
    
    aborthublist();
    filter = flt;
    pipe(pfd);
    if((fetchpid = fork()) == 0) {
	dup2(pfd[1], 1);
	close(pfd[0]);
	close(pfd[1]);
	execlp("wget", "wget", "-qO", "-", url, NULL);
	perror("wget");
	exit(127);
    }
    close(pfd[1]);
    listfd = pfd[0];
    len = strlen(url);
    p = url + len;
    if((len > 4) && !strncmp(p - 4, ".bz2", 4)) {
	p -= 4;
	len -= 4;
	pipe(pfd);
	if(fork() == 0) {
	    dup2(listfd, 0);
	    dup2(pfd[1], 1);
	    close(listfd);
	    close(pfd[0]);
	    close(pfd[1]);
	    execlp("bzcat", "bzcat", NULL);
	    perror("bzcat");
	    exit(127);
	}
	close(listfd);
	close(pfd[1]);
	listfd = pfd[0];
    }
    if((len > 4) && !strncmp(p - 4, ".xml", 4)) {
	p -= 4;
	len -= 4;
	handler = pubhubxmlhandler;
    } else {
	handler = pubhuboldhandler;
    }
    bufpos = 0;
    handler(PHO_INIT, NULL, 0);
    iotag = gdk_input_add(listfd, GDK_INPUT_READ, readcb, NULL);
}
