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
#include <bzlib.h>
#include <errno.h>
#include <sys/poll.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "dolcon.h"
#include "hublist.h"
#include <http.h>

static void settags(void);

static regex_t *filter = NULL;
static int itag = -1, otag = -1;
static int (*handler)(int, char *, size_t) = NULL;
static int state;
static struct htconn *hc;
static bz_stream *bzs;
/* Only needed because of bzlib: */
static char *mybuf;
static size_t mybufsize, mybufdata;

#include "mainwnd.gtkh"

void aborthublist(void)
{
    if(mybuf != NULL) {
	free(mybuf);
	mybuf = NULL;
	mybufsize = mybufdata = 0;
    }
    if(hc != NULL) {
	gtk_widget_hide(main_pubhubbarbox);
	if(itag != -1)
	    gdk_input_remove(itag);
	if(otag != -1)
	    gdk_input_remove(otag);
	itag = otag = -1;
	freehtconn(hc);
	hc = NULL;
	if(bzs != NULL) {
	    BZ2_bzDecompressEnd(bzs);
	    free(bzs);
	    bzs = NULL;
	}
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

static void fdcb(gpointer data, gint source, GdkInputCondition cond)
{
    int ret, bzret, hret;
    
    if(itag != -1)
	gdk_input_remove(itag);
    if(otag != -1)
	gdk_input_remove(otag);
    itag = otag = -1;
    ret = htprocess(hc, ((cond & GDK_INPUT_READ)?POLLIN:0) | ((cond & GDK_INPUT_WRITE)?POLLOUT:0));
    if(ret < 0) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not read hublist from server: %s"), strerror(errno));
	aborthublist();
	return;
    }
    if(state == 0) {
	if(hc->rescode != 0) {
	    if(hc->rescode != 200) {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The hublist server returned an error: \"%i %s\""), hc->rescode, hc->resstr);
		aborthublist();
		return;
	    }
	    state = 1;
	    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(main_pubhubbar), _("Getting list..."));
	}
    }
    if(state == 1) {
	if(hc->tlen > 0) {
	    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(main_pubhubbar), ((double)hc->rxd) / ((double)hc->tlen));
	} else {
	    gtk_progress_bar_set_pulse_step(GTK_PROGRESS_BAR(main_pubhubbar), ((double)hc->databufdata) / 10000.0);
	    gtk_progress_bar_pulse(GTK_PROGRESS_BAR(main_pubhubbar));
	}
	if(hc->databufdata > 0) {
	    if(bzs == NULL) {
		bufcat(mybuf, hc->databuf, hc->databufdata);
		hc->databufdata = 0;
	    } else {
		bzs->next_in = hc->databuf;
		bzs->avail_in = hc->databufdata;
		do {
		    sizebuf2(mybuf, mybufdata + 1024, 1);
		    bzs->next_out = mybuf + mybufdata;
		    bzs->avail_out = mybufsize - mybufdata;
		    bzret = BZ2_bzDecompress(bzs);
		    if((bzret != BZ_OK) && (bzret != BZ_STREAM_END)) {
			msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not decompress hublist (%i)"), hret);
			aborthublist();
			return;
		    }
		    mybufdata = mybufsize - bzs->avail_out;
		    if(bzret == BZ_STREAM_END) {
			ret = 1;
			break;
		    }
		} while(bzs->avail_out == 0);
		memmove(hc->databuf, bzs->next_in, hc->databufdata -= (bzs->next_in - hc->databuf));
	    }
	    if((hret = handler(PHO_DATA, mybuf, mybufdata)) < 0)
		aborthublist();
	    else
		memmove(mybuf, mybuf + hret, mybufdata -= hret);
	}
	if(ret) {
	    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(main_pubhubbar), 1);
	    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(main_pubhubbar), _("Finalizing list..."));
	    gdk_window_process_updates(main_pubhubbar->window, FALSE);
	    handler(PHO_EOF, NULL, 0);
	    aborthublist();
	}
    }
    if(hc != NULL)
	settags();
}

static void settags(void)
{
    if(htpollflags(hc) & POLLIN)
	itag = gdk_input_add(hc->fd, GDK_INPUT_READ, fdcb, NULL);
    if(htpollflags(hc) & POLLOUT)
	otag = gdk_input_add(hc->fd, GDK_INPUT_WRITE, fdcb, NULL);
}

void fetchhublist(char *url, regex_t *flt)
{
    int len;
    char *p;
    struct hturlinfo *u;
    
    aborthublist();
    filter = flt;
    u = parseurl(url);
    hc = htconnect(u);
    freeurl(u);
    if(hc == NULL) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not read hublist from server: %s"), strerror(errno));
	return;
    }
    state = 0;
    settags();
    gtk_widget_show(main_pubhubbarbox);
    gtk_progress_bar_set_text(GTK_PROGRESS_BAR(main_pubhubbar), _("Connecting..."));
    gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(main_pubhubbar), 0);
    gdk_window_process_updates(main_pubhubbarbox->window, TRUE);

    len = strlen(url);
    p = url + len;
    if((len > 4) && !strncmp(p - 4, ".bz2", 4)) {
	p -= 4;
	len -= 4;
	bzs = memset(smalloc(sizeof(*bzs)), 0, sizeof(*bzs));
	if(BZ2_bzDecompressInit(bzs, 0, 0) != BZ_OK) {
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not initialize decompression library"));
	    free(bzs);
	    bzs = NULL;
	    aborthublist();
	    return;
	}
    }
    if((len > 4) && !strncmp(p - 4, ".xml", 4)) {
	p -= 4;
	len -= 4;
	handler = pubhubxmlhandler;
    } else {
	handler = pubhuboldhandler;
    }
    handler(PHO_INIT, NULL, 0);
}
