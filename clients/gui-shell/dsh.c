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
#include <errno.h>
#include <pwd.h>
#include <locale.h>
#include <libintl.h>
#include <signal.h>
#include <sys/wait.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdarg.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_NOTIFY
#include <libnotify/notify.h>
#endif

#define _(text) gettext(text)

struct trinfo {
    int ostate;
};

void updatewrite(void);

GtkStatusIcon *tray;
pid_t dpid = 0, dcpid = 0;
int connected = 0;
int dstat;
int derrfd, derrtag;
char *derrbuf = NULL;
size_t derrbufsize = 0, derrbufdata = 0;
int dcfd, dcfdrtag, dcfdwtag = -1;
GdkPixbuf *dcicon;
#ifdef HAVE_NOTIFY
NotifyNotification *trnote = NULL;
#endif

#include "dsh-start.gtkh"
#include "dsh-menu.gtkh"

int running(char *pf)
{
    FILE *pfs;
    char buf[1024];
    int pid;
    
    if((pfs = fopen(pf, "r")) == NULL) {
	perror(pf);
	return(0);
    }
    fgets(buf, sizeof(buf), pfs);
    fclose(pfs);
    if((pid = atoi(buf)) == 0)
	return(0);
    return(!kill(pid, 0));
}

void derrcb(gpointer data, gint source, GdkInputCondition cond)
{
    int ret = 0;
    
    sizebuf2(derrbuf, derrbufdata + 1024, 1);
    ret = read(derrfd, derrbuf + derrbufdata, derrbufsize - derrbufdata);
    if(ret <= 0) {
	if(ret < 0)
	    bprintf(derrbuf, "\ncould not read from daemon: %s\n", strerror(errno));
	gdk_input_remove(derrtag);
	close(derrfd);
	derrfd = -1;
    } else {
	derrbufdata += ret;
    }
}

int msgbox(int type, int buttons, char *format, ...)
{
    GtkWidget *swnd;
    va_list args;
    char *buf;
    int resp;
    
    va_start(args, format);
    buf = vsprintf2(format, args);
    va_end(args);
    swnd = gtk_message_dialog_new(NULL, 0, type, buttons, "%s", buf);
    resp = gtk_dialog_run(GTK_DIALOG(swnd));
    gtk_widget_destroy(swnd);
    free(buf);
    return(resp);
}

void destroytr(struct dc_transfer *tr)
{
    struct trinfo *tri;
    
    tri = tr->udata;
    free(tri);
}

void inittr(struct dc_transfer *tr)
{
    struct trinfo *tri;
    
    tr->udata = tri = memset(smalloc(sizeof(*tri)), 0, sizeof(*tri));
    tr->destroycb = destroytr;
    tri->ostate = tr->state;
}

#ifdef HAVE_NOTIFY
void notify(NotifyNotification **n, char *cat, char *title, char *body, ...)
{
    va_list args;
    char *bbuf;
    
    va_start(args, body);
    bbuf = vsprintf2(body, args);
    va_end(args);
    if(*n == NULL) {
	*n = notify_notification_new_with_status_icon(title, bbuf, NULL, tray);
	notify_notification_set_icon_from_pixbuf(*n, dcicon);
    } else {
	notify_notification_update(*n, title, bbuf, NULL);
    }
    notify_notification_show(*n, NULL);
}
#endif

void trstatechange(struct dc_transfer *tr, int ostate)
{
    if(ostate == DC_TRNS_MAIN) {
	if(tr->state == DC_TRNS_DONE) {
#ifdef HAVE_NOTIFY
	    notify(&trnote, "transfer.complete", _("Transfer complete"), _("Finished downloading %ls from %ls"), tr->path, tr->peernick);
#endif
	} else {
#ifdef HAVE_NOTIFY
	    notify(&trnote, "transfer.error", _("Transfer interrupted"), _("The transfer of %ls from %ls was interrupted from the other side"), tr->path, tr->peernick);
#endif
	}
    }
}

void updatetrinfo(void)
{
    struct dc_transfer *tr;
    struct trinfo *tri;
    
    for(tr = dc_transfers; tr != NULL; tr = tr->next) {
	if(tr->udata == NULL) {
	    inittr(tr);
	} else {
	    tri = tr->udata;
	    if(tri->ostate != tr->state) {
		trstatechange(tr, tri->ostate);
		tri->ostate = tr->state;
	    }
	}
    }
}

void trlscb(int resp, void *data)
{
    updatetrinfo();
}

void logincb(int err, wchar_t *reason, void *data)
{
    if(err != DC_LOGIN_ERR_SUCCESS) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server"));
	exit(1);
    }
    gtk_status_icon_set_tooltip(tray, "Dolda Connect");
    dc_queuecmd(NULL, NULL, L"notify", L"trans:act", L"on", L"trans:prog", L"on", NULL);
    dc_gettrlistasync(trlscb, NULL);
    connected = 1;
    updatewrite();
}

void dcfdcb(gpointer data, gint source, GdkInputCondition cond)
{
    struct dc_response *resp;
    
    if(((cond & GDK_INPUT_READ) && dc_handleread()) || ((cond & GDK_INPUT_WRITE) && dc_handlewrite())) {
	if(errno == 0) {
	    gtk_main_quit();
	} else {
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server: %s"), strerror(errno));
	    exit(1);
	}
	return;
    }
    while((resp = dc_getresp()) != NULL) {
	if(!wcscmp(resp->cmdname, L".connect")) {
	    if(resp->code != 201) {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The server refused the connection"));
		exit(1);
	    } else if(dc_checkprotocol(resp, DC_LATEST)) {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Server protocol revision mismatch"));
		exit(1);
	    } else {
		gtk_status_icon_set_tooltip(tray, _("Authenticating..."));
		dc_loginasync(NULL, 1, dc_convnone, logincb, NULL);
	    }
	} else if(!wcscmp(resp->cmdname, L".notify")) {
	    dc_uimisc_handlenotify(resp);
	    updatetrinfo();
	}
    }
    updatewrite();
}

void updatewrite(void)
{
    if(dcfd < 0)
	return;
    if(dc_wantwrite()) {
	if(dcfdwtag == -1)
	    dcfdwtag = gdk_input_add(dcfd, GDK_INPUT_WRITE, dcfdcb, NULL);
    } else {
	if(dcfdwtag != -1) {
	    gdk_input_remove(dcfdwtag);
	    dcfdwtag = -1;
	}
    }
}

void connectdc(void)
{
    if((dcfd = dc_connect(dc_srv_local)) < 0) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server: %s"), strerror(errno));
	exit(1);
    }
    dcfdrtag = gdk_input_add(dcfd, GDK_INPUT_READ, dcfdcb, NULL);
    updatewrite();
    gtk_status_icon_set_tooltip(tray, _("Connecting to server..."));
}

void startdaemon(void)
{
    char pf[1024];
    int pfd[2], i;
    
    if(getenv("HOME") != NULL)
	snprintf(pf, sizeof(pf), "%s/.doldacond.pid", getenv("HOME"));
    else
	snprintf(pf, sizeof(pf), "%s/.doldacond.pid", getpwuid(getuid())->pw_dir);
    if(access(pf, F_OK) || !running(pf)) {
	pipe(pfd);
	if((dpid = fork()) == 0) {
	    dup2(pfd[1], 2);
	    for(i = 3; i < FD_SETSIZE; i++)
		close(i);
	    execlp("doldacond", "doldacond", "-p", pf, NULL);
	    perror("doldacond");
	    exit(127);
	}
	if(dpid == -1)
	    abort();
	close(pfd[1]);
	derrfd = pfd[0];
	derrtag = gdk_input_add(derrfd, GDK_INPUT_READ, derrcb, NULL);
	create_start_wnd();
	gtk_widget_show(start_wnd);
	gtk_status_icon_set_tooltip(tray, _("Starting..."));
    } else {
	connectdc();
    }
}

gboolean daemonized(gpointer uu)
{
    gtk_widget_hide(start_wnd);
    dpid = 0;
    if(derrfd != -1) {
	gdk_input_remove(derrtag);
	close(derrfd);
    }
    if(dstat != 0) {
	gtk_status_icon_set_visible(tray, FALSE);
	gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(start_log)), derrbuf, derrbufdata);
	gtk_widget_show(start_errwnd);
    } else {
	connectdc();
    }
    return(FALSE);
}

void sighandler(int sig)
{
    pid_t p;
    int status;
    
    if(sig == SIGCHLD) {
	while((p = waitpid(-1, &status, WNOHANG)) > 0) {
	    if(p == dpid) {
		dstat = status;
		gtk_timeout_add(1, daemonized, NULL);
	    } else if(p == dcpid) {
		dcpid = 0;
	    }
	}
    }
}

void dolcon(void)
{
    int i;
    
    if((dcpid = fork()) == 0) {
	for(i = 3; i < FD_SETSIZE; i++)
	    close(i);
	execlp("dolcon", "dolcon", "-l", NULL);
	perror("dolcon");
	exit(127);
    }
}

void cb_shm_dolcon_activate(GtkWidget *uu1, gpointer uu2)
{
    dolcon();
}

void cb_shm_quit_activate(GtkWidget *uu1, gpointer uu2)
{
    dc_queuecmd(NULL, NULL, L"shutdown", NULL);
    updatewrite();
}

void tray_activate(GtkStatusIcon *uu1, gpointer uu2)
{
    if(dpid != 0) {
	gtk_widget_show(start_wnd);
    } else if(connected) {
	dolcon();
    }
}

void tray_popup(GtkStatusIcon *uu1, guint button, guint time, gpointer uu2)
{
    gtk_menu_popup(GTK_MENU(shm_menu), NULL, NULL, NULL, NULL, button, time);
}

void cb_start_hide_clicked(GtkWidget *uu1, gpointer uu2)
{
    gtk_widget_hide(start_wnd);
}

void cb_start_abort_clicked(GtkWidget *uu1, gpointer uu2)
{
    kill(dpid, SIGINT);
    exit(0);
}

void cb_start_errok_clicked(GtkWidget *uu1, gpointer uu2)
{
    gtk_main_quit();
}

#include "../dolda-icon.xpm"

void inittray(void)
{
    tray = gtk_status_icon_new_from_pixbuf(gdk_pixbuf_scale_simple(dcicon, 24, 24, GDK_INTERP_BILINEAR));
    gtk_status_icon_set_tooltip(tray, "");
    g_signal_connect(G_OBJECT(tray), "activate", G_CALLBACK(tray_activate), (gpointer)NULL);
    g_signal_connect(G_OBJECT(tray), "popup-menu", G_CALLBACK(tray_popup), (gpointer)NULL);
}

int main(int argc, char **argv)
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    signal(SIGCHLD, sighandler);
    dc_init();
    gtk_init(&argc, &argv);
#ifdef HAVE_NOTIFY
    notify_init("Dolda Connect");
#endif

    create_shm_wnd();
    dcicon = gdk_pixbuf_new_from_xpm_data((const char **)dolda_icon_xpm);
    gtk_window_set_default_icon(dcicon);
    inittray();
    startdaemon();
    
    gtk_main();

    return(0);
}

#include "dsh-start.gtk"
#include "dsh-menu.gtk"
