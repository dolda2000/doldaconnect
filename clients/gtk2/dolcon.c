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

/* This file is a complete and total mess, mostly because of my
 * inability to structure GUI programs properly. Looking at it too
 * closely may cause ocular hemorrhaging. */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <gdk/gdkkeysyms.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>
#include <errno.h>
#include <regex.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <pwd.h>
#include <locale.h>
#include <assert.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "dolcon.h"
#include "hublist.h"

#define TRHISTSIZE 10

struct trdata
{
    dc_lnum_t poshist[TRHISTSIZE];
    double timehist[TRHISTSIZE];
    int hc;
};

struct fndata
{
    GtkTextBuffer *textbuf;
};

struct srchsize
{
    gint64 size;
    int num;
    int slots;
    double resptime;
    GtkTreeRowReference *ref;
};

struct knownspeed
{
    char *userid;
    int speed, seq;
    time_t fetched;
};

GtkWidget *inpdialog;
GtkListStore *fnmodel, *ulmodel, *dlmodel, *reslist;
GtkTreeStore *srchmodel;
GtkTreeModelFilter *srchmodelfilter;
GtkTextTagTable *chattags;
int dcfd = -1, gdkread = -1, gdkwrite = -1;
int curchat = -1;
char *pubhubaddr = NULL;
char *connectas = NULL;
char *dcserver = NULL;
int autoconn = 1;
int srchautoupdate = 0;
int cursrch = -1, nextsrch = -1;
time_t srcheta;
struct srchsize *srchsizes = NULL;
struct knownspeed *knownspeeds = NULL;
int numsizes = 0, numspeeds = 0, ksqueryseq = -1, ksquerytag = -1, lsrestag = -1;

void dcfdcallback(gpointer data, gint source, GdkInputCondition condition);
void srchstatupdate(void);

#include "mainwnd.gtkh"
#include "inpdialog.gtkh"
#include "pref.gtkh"
#include "reslist.gtkh"

void updatewrite(void)
{
    if(dcfd < 0)
	return;
    if(dc_wantwrite())
    {
	if(gdkwrite == -1)
	    gdkwrite = gdk_input_add(dcfd, GDK_INPUT_WRITE, dcfdcallback, NULL);
    } else {
	if(gdkwrite != -1)
	{
	    gdk_input_remove(gdkwrite);
	    gdkwrite = -1;
	}
    }
}

double ntime(void)
{
    struct timeval tv;
    
    gettimeofday(&tv, NULL);
    return((double)tv.tv_sec + ((double)tv.tv_usec / 1000000.0));
}

void fndestroycb(struct dc_fnetnode *fn)
{
    struct fndata *data;
    GtkTextBuffer *textbuf;
    
    data = fn->udata;
    g_object_unref(data->textbuf);
    free(data);
    if(curchat == fn->id)
    {
	textbuf = gtk_text_buffer_new(chattags);
	gtk_text_view_set_buffer(GTK_TEXT_VIEW(main_chatview), textbuf);
	g_object_unref(textbuf);
    }
}

void addfndata(struct dc_fnetnode *fn)
{
    struct fndata *data;
    
    if(fn->udata != NULL)
	return;
    fn->destroycb = fndestroycb;
    data = smalloc(sizeof(*data));
    data->textbuf = gtk_text_buffer_new(chattags);
    fn->udata = data;
}

void trdestroycb(struct dc_transfer *tr)
{
    free(tr->udata);
}

void addtrdata(struct dc_transfer *tr)
{
    struct trdata *data;
    
    if(tr->udata != NULL)
	return;
    tr->destroycb = trdestroycb;
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    tr->udata = data;
}

void updatetrdata(struct dc_transfer *tr)
{
    int i;
    struct trdata *data;
    
    data = tr->udata;
    if(data->hc < TRHISTSIZE)
    {
	data->poshist[data->hc] = tr->curpos;
	data->timehist[data->hc] = ntime();
	data->hc++;
    } else {
	for(i = 0; i < TRHISTSIZE - 1; i++)
	{
	    data->poshist[i] = data->poshist[i + 1];
	    data->timehist[i] = data->timehist[i + 1];
	}
	data->poshist[i] = tr->curpos;
	data->timehist[i] = ntime();
    }
}

char *getfnstatestock(int state)
{
    if(state == DC_FNN_STATE_SYN)
	return("gtk-jump-to");
    if(state == DC_FNN_STATE_HS)
	return("gtk-execute");
    if(state == DC_FNN_STATE_EST)
	return("gtk-yes");
    if(state == DC_FNN_STATE_DEAD)
	return("gtk-cancel");
    return(NULL);
}

void updatehublist(void)
{
    int done;
    struct dc_fnetnode *fn;
    GtkTreeIter iter;
    int id;
    char *buf;
    char *name;
    int state, numusers;
    
    for(fn = dc_fnetnodes; fn != NULL; fn = fn->next)
	fn->found = 0;
    done = 0;
    while(!done)
    {
	done = 1;
	if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(fnmodel), &iter))
	{
	    do
	    {
		gtk_tree_model_get(GTK_TREE_MODEL(fnmodel), &iter, 0, &id, -1);
		if((fn = dc_findfnetnode(id)) == NULL)
		{
		    /* I can't seem to get a sensible reply fromp
		     * gtk_list_store, so I'm just doing this
		     * instead. */
		    gtk_list_store_remove(fnmodel, &iter);
		    done = 0;
		    break;
		} else {
		    gtk_tree_model_get(GTK_TREE_MODEL(fnmodel), &iter, 1, &name, 2, &state, 3, &numusers, -1);
		    if(fn->name == NULL)
			buf = _("Unknown");
		    else
			buf = icswcstombs(fn->name, "UTF-8", NULL);
		    if(strcmp(buf, name))
			gtk_list_store_set(fnmodel, &iter, 1, buf, -1);
		    if(state != fn->state)
		    {
			gtk_list_store_set(fnmodel, &iter, 2, fn->state, -1);
			gtk_list_store_set(fnmodel, &iter, 4, getfnstatestock(fn->state), -1);
		    }
		    if(numusers != fn->numusers)
			gtk_list_store_set(fnmodel, &iter, 3, fn->numusers, -1);
		    g_free(name);
		    fn->found = 1;
		}
	    } while(gtk_tree_model_iter_next(GTK_TREE_MODEL(fnmodel), &iter));
	}
    }
    for(fn = dc_fnetnodes; fn != NULL; fn = fn->next)
    {
	if(!fn->found)
	{
	    if(fn->name == NULL)
		buf = _("Unknown");
	    else
		buf = icswcstombs(fn->name, "UTF-8", NULL);
	    gtk_list_store_append(fnmodel, &iter);
	    gtk_list_store_set(fnmodel, &iter, 0, fn->id, 1, buf, 2, fn->state, 3, fn->numusers, 4, getfnstatestock(fn->state), -1);
	    addfndata(fn);
	}
    }
}

char *bytes2si(long long bytes)
{
    int i;
    double b;
    static char ret[64];
    static char pfx[] = {'k', 'M', 'G', 'T'};
    
    b = bytes;
    for(i = 0; (b >= 1024) && (i < sizeof(pfx)); i++)
	b /= 1024;
    if(i == 0)
	snprintf(ret, 64, "%.1f B", b);
    else
	snprintf(ret, 64, "%.1f %ciB", b, pfx[i - 1]);
    return(ret);
}

void progressfunc(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int totalc, curc;
    gint64 total, cur;
    char buf[64];
    
    totalc = (GPOINTER_TO_INT(data) & 0xff00) >> 8;
    curc = GPOINTER_TO_INT(data) & 0xff;
    gtk_tree_model_get(model, iter, totalc, &total, curc, &cur, -1);
    if(total < 1)
	g_object_set(rend, "value", GINT_TO_POINTER(0), NULL);
    else
	g_object_set(rend, "value", GINT_TO_POINTER((int)(((double)cur / (double)total) * 100)), NULL);
    if(cur < 0) {
	g_object_set(rend, "text", "", NULL);
    } else {
	snprintf(buf, 64, "%'ji", (intmax_t)cur);
	g_object_set(rend, "text", buf, NULL);
    }
}

void percentagefunc(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int colnum;
    float val;
    char buf[64];
    
    colnum = GPOINTER_TO_INT(data);
    gtk_tree_model_get(model, iter, colnum, &val, -1);
    snprintf(buf, 64, "%.2f%%", (double)(val * 100.0));
    g_object_set(rend, "text", buf, NULL);
}

void transnicebytefunc(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int colnum;
    gint64 val;
    char buf[64];
    
    colnum = GPOINTER_TO_INT(data);
    gtk_tree_model_get(model, iter, colnum, &val, -1);
    if(val >= 0)
	snprintf(buf, 64, "%'ji", (intmax_t)val);
    else
	strcpy(buf, _("Unknown"));
    g_object_set(rend, "text", buf, NULL);
}

void transnicebytefunc2(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int colnum;
    gint64 val;
    char buf[64];
    
    colnum = GPOINTER_TO_INT(data);
    gtk_tree_model_get(model, iter, colnum, &val, -1);
    if(val >= 0)
	strcpy(buf, bytes2si(val));
    else
	strcpy(buf, _("Unknown"));
    g_object_set(rend, "text", buf, NULL);
}

void hidezerofunc(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int colnum, val;
    char buf[64];
    
    colnum = GPOINTER_TO_INT(data);
    gtk_tree_model_get(model, iter, colnum, &val, -1);
    if(val > 0)
	snprintf(buf, 64, "%i", val);
    else
	strcpy(buf, "");
    g_object_set(rend, "text", buf, NULL);
}

void speedtimefunc(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int speed, time;
    gint64 size;
    char buf[64];
    
    gtk_tree_model_get(model, iter, 4, &size, 8, &speed, -1);
    if(speed > 0)
    {
	time = (size / speed) / 60;
	if(time < 1)
	    snprintf(buf, 64, "%'i (<00:01)", speed);
	else
	    snprintf(buf, 64, "%'i (%02i:%02i)", speed, time / 60, time % 60);
    } else if(speed == 0) {
	strcpy(buf, "0");
    } else {
	strcpy(buf, _("Unknown"));
    }
    g_object_set(rend, "text", buf, NULL);
}

void transspeedinfo(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int id;
    struct dc_transfer *tr;
    struct trdata *d;
    char buf[64];
    int speed;
    
    gtk_tree_model_get(model, iter, 0, &id, -1);
    if((tr = dc_findtransfer(id)) != NULL)
    {
	d = tr->udata;
	if((tr->state != DC_TRNS_MAIN) || (d == NULL))
	{
	    buf[0] = 0;
	} else if(d->hc < 2) {
	    strcpy(buf, "...");
	} else {
	    speed = (((double)(d->poshist[d->hc - 1] - d->poshist[0])) / (d->timehist[d->hc - 1] - d->timehist[0]));
	    snprintf(buf, 64, "%s/s", bytes2si(speed));
	}
    } else {
	buf[0] = 0;
    }
    g_object_set(rend, "text", buf, NULL);
}

void transerrorinfo(GtkTreeViewColumn *col, GtkCellRenderer *rend, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int error;
    time_t errortime;
    char finbuf[64], tbuf[64], *errstr;
    
    gtk_tree_model_get(model, iter, 10, &error, 11, &errortime, -1);
    if(error != DC_TRNSE_NOERROR)
    {
	if(error == DC_TRNSE_NOTFOUND)
	    errstr = _("Not found");
	else if(error == DC_TRNSE_NOSLOTS)
	    errstr = _("No slots");
	strftime(tbuf, 64, _("%H:%M:%S"), localtime(&errortime));
	snprintf(finbuf, 64, _("%s (reported at %s)"), errstr, tbuf);
    } else {
	*finbuf = 0;
    }
    g_object_set(rend, "text", finbuf, NULL);
}

char *gettrstatestock(int state)
{
    if(state == DC_TRNS_WAITING)
	return("gtk-jump-to");
    if(state == DC_TRNS_HS)
	return("gtk-execute");
    if(state == DC_TRNS_MAIN)
	return("gtk-network");
    if(state == DC_TRNS_DONE)
	return("gtk-yes");
    return(NULL);
}

gint updatetransfers(gpointer data)
{
    struct dc_transfer *tr;
    struct trdata *d;
    double now;
    
    now = ntime();
    for(tr = dc_transfers; tr != NULL; tr = tr->next)
    {
	if((d = tr->udata) != NULL)
	{
	    if((d->hc > 0) && ((now - d->timehist[d->hc - 1]) > 2))
		updatetrdata(tr);
	}
    }
    return(TRUE);
}

void updatetransferlists(void)
{
    int i;
    int done;
    struct dc_transfer *transfer;
    GtkTreeIter iter;
    int id;
    char *buf;
    char *peerid, *peernick, *path, *hash;
    int state, dir, error;
    gint64 size, curpos;
    time_t errortime;
    GtkListStore *stores[3];
    
    for(transfer = dc_transfers; transfer != NULL; transfer = transfer->next)
	transfer->found = 0;
    stores[DC_TRNSD_UNKNOWN] = NULL;
    stores[DC_TRNSD_UP] = ulmodel;
    stores[DC_TRNSD_DOWN] = dlmodel;
    for(i = 0; i < 3; i++)
    {
	if(stores[i] == NULL)
	    continue;
	done = 0;
	while(!done)
	{
	    done = 1;
	    if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(stores[i]), &iter))
	    {
		do
		{
		    gtk_tree_model_get(GTK_TREE_MODEL(stores[i]), &iter, 0, &id, 1, &dir, -1);
		    if(((transfer = dc_findtransfer(id)) == NULL) || (transfer->dir != dir))
		    {
			gtk_list_store_remove(stores[i], &iter);
			done = 0;
			break;
		    } else {
			transfer->found = 1;
			gtk_tree_model_get(GTK_TREE_MODEL(stores[i]), &iter, 2, &state, 3, &peerid, 4, &peernick, 5, &path, 6, &size, 7, &curpos, 10, &error, 11, &errortime, 12, &hash, -1);
			if(state != transfer->state)
			    gtk_list_store_set(stores[i], &iter, 2, transfer->state, 8, gettrstatestock(transfer->state), -1);
			if(size != transfer->size)
			    gtk_list_store_set(stores[i], &iter, 6, (gint64)transfer->size, -1);
			if(curpos != transfer->curpos)
			{
			    gtk_list_store_set(stores[i], &iter, 7, (gint64)transfer->curpos, -1);
			    if(transfer->udata != NULL)
				updatetrdata(transfer);
			}
			if(error != transfer->error)
			    gtk_list_store_set(stores[i], &iter, 10, transfer->error, -1);
			if(errortime != transfer->errortime)
			    gtk_list_store_set(stores[i], &iter, 11, transfer->errortime, -1);
			if((transfer->size > 0) && (transfer->curpos > 0))
			    gtk_list_store_set(stores[i], &iter, 9, (float)transfer->curpos / (float)transfer->size, -1);
			buf = icswcstombs(transfer->peerid, "UTF-8", NULL);
			if(strcmp(buf, peerid))
			    gtk_list_store_set(stores[i], &iter, 3, buf, -1);
			buf = icswcstombs(((transfer->peernick == NULL) || (transfer->peernick[0] == L'\0'))?transfer->peerid:transfer->peernick, "UTF-8", NULL);
			if(strcmp(buf, peernick))
			    gtk_list_store_set(stores[i], &iter, 4, buf, -1);
			buf = (transfer->path == NULL)?_("Unknown"):icswcstombs(transfer->path, "UTF-8", NULL);
			if(strcmp(buf, path))
			    gtk_list_store_set(stores[i], &iter, 5, buf, -1);
			buf = (transfer->hash == NULL)?"":icswcstombs(transfer->hash, "UTF-8", NULL);
			if(strcmp(buf, path))
			    gtk_list_store_set(stores[i], &iter, 12, buf, -1);
			g_free(hash);
			g_free(peerid);
			g_free(peernick);
			g_free(path);
		    }
		} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(stores[i]), &iter));
	    }
	}
    }
    for(transfer = dc_transfers; transfer != NULL; transfer = transfer->next)
    {
	if(!transfer->found)
	{
	    if(stores[transfer->dir] != NULL)
	    {
		peerid = icwcstombs(transfer->peerid, "UTF-8");
		peernick = icwcstombs(((transfer->peernick == NULL) || (transfer->peernick[0] == L'\0'))?transfer->peerid:transfer->peernick, "UTF-8");
		path = (transfer->path == NULL)?_("Unknown"):icwcstombs(transfer->path, "UTF-8");
		hash = (transfer->hash == NULL)?"":icwcstombs(transfer->hash, "UTF-8");
		gtk_list_store_append(stores[transfer->dir], &iter);
		gtk_list_store_set(stores[transfer->dir], &iter,
				   0, transfer->id,
				   1, transfer->dir,
				   2, transfer->state,
				   3, peerid,
				   4, peernick,
				   5, path,
				   6, (gint64)transfer->size,
				   7, (gint64)transfer->curpos,
				   8, gettrstatestock(transfer->state),
				   9, 0.0,
				   10, transfer->error,
				   11, transfer->errortime,
				   12, hash,
				   -1);
		free(peerid);
		free(peernick);
		if(transfer->path != NULL)
		    free(path);
		if(transfer->hash != NULL)
		    free(hash);
		addtrdata(transfer);
	    }
	}
    }
}

void updatesbar(char *msg)
{
    gtk_statusbar_pop(GTK_STATUSBAR(main_statusbar), 0);
    gtk_statusbar_push(GTK_STATUSBAR(main_statusbar), 0, msg);
}

void freesrchsizes(void)
{
    int i;
    
    for(i = 0; i < numsizes; i++)
    {
	if(srchsizes[i].ref != NULL)
	    gtk_tree_row_reference_free(srchsizes[i].ref);
    }
    if(srchsizes != NULL)
	free(srchsizes);
    srchsizes = NULL;
    numsizes = 0;
}

void dcdisconnected(void)
{
    if(gdkread != -1)
    {
	gdk_input_remove(gdkread);
	gdkread = -1;
    }
    dcfd = -1;
    updatehublist();
    updatetransferlists();
    cursrch = nextsrch = -1;
    gtk_tree_store_clear(srchmodel);
    freesrchsizes();
    gtk_widget_set_sensitive(main_connmenu, TRUE);
    gtk_widget_set_sensitive(main_dconnmenu, FALSE);
    gtk_widget_set_sensitive(main_simplesrch, TRUE);
    gtk_widget_set_sensitive(main_realsrch, TRUE);
    gtk_widget_set_sensitive(main_srchbtn, TRUE);
    gtk_widget_set_sensitive(main_srchcanbtn, FALSE);
    updatesbar(_("Disconnected"));
}

char *inputbox(char *title, char *prompt, char *def, int echo)
{
    int resp;
    GtkWidget *swnd;
    char *buf;
    
    inpdialog = gtk_dialog_new_with_buttons(title, GTK_WINDOW(main_wnd), GTK_DIALOG_MODAL, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(inpdialog)->vbox), swnd = create_inpdialog_wnd(), TRUE, TRUE, 0);
    gtk_widget_show(swnd);
    if(!echo)
	gtk_entry_set_visibility(GTK_ENTRY(inpdialog_entry), FALSE);
    gtk_label_set_text(GTK_LABEL(inpdialog_prompt), prompt);
    gtk_entry_set_text(GTK_ENTRY(inpdialog_entry), def);
    resp = gtk_dialog_run(GTK_DIALOG(inpdialog));
    if(!echo)
	gtk_entry_set_visibility(GTK_ENTRY(inpdialog_entry), TRUE);
    if(resp == GTK_RESPONSE_ACCEPT)
	buf = strdup(gtk_entry_get_text(GTK_ENTRY(inpdialog_entry)));
    else
	buf = NULL;
    gtk_widget_destroy(inpdialog);
    updatewrite();
    return(buf);
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
    swnd = gtk_message_dialog_new(GTK_WINDOW(main_wnd), GTK_DIALOG_MODAL, type, buttons, "%s", buf);
    resp = gtk_dialog_run(GTK_DIALOG(swnd));
    gtk_widget_destroy(swnd);
    free(buf);
    return(resp);
}

void readconfigfile(void)
{
    FILE *cfgfile;
    char *homedir, *buf, *p;
    int w, h;
    
    if((homedir = getenv("HOME")) == NULL)
    {
	fprintf(stderr, "warning: could not find home directory!\n");
	return;
    }
    buf = sprintf2("%s/.dolconrc", homedir);
    if((cfgfile = fopen(buf, "r")) == NULL)
    {
	if(errno != ENOENT)
	    perror(buf);
	free(buf);
	return;
    }
    free(buf);
    buf = smalloc(1024);
    while(fgets(buf, 1024, cfgfile) != NULL)
    {
	if(strlen(buf) < 1)
	    continue;
	p = buf + strlen(buf);
	if(p[-1] == '\n')
	    *(--p) = 0;
	if((p = strchr(buf, ':')) == NULL)
	    continue;
	*(p++) = 0;
	while((*p == ' ') || (*p == '\t'))
	    p++;
	if(!strcmp(buf, "wnd-width"))
	{
	    w = atoi(p);
	} else if(!strcmp(buf, "wnd-height")) {
	    h = atoi(p);
	} else if(!strcmp(buf, "pane1-pos")) {
	    gtk_paned_set_position(GTK_PANED(main_pane1), atoi(p));
	} else if(!strcmp(buf, "pane2-pos")) {
	    gtk_paned_set_position(GTK_PANED(main_pane2), atoi(p));
	} else if(!strcmp(buf, "pane3-pos")) {
	    gtk_paned_set_position(GTK_PANED(main_pane3), atoi(p));
	} else if(!strcmp(buf, "pubhubaddr")) {
	    free(pubhubaddr);
	    pubhubaddr = sstrdup(p);
	} else if(!strcmp(buf, "dcserver")) {
	    free(dcserver);
	    dcserver = sstrdup(p);
	} else if(!strcmp(buf, "advexpanded")) {
	    gtk_expander_set_expanded(GTK_EXPANDER(main_advexp), atoi(p));
	} else if(!strcmp(buf, "connectas")) {
	    free(connectas);
	    connectas = sstrdup(p);
	} else if(!strcmp(buf, "autoconn")) {
	    autoconn = atoi(p);
	} else if(!strcmp(buf, "filternoslots")) {
	    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(main_filternoslots), atoi(p));
	}
    }
    free(buf);
    fclose(cfgfile);
/*
    if(w != 1589)
	abort();
*/
    gtk_window_resize(GTK_WINDOW(main_wnd), w, h);
}

void updateconfigfile(void)
{
    FILE *cfgfile;
    char *homedir, *buf;
    int w, h;
    
    if((homedir = getenv("HOME")) == NULL)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not get your home directory!"));
	return;
    }
    buf = sprintf2("%s/.dolconrc", homedir);
    if((cfgfile = fopen(buf, "w")) == NULL)
    {
	free(buf);
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not open configuration file for writing: %s"), strerror(errno));
	return;
    }
    free(buf);
    gtk_window_get_size(GTK_WINDOW(main_wnd), &w, &h);
    fprintf(cfgfile, "wnd-width: %i\n", w);
    fprintf(cfgfile, "wnd-height: %i\n", h);
    fprintf(cfgfile, "pane1-pos: %i\n", gtk_paned_get_position(GTK_PANED(main_pane1)));
    fprintf(cfgfile, "pane2-pos: %i\n", gtk_paned_get_position(GTK_PANED(main_pane2)));
    fprintf(cfgfile, "pane3-pos: %i\n", gtk_paned_get_position(GTK_PANED(main_pane3)));
    fprintf(cfgfile, "pubhubaddr: %s\n", pubhubaddr);
    fprintf(cfgfile, "dcserver: %s\n", dcserver);
    fprintf(cfgfile, "advexpanded: %i\n", gtk_expander_get_expanded(GTK_EXPANDER(main_advexp)));
    fprintf(cfgfile, "connectas: %s\n", connectas);
    fprintf(cfgfile, "autoconn: %i\n", autoconn);
    fprintf(cfgfile, "filternoslots: %i\n", gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(main_filternoslots)));
    fclose(cfgfile);
}

gboolean initdeath(GtkWidget *widget, gpointer data)
{
    updateconfigfile();
    gtk_main_quit();
    return(TRUE);
}

void cb_inpdialog_entry_activate(GtkWidget *widget, gpointer data)
{
    gtk_dialog_response(GTK_DIALOG(inpdialog), GTK_RESPONSE_ACCEPT);
}

int loginconv(int type, wchar_t *prompt, char **resp, void *data)
{
    int ret;
    char *buf;
    
    ret = 0;
    buf = icwcstombs(prompt, "UTF-8");
    switch(type)
    {
    case DC_LOGIN_CONV_NOECHO:
	if((*resp = inputbox(_("Login"), buf, "", 0)) == NULL)
	    ret = 1;
	break;
    case DC_LOGIN_CONV_ECHO:
	if((*resp = inputbox(_("Login"), buf, "", 1)) == NULL)
	    ret = 1;
	break;
    case DC_LOGIN_CONV_INFO:
	msgbox(GTK_MESSAGE_INFO, GTK_BUTTONS_OK, "%s", buf);
	break;
    case DC_LOGIN_CONV_ERROR:
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, "%s", buf);
	break;
    }
    free(buf);
    updatewrite();
    return(ret);
}

void getfnlistcallback(int resp, void *data)
{
    updatehublist();
}

void gettrlistcallback(int resp, void *data)
{
    updatetransferlists();
}

void logincallback(int err, wchar_t *reason, void *data)
{
    switch(err)
    {
    case DC_LOGIN_ERR_SUCCESS:
	dc_queuecmd(NULL, NULL, L"notify", L"all", L"on", NULL);
	dc_getfnlistasync(getfnlistcallback, NULL);
	dc_gettrlistasync(gettrlistcallback, NULL);
	updatesbar("Authenticated");
	break;
    case DC_LOGIN_ERR_NOLOGIN:
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not negotiate an acceptable authentication mechanism"));
	dc_disconnect();
	dcdisconnected();
	break;
    case DC_LOGIN_ERR_SERVER:
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The server has encountered an error"));
	dc_disconnect();
	dcdisconnected();
	break;
    case DC_LOGIN_ERR_USER:
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Internal client error"));
	dc_disconnect();
	dcdisconnected();
	break;
    case DC_LOGIN_ERR_CONV:
	dc_disconnect();
	dcdisconnected();
	break;
    case DC_LOGIN_ERR_AUTHFAIL:
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Login attempt failed!"));
	dc_disconnect();
	dcdisconnected();
	break;
    }
    updatewrite();
}

GtkTreeIter *ref2iter(GtkTreeRowReference *ref)
{
    static GtkTreeIter iter;
    GtkTreePath *path;
    
    assert((path = gtk_tree_row_reference_get_path(ref)) != NULL);
    assert(gtk_tree_model_get_iter(GTK_TREE_MODEL(srchmodel), &iter, path));
    gtk_tree_path_free(path);
    return(&iter);
}

GtkTreeRowReference *iter2ref(GtkTreeIter *iter)
{
    GtkTreePath *path;
    GtkTreeRowReference *ref;
    
    assert((path = gtk_tree_model_get_path(GTK_TREE_MODEL(srchmodel), iter)) != NULL);
    assert((ref = gtk_tree_row_reference_new(GTK_TREE_MODEL(srchmodel), path)) != NULL);
    gtk_tree_path_free(path);
    return(ref);
}

struct srchsize *finddiscsize(void)
{
    int i;
    GtkTreeIter iter;
    
    for(i = 0; i < numsizes; i++)
    {
	if(srchsizes[i].size == -1)
	    return(&srchsizes[i]);
    }
    srchsizes = srealloc(srchsizes, sizeof(*srchsizes) * ++numsizes);
    srchsizes[i].size = -1;
    srchsizes[i].num = 1;
    srchsizes[i].slots = 0;
    srchsizes[i].resptime = 0.0;
    gtk_tree_store_append(srchmodel, &iter, NULL);
    gtk_tree_store_set(srchmodel, &iter, 3, _("Discrete sizes"), 7, 1, -1);
    srchsizes[i].ref = iter2ref(&iter);
    return(&srchsizes[i]);
}

struct knownspeed *findksentbyname(char *userid)
{
    int i;
    
    for(i = 0; i < numspeeds; i++)
    {
	if(!strcmp(knownspeeds[i].userid, userid))
	    return(&knownspeeds[i]);
    }
    return(NULL);
}

struct knownspeed *findksentbyseq(int seq)
{
    int i;
    
    for(i = 0; i < numspeeds; i++)
    {
	if(knownspeeds[i].seq == seq)
	    return(&knownspeeds[i]);
    }
    return(NULL);
}

gboolean ksupdaterow(GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer data)
{
    struct knownspeed *ks;
    char *userid;
    
    gtk_tree_model_get(GTK_TREE_MODEL(model), iter, 1, &userid, -1);
    if(userid == NULL)
	return(FALSE);
    ks = findksentbyname(userid);
    if(ks == NULL)
    {
	knownspeeds = srealloc(knownspeeds, (numspeeds + 1) * sizeof(*knownspeeds));
	ks = &knownspeeds[numspeeds];
	numspeeds++;
	ks->userid = sstrdup(userid);
	ks->speed = -1;
	ks->seq = -2;
	ksqueryseq = -2;
    }
    g_free(userid);
    if(ks->speed != -1)
	gtk_tree_store_set(GTK_TREE_STORE(model), iter, 8, ks->speed, -1);
    return(FALSE);
}

gint ksupdatecb(gpointer data)
{
    int i, oldnum;
    time_t now;
    wchar_t **users, *buf;
    size_t userssize, usersdata;
    
    if(ksquerytag != -1)
	return(TRUE);
    now = time(NULL);
    oldnum = numspeeds;
    for(i = 0; i < numspeeds;)
    {
	if(now - knownspeeds[i].fetched > 60)
	{
	    free(knownspeeds[i].userid);
	    memmove(&knownspeeds[i], &knownspeeds[i + 1], (--numspeeds - i) * sizeof(*knownspeeds));
	} else {
	    i++;
	}
    }
    if(oldnum != numspeeds)
    {
	if(numspeeds == 0)
	{
	    free(knownspeeds);
	    knownspeeds = NULL;
	} else {
	    knownspeeds = srealloc(knownspeeds, numspeeds * sizeof(*knownspeeds));
	}
    }
    gtk_tree_model_foreach(GTK_TREE_MODEL(srchmodel), ksupdaterow, NULL);
    if(ksqueryseq == -2)
    {
	users = NULL;
	userssize = usersdata = 0;
	ksqueryseq = 0;
	for(i = 0; i < numspeeds; i++)
	{
	    if(knownspeeds[i].seq == -2)
	    {
		assert((buf = icmbstowcs(knownspeeds[i].userid, "UTF-8")) != NULL);
		knownspeeds[i].seq = ksqueryseq++;
		addtobuf(users, buf);
	    }
	}
	addtobuf(users, NULL);
	ksquerytag = dc_queuecmd(NULL, NULL, L"filtercmd", L"userspeeda", L"%a", users, NULL);
	dc_freewcsarr(users);
    }
    updatewrite();
    return(TRUE);
}

void handleresps(void)
{
    int i;
    struct dc_response *resp;
    struct dc_intresp *ires;
    struct dc_fnetnode *fn;
    struct fndata *fndata;
    GtkTextIter iter;
    GtkTreeIter titer, piter;
    char *buf, *p;
    int tosbuf;
    struct srchsize *ss;
    struct knownspeed *ks;
    
    while((resp = dc_getresp()) != NULL)
    {
	if(!wcscmp(resp->cmdname, L".connect"))
	{
	    if(resp->code != 201)
	    {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The server refused the connection"));
		dc_disconnect();
		dcdisconnected();
	    } else if(dc_checkprotocol(resp, DC_LATEST)) {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Server protocol revision mismatch"));
		dc_disconnect();
		dcdisconnected();
	    } else {
		tosbuf = 0x10; /* Minimum delay */
		setsockopt(dcfd, IPPROTO_IP, IP_TOS, &tosbuf, sizeof(tosbuf));
		updatesbar(_("Connected"));
		dc_loginasync(connectas, 1, loginconv, logincallback, NULL);
	    }
	} else if(!wcscmp(resp->cmdname, L".notify")) {
	    dc_uimisc_handlenotify(resp);
	    switch(resp->code)
	    {
	    case 600:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if((fn = dc_findfnetnode(ires->argv[0].val.num)) != NULL)
		    {
			fndata = fn->udata;
			gtk_text_buffer_get_end_iter(fndata->textbuf, &iter);
			if((buf = icwcstombs(ires->argv[3].val.str, "UTF-8")) != NULL)
			{
			    gtk_text_buffer_insert_with_tags_by_name(fndata->textbuf, &iter, "<", -1, "sender", NULL);
			    gtk_text_buffer_insert_with_tags_by_name(fndata->textbuf, &iter, buf, -1, "sender", NULL);
			    gtk_text_buffer_insert_with_tags_by_name(fndata->textbuf, &iter, ">", -1, "sender", NULL);
			    gtk_text_buffer_insert(fndata->textbuf, &iter, " ", -1);
			    free(buf);
			}
			if((buf = icwcstombs(ires->argv[4].val.str, "UTF-8")) != NULL)
			{
			    gtk_text_buffer_insert(fndata->textbuf, &iter, buf, -1);
			    gtk_text_buffer_insert(fndata->textbuf, &iter, "\n", -1);
			    free(buf);
			    if(curchat == fn->id)
				gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(main_chatview), &iter, 0, 0, 0, 0);
			}
		    }
		    dc_freeires(ires);
		}
		break;
	    case 601:
	    case 602:
	    case 603:
	    case 604:
	    case 605:
		updatehublist();
		break;
	    case 610:
	    case 611:
	    case 612:
	    case 613:
	    case 614:
	    case 615:
	    case 616:
	    case 617:
	    case 618:
		updatetransferlists();
		break;
	    case 620:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if(ires->argv[0].val.num == nextsrch)
			srcheta = time(NULL) + ires->argv[0].val.num;
		    dc_freeires(ires);
		}
		break;
	    case 621:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if(ires->argv[0].val.num == nextsrch)
		    {
			if(cursrch != -1)
			    dc_queuecmd(NULL, NULL, L"cansrch", L"%i", cursrch, NULL);
			cursrch = nextsrch;
			nextsrch = -1;
			gtk_widget_set_sensitive(main_realsrch, TRUE);
			gtk_widget_set_sensitive(main_simplesrch, TRUE);
			gtk_widget_set_sensitive(main_srchbtn, TRUE);
			gtk_widget_set_sensitive(main_srchcanbtn, FALSE);
			srchstatupdate();
			gtk_entry_set_text(GTK_ENTRY(main_realsrch), "");
			gtk_entry_set_text(GTK_ENTRY(main_simplesrch), "");
			gtk_tree_store_clear(srchmodel);
			freesrchsizes();
		    }
		    dc_freeires(ires);
		}
		break;
	    case 622:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if(ires->argv[0].val.num == cursrch)
		    {
			for(i = 0; i < numsizes; i++)
			{
			    if(srchsizes[i].size == ires->argv[4].val.lnum)
				break;
			}
			if(i == numsizes)
			{
			    srchsizes = srealloc(srchsizes, sizeof(*srchsizes) * ++numsizes);
			    srchsizes[i].size = (gint64)ires->argv[4].val.lnum;
			    srchsizes[i].num = 1;
			    srchsizes[i].slots = ires->argv[5].val.num;
			    srchsizes[i].resptime = ires->argv[7].val.flnum;
			    ss = finddiscsize();
			    ss->slots += ires->argv[5].val.num;
			    if((ss->resptime == 0.0) || (ss->resptime > ires->argv[7].val.flnum))
				ss->resptime = ires->argv[7].val.flnum;
			    piter = *ref2iter(ss->ref);
			    gtk_tree_store_set(srchmodel, &piter, 5, ss->slots, 6, ss->resptime, -1);
			    gtk_tree_store_append(srchmodel, &titer, &piter);
			    srchsizes[i].ref = iter2ref(&titer);
			} else if(srchsizes[i].num == 1) {
			    char *filename, *peername, *fnetname, *hash;
			    int slots, speed;
			    double resptime;
			    
			    gtk_tree_model_get(GTK_TREE_MODEL(srchmodel), ref2iter(srchsizes[i].ref), 0, &fnetname, 1, &peername, 3, &filename, 5, &slots, 6, &resptime, 8, &speed, 9, &hash, -1);
			    gtk_tree_store_remove(srchmodel, ref2iter(srchsizes[i].ref));
			    gtk_tree_row_reference_free(srchsizes[i].ref);
			    ss = finddiscsize();
			    ss->slots -= slots;
			    gtk_tree_store_set(srchmodel, ref2iter(ss->ref), 5, ss->slots, -1);
			    gtk_tree_store_append(srchmodel, &piter, NULL);
			    srchsizes[i].slots = ires->argv[5].val.num + slots;
			    srchsizes[i].resptime = (ires->argv[7].val.flnum < resptime)?ires->argv[7].val.flnum:resptime;
			    srchsizes[i].num = 2;
			    srchsizes[i].ref = iter2ref(&piter);
			    gtk_tree_store_set(srchmodel, &piter, 4, srchsizes[i].size, 5, srchsizes[i].slots, 6, srchsizes[i].resptime, 7, 2, -1);
			    if((buf = icwcstombs(ires->argv[1].val.str, "UTF-8")) != NULL)
			    {
				p = buf;
				/* XXX: Too NMDC-specific! */
				if(strrchr(p, '\\') != NULL)
				    p = strrchr(p, '\\') + 1;
				gtk_tree_store_set(srchmodel, &piter, 3, p, -1);
				free(buf);
			    }
			    gtk_tree_store_append(srchmodel, &titer, &piter);
			    gtk_tree_store_set(srchmodel, &titer, 0, fnetname, 1, peername, 2, peername, 3, filename, 4, srchsizes[i].size, 5, slots, 6, resptime, 8, speed, 9, hash, -1);
			    g_free(filename); g_free(peername); g_free(fnetname); g_free(hash);
			    gtk_tree_store_append(srchmodel, &titer, &piter);
			} else {
			    srchsizes[i].num++;
			    srchsizes[i].slots += ires->argv[5].val.num;
			    if(ires->argv[7].val.flnum < srchsizes[i].resptime)
				srchsizes[i].resptime = ires->argv[7].val.flnum;
			    piter = *ref2iter(srchsizes[i].ref);
			    gtk_tree_store_set(srchmodel, &piter, 5, srchsizes[i].slots, 6, srchsizes[i].resptime, 7, srchsizes[i].num, -1);
			    gtk_tree_store_append(srchmodel, &titer, &piter);
			}
			if((buf = icwcstombs(ires->argv[1].val.str, "UTF-8")) != NULL)
			{
			    gtk_tree_store_set(srchmodel, &titer, 3, buf, -1);
			    free(buf);
			}
			if((buf = icwcstombs(ires->argv[2].val.str, "UTF-8")) != NULL)
			{
			    gtk_tree_store_set(srchmodel, &titer, 0, buf, -1);
			    free(buf);
			}
			if((buf = icwcstombs(ires->argv[3].val.str, "UTF-8")) != NULL)
			{
			    gtk_tree_store_set(srchmodel, &titer, 1, buf, -1);
			    gtk_tree_store_set(srchmodel, &titer, 2, buf, -1);
			    free(buf);
			}
			if((buf = icwcstombs(ires->argv[8].val.str, "UTF-8")) != NULL)
			{
			    gtk_tree_store_set(srchmodel, &titer, 9, buf, -1);
			    free(buf);
			}
			gtk_tree_store_set(srchmodel, &titer, 4, (gint64)ires->argv[4].val.lnum, 5, ires->argv[5].val.num, 6, ires->argv[7].val.flnum, 8, -1, -1);
		    }
		    dc_freeires(ires);
		}
		break;
	    default:
		break;
	    }
	} else if(!wcscmp(resp->cmdname, L"filtercmd")) {
	    if((ksquerytag >= 0) && (ksquerytag == resp->tag))
	    {
		for(i = 0; i < resp->numlines; i++)
		{
		    assert((ks = findksentbyseq(i)) != NULL);
		    ks->speed = wcstol(resp->rlines[i].argv[1], NULL, 10);
		    ks->seq = -1;
		    ks->fetched = time(NULL);
		}
		ksquerytag = -1;
		ksupdatecb(NULL);
	    } else if((lsrestag >= 0) && (lsrestag == resp->tag)) {
		for(i = 0; i < resp->numlines; i++)
		{
		    if(!wcsncmp(resp->rlines[i].argv[1], L"id:", 3))
		    {
			gtk_list_store_append(reslist, &titer);
			gtk_list_store_set(reslist, &titer, 0, icswcstombs(resp->rlines[i].argv[1] + 3, "UTF-8", NULL), -1);
		    } else if(!wcsncmp(resp->rlines[i].argv[1], L"size:", 5)) {
			gtk_list_store_set(reslist, &titer, 1, (gint64)wcstoll(resp->rlines[i].argv[1] + 5, NULL, 10), -1);
		    } else if(!wcsncmp(resp->rlines[i].argv[1], L"prog:", 5)) {
			gtk_list_store_set(reslist, &titer, 2, (gint64)wcstoll(resp->rlines[i].argv[1] + 5, NULL, 10), -1);
		    } else if(!wcsncmp(resp->rlines[i].argv[1], L"name:", 5)) {
			gtk_list_store_set(reslist, &titer, 3, icswcstombs(resp->rlines[i].argv[1] + 5, "UTF-8", NULL), -1);
		    } else if(!wcsncmp(resp->rlines[i].argv[1], L"lock:", 5)) {
			if(!wcscmp(resp->rlines[i].argv[1] + 5, L"yes"))
			    gtk_list_store_set(reslist, &titer, 4, TRUE, -1);
			else
			    gtk_list_store_set(reslist, &titer, 4, FALSE, -1);
		    } else if(!wcsncmp(resp->rlines[i].argv[1], L"hash:", 5)) {
			gtk_list_store_set(reslist, &titer, 5, icswcstombs(resp->rlines[i].argv[1] + 5, "UTF-8", NULL), -1);
		    }
		}
		lsrestag = -1;
		gtk_widget_set_sensitive(reslist_reload, TRUE);
	    }
	}
	dc_freeresp(resp);
    }
    updatewrite();
}

void dcfdcallback(gpointer data, gint source, GdkInputCondition condition)
{
    int errnobak;
    
    if(((condition & GDK_INPUT_READ) && dc_handleread()) || ((condition & GDK_INPUT_WRITE) && dc_handlewrite()))
    {
	errnobak = errno;
	dcdisconnected();
	if(errnobak == 0)
	{
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The server has closed the connection"));
	} else {
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The connection to the server failed:\n\n%s"), strerror(errnobak));
	}
	return;
    }
    handleresps();
}

void cb_main_dconnmenu_activate(GtkWidget *widget, gpointer data)
{
    if(dcfd < 0)
	return;
    dc_disconnect();
    dcdisconnected();
}

void cb_main_prefmenu_activate(GtkWidget *widget, gpointer data)
{
    GtkWidget *dialog, *swnd;
    int resp;
    
    dialog = gtk_dialog_new_with_buttons(_("Preferences"), GTK_WINDOW(main_wnd), GTK_DIALOG_MODAL, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
    gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), swnd = create_pref_wnd(), TRUE, TRUE, 0);
    gtk_entry_set_text(GTK_ENTRY(pref_pubhuburl), pubhubaddr);
    gtk_entry_set_text(GTK_ENTRY(pref_connectas), connectas);
    gtk_entry_set_text(GTK_ENTRY(pref_dcserver), dcserver);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pref_autoconn), autoconn);
    gtk_widget_show(swnd);
    resp = gtk_dialog_run(GTK_DIALOG(dialog));
    if(resp == GTK_RESPONSE_ACCEPT)
    {
	free(pubhubaddr);
	pubhubaddr = sstrdup(gtk_entry_get_text(GTK_ENTRY(pref_pubhuburl)));
	free(connectas);
	connectas = sstrdup(gtk_entry_get_text(GTK_ENTRY(pref_connectas)));
	free(dcserver);
	dcserver = sstrdup(gtk_entry_get_text(GTK_ENTRY(pref_dcserver)));
	autoconn = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pref_autoconn));
    }
    gtk_widget_destroy(dialog);
}

void cb_main_lsres_activate(GtkWidget *widget, gpointer data)
{
    gtk_list_store_clear(reslist);
    gtk_widget_set_sensitive(reslist_delete, FALSE);
    gtk_widget_set_sensitive(reslist_search, FALSE);
    gtk_widget_show(reslist_wnd);
    if(lsrestag == -1)
    {
	lsrestag = dc_queuecmd(NULL, NULL, L"filtercmd", L"lsres", NULL);
	gtk_widget_set_sensitive(reslist_reload, FALSE);
    }
    updatewrite();
}

void dcconnect(char *host)
{
    dcfd = dc_connect(host);
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect:\n\n%s"), strerror(errno));
	return;
    }
    gdkread = gdk_input_add(dcfd, GDK_INPUT_READ, dcfdcallback, NULL);
    updatewrite();
    gtk_widget_set_sensitive(main_connmenu, FALSE);
    gtk_widget_set_sensitive(main_dconnmenu, TRUE);
    updatesbar(_("Connecting..."));
}

void cb_main_connmenu_activate(GtkWidget *widget, gpointer data)
{
    char *buf;
    
    if(dcfd >= 0)
	return;
    if((buf = inputbox(_("Connect"), _("Server address:"), dcserver, 1)) == NULL)
	return;
    dcconnect(buf);
    free(buf);
}

void cb_main_sdmenu_activate(GtkWidget *widget, gpointer data)
{
    int tag;
    struct dc_response *resp;

    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    tag = dc_queuecmd(NULL, NULL, L"shutdown", NULL);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	dc_freeresp(resp);
    }
    handleresps();
}

void cb_main_fnaddr_activate(GtkWidget *widget, gpointer data)
{
    int tag;
    struct dc_response *resp;
    wchar_t **toks;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    toks = dc_lexsexpr(icsmbstowcs((char *)gtk_entry_get_text(GTK_ENTRY(main_fnaddr)), "UTF-8", NULL));
    if(*toks == NULL)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Illegal address entered"));
	return;
    }
    if(wcschr(toks[0], L':') == NULL)
    {
	toks[0] = srealloc(toks[0], (wcslen(toks[0]) + 5) * sizeof(wchar_t));
	wcscat(toks[0], L":411");
    }
    tag = dc_queuecmd(NULL, NULL, L"cnct", L"dc", L"%a", toks, NULL);
    dc_freewcsarr(toks);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	if(resp->code == 509)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The server could not parse that address"));
	if(resp->code == 515)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("There are too many hubs connected"));
	dc_freeresp(resp);
    }
    gtk_entry_set_text(GTK_ENTRY(main_fnaddr), "");
    handleresps();
}

void setpubhubmodel(GtkTreeModel *model, int sortcol, int numcols, int *cols, char **names)
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *rnd;
    GtkTreeModel *sortmodel;
    int i;
    
    while((col = gtk_tree_view_get_column(GTK_TREE_VIEW(main_phublist), 0)) != NULL)
	gtk_tree_view_remove_column(GTK_TREE_VIEW(main_phublist), col);
    for(i = 0; i < numcols; i++) {
	if(gtk_tree_model_get_column_type(model, cols[i]) == G_TYPE_INT64)
	{
	    col = gtk_tree_view_column_new();
	    gtk_tree_view_column_set_title(col, names[i]);
	    rnd = gtk_cell_renderer_text_new();
	    gtk_tree_view_column_pack_start(col, rnd, TRUE);
	    gtk_tree_view_column_set_cell_data_func(col, rnd, transnicebytefunc2, GINT_TO_POINTER(cols[i]), NULL);
	} else {
	    col = gtk_tree_view_column_new_with_attributes(names[i], gtk_cell_renderer_text_new(), "text", cols[i], NULL);
	}
	gtk_tree_view_column_set_sort_column_id(col, cols[i]);
	gtk_tree_view_column_set_resizable(col, TRUE);
	gtk_tree_view_append_column(GTK_TREE_VIEW(main_phublist), col);
    }
    sortmodel = gtk_tree_model_sort_new_with_model(model);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(sortmodel), sortcol, GTK_SORT_DESCENDING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_phublist), sortmodel);
    g_object_unref(sortmodel);
}

void cb_main_pubhubfilter_activate(GtkWidget *widget, gpointer data)
{
    int err;
    const char *buf;
    char errbuf[1024];
    regex_t *filter;
    
    buf = gtk_entry_get_text(GTK_ENTRY(main_pubhubfilter));
    if(*buf)
    {
	filter = smalloc(sizeof(*filter));
	if((err = regcomp(filter, buf, REG_EXTENDED | REG_ICASE | REG_NOSUB)) != 0)
	{
	    regerror(err, filter, errbuf, sizeof(errbuf));
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, "Could not compile regex: %s", errbuf);
	    regfree(filter);
	    free(filter);
	    return;
	}
    } else {
	filter = NULL;
    }
    fetchhublist(pubhubaddr, filter);
}

void cb_main_pubhubabort_clicked(GtkWidget *widget, gpointer data)
{
    aborthublist();
}

void cb_main_dcnctbtn_clicked(GtkWidget *widget, gpointer data)
{
    GtkTreeIter iter;
    int tag, id;
    struct dc_response *resp;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(main_fnetnodes)), NULL, &iter))
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("No hub selected"));
	return;
    }
    gtk_tree_model_get(GTK_TREE_MODEL(fnmodel), &iter, 0, &id, -1);
    tag = dc_queuecmd(NULL, NULL, L"dcnct", L"%i", id, NULL);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	dc_freeresp(resp);
    }
    handleresps();
}

void cb_main_phublist_cchange(GtkWidget *widget, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    char *addr;
    
    if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(main_phublist)), &model, &iter))
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0, &addr, -1);
    gtk_entry_set_text(GTK_ENTRY(main_fnaddr), addr);
    g_free(addr);
}

void cb_main_phublist_activate(GtkWidget *widget, GtkTreePath *path, GtkTreeViewColumn *col, gpointer data)
{
    int tag;
    struct dc_response *resp;
    GtkTreeIter iter;
    GtkTreeModel *model;
    char *buf;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
    if(!gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path))
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0, &buf, -1);
    if(strchr(buf, ':') == NULL)
    {
	buf = g_realloc(buf, strlen(buf) + 5);
	strcat(buf, ":411");
    }
    tag = dc_queuecmd(NULL, NULL, L"cnct", L"dc", L"%s", buf, NULL);
    g_free(buf);
    gtk_entry_set_text(GTK_ENTRY(main_fnaddr), "");
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	if(resp->code == 509)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The server could not parse that address"));
	if(resp->code == 515)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("There are too many hubs connected"));
	dc_freeresp(resp);
    }
    handleresps();
}

void cb_main_chatnodes_activate(GtkWidget *widget, GtkTreePath *path, GtkTreeViewColumn *col, gpointer uudata)
{
    GtkTreeIter iter;
    int id;
    struct dc_fnetnode *fn;
    struct fndata *data;
    
    if(!gtk_tree_model_get_iter(GTK_TREE_MODEL(fnmodel), &iter, path))
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(fnmodel), &iter, 0, &id, -1);
    if((fn = dc_findfnetnode(id)) == NULL)
	return;
    data = fn->udata;
    curchat = id;
    if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(fnmodel), &iter))
    {
	do
	{
	    gtk_tree_model_get(GTK_TREE_MODEL(fnmodel), &iter, 0, &id, -1);
	    if(id == curchat)
		gtk_list_store_set(fnmodel, &iter, 5, "gtk-apply", -1);
	    else
		gtk_list_store_set(fnmodel, &iter, 5, NULL, -1);
	} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(fnmodel), &iter));
    }
    gtk_text_view_set_buffer(GTK_TEXT_VIEW(main_chatview), GTK_TEXT_BUFFER(data->textbuf));
}

void cb_main_chatstr_activate(GtkWidget *widget, gpointer data)
{
    int tag;
    const char *buf;
    struct dc_response *resp;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    if(curchat < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("No hub selected"));
	return;
    }
    buf = gtk_entry_get_text(GTK_ENTRY(main_chatstr));
    tag = dc_queuecmd(NULL, NULL, L"sendchat", L"%i", curchat, L"1", L"", L"%s", buf, NULL);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	else if(resp->code == 504)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("This hub could not support all the types of characters in your chat message"));
	else if(resp->code == 513)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("This hub does not support chatting"));
	else if(resp->code != 200)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to chat (%i)"), resp->code);
	dc_freeresp(resp);
    }
    gtk_entry_set_text(GTK_ENTRY(main_chatstr), "");
    handleresps();
}

void updatesrchfld(const char *simple)
{
    char *buf, *s;
    char *p, *p2;
    size_t bufsize, bufdata;
    
    s = sstrdup(simple);
    buf = NULL;
    bufsize = bufdata = 0;
    p = s;
    do
    {
	p2 = strchr(p, ' ');
	if(p2 != NULL)
	    *(p2++) = 0;
	if(*p)
	{
	    if(bufdata > 0)
		bufcat(buf, " & ", 3);
	    bufcat(buf, "N~", 2);
	    for(; *p; p++)
	    {
		if(strchr("[]()$^.*?+\\|\"!", *p) != NULL)
		    addtobuf(buf, '\\');
		addtobuf(buf, *p);
	    }
	}
	p = p2;
    } while(p2 != NULL);
    addtobuf(buf, 0);
    gtk_entry_set_text(GTK_ENTRY(main_realsrch), buf);
    free(buf);
    free(s);
}

void cb_main_simplesrch_changed(GtkWidget *widget, gpointer data)
{
    if(srchautoupdate)
	return;
    srchautoupdate = 1;
    updatesrchfld(gtk_entry_get_text(GTK_ENTRY(main_simplesrch)));
    srchautoupdate = 0;
}

void cb_main_realsrch_changed(GtkWidget *widget, gpointer data)
{
    if(srchautoupdate)
	return;
    srchautoupdate = 1;
    gtk_entry_set_text(GTK_ENTRY(main_simplesrch), "");
    srchautoupdate = 0;
}

void cb_main_srchbtn_clicked(GtkWidget *widget, gpointer data)
{
    wchar_t **toks;
    int tag;
    struct dc_response *resp;
    struct dc_intresp *ires;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    if(nextsrch != -1) /* Impossible case, but oh well... */
	return;
    toks = dc_lexsexpr(icsmbstowcs((char *)gtk_entry_get_text(GTK_ENTRY(main_realsrch)), "UTF-8", NULL));
    if(*toks == NULL)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Please enter a search expression before searching"));
	return;
    }
    tag = dc_queuecmd(NULL, NULL, L"search", L"all", L"%a", toks, NULL);
    dc_freewcsarr(toks);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 501)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("Could not find any hubs to search on"));
	else if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	else if(resp->code == 509)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The server could not parse your search expression"));
	else if(resp->code != 200)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to search (%i)"), resp->code);
	if(resp->code == 200)
	{
	    if((ires = dc_interpret(resp)) != NULL)
	    {
		nextsrch = ires->argv[0].val.num;
		srcheta = time(NULL) + ires->argv[1].val.num;
		dc_freeires(ires);
	    }
	    gtk_widget_set_sensitive(main_realsrch, FALSE);
	    gtk_widget_set_sensitive(main_simplesrch, FALSE);
	    gtk_widget_set_sensitive(main_srchbtn, FALSE);
	    gtk_widget_set_sensitive(main_srchcanbtn, TRUE);
	    srchstatupdate();
	}
	dc_freeresp(resp);
    }
    handleresps();
}

void cb_main_srchcanbtn_clicked(GtkWidget *widget, gpointer data)
{
    if(nextsrch == -1)
	return;
    dc_queuecmd(NULL, NULL, L"cansrch", L"%i", nextsrch, NULL);
    nextsrch = -1;
    gtk_widget_set_sensitive(main_realsrch, TRUE);
    gtk_widget_set_sensitive(main_simplesrch, TRUE);
    gtk_widget_set_sensitive(main_srchbtn, TRUE);
    gtk_widget_set_sensitive(main_srchcanbtn, FALSE);
    srchstatupdate();
}

gboolean cb_main_trlist_keypress(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    int id, tag;
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    struct dc_response *resp;
    
    if((event->type == GDK_KEY_PRESS) && (event->keyval == GDK_Delete))
    {
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
	if(gtk_tree_selection_get_selected(sel, &model, &iter))
	{
	    gtk_tree_model_get(model, &iter, 0, &id, -1);
	    tag = dc_queuecmd(NULL, NULL, L"cancel", L"%i", id, NULL);
	    if((resp = dc_gettaggedrespsync(tag)) != NULL)
	    {
		if(resp->code == 502)
		    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
		else if(resp->code != 200)
		    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to cancel (%i)"), resp->code);
		dc_freeresp(resp);
	    }
	    handleresps();
	}
	return(TRUE);
    }
    return(FALSE);
}

void cb_main_srchres_activate(GtkWidget *widget, GtkTreePath *path, GtkTreeViewColumn *col, gpointer data)
{
    int tag;
    struct dc_response *resp;
    GtkTreeIter iter;
    GtkTreeModel *model;
    int num;
    gint64 size;
    char *tfnet, *tpeerid, *tfilename, *thash, *arg;
    wchar_t *fnet, *peerid, *filename, *hash;
    
    if(dcfd < 0)
    {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Not connected to DC server"));
	return;
    }
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
    if(!gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path))
	return;
    gtk_tree_model_get(model, &iter, 7, &num, -1);
    if(num > 0)
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0, &tfnet, 1, &tpeerid, 3, &tfilename, 4, &size, 9, &thash, -1);
    fnet = icmbstowcs(tfnet, "UTF-8");
    peerid = icmbstowcs(tpeerid, "UTF-8");
    filename = icmbstowcs(tfilename, "UTF-8");
    hash = (thash == NULL)?NULL:icmbstowcs(thash, "UTF-8");
    if((fnet == NULL) || (peerid == NULL) || (filename == NULL))
    {
	if(fnet != NULL)
	    free(fnet);
	if(peerid != NULL)
	    free(peerid);
	if(filename != NULL)
	    free(filename);
	if(hash != NULL)
	    free(hash);
	g_free(tfnet);
	g_free(tpeerid);
	g_free(tfilename);
	if(thash != NULL)
	    g_free(thash);
	return;
    }
    g_free(tfnet);
    g_free(tpeerid);
    g_free(tfilename);
    arg = (char *)gtk_entry_get_text(GTK_ENTRY(main_dlarg));
    if(*arg)
	tag = dc_queuecmd(NULL, NULL, L"download", fnet, L"%ls", peerid, L"%ls", filename, L"%li", (dc_lnum_t)size, L"hash", L"%ls", (hash == NULL)?L"":hash, L"user", L"%s", arg, NULL);
    else
	tag = dc_queuecmd(NULL, NULL, L"download", fnet, L"%ls", peerid, L"%ls", filename, L"%li", (dc_lnum_t)size, L"hash", L"%ls", (hash == NULL)?L"":hash, NULL);
    free(fnet);
    free(peerid);
    free(filename);
    if(hash != NULL)
	free(hash);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->code == 502)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	if(resp->code != 200)
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to queue the download (%i)"), resp->code);
	dc_freeresp(resp);
    }
    handleresps();
}

gboolean srchfilterfunc(GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
    int slots;
    int filteratall;
    
    filteratall = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(main_filternoslots));
    if(!filteratall)
	return(TRUE);
    gtk_tree_model_get(model, iter, 5, &slots, -1);
    if(slots < 1)
	return(FALSE);
    return(TRUE);
}

void cb_main_filternoslots_toggled(GtkToggleButton *widget, gpointer data)
{
    gtk_tree_model_filter_refilter(srchmodelfilter);
}

void cb_main_srhash_activate(GtkWidget *widget, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash, *buf;
    
    if(nextsrch != -1)
	return;
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_srchres));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 9, &hash, -1);
	buf = sprintf2("H=%s", hash);
	gtk_entry_set_text(GTK_ENTRY(main_realsrch), buf);
	g_free(hash);
	free(buf);
	cb_main_srchbtn_clicked(widget, NULL);
    } else {
	return;
    }
}

void cb_main_srcopy_activate(GtkWidget *widget, gpointer data)
{
    GtkClipboard *cb;
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash;
    
    if(nextsrch != -1)
	return;
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_srchres));
    if(!gtk_tree_selection_get_selected(sel, &model, &iter))
	return;
    gtk_tree_model_get(model, &iter, 9, &hash, -1);
    cb = gtk_clipboard_get(gdk_atom_intern("PRIMARY", FALSE));
    gtk_clipboard_set_text(cb, hash, -1);
    g_free(hash);
}

void cb_main_trhash_activate(GtkWidget *widget, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash, *buf;
    
    if(nextsrch != -1)
	return;
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_downloads));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 12, &hash, -1);
	buf = sprintf2("H=%s", hash);
	gtk_entry_set_text(GTK_ENTRY(main_realsrch), buf);
	g_free(hash);
	free(buf);
	cb_main_srchbtn_clicked(widget, NULL);
    } else {
	return;
    }
}

void cb_main_trcopy_activate(GtkWidget *widget, gpointer data)
{
    GtkClipboard *cb;
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash;
    
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_downloads));
    if(!gtk_tree_selection_get_selected(sel, &model, &iter))
	return;
    gtk_tree_model_get(model, &iter, 12, &hash, -1);
    cb = gtk_clipboard_get(gdk_atom_intern("PRIMARY", FALSE));
    gtk_clipboard_set_text(cb, hash, -1);
    g_free(hash);
}

void cb_main_trreset_activate(GtkWidget *widget, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    int id, tag;
    struct dc_response *resp;
    
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_downloads));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 0, &id, -1);
	tag = dc_queuecmd(NULL, NULL, L"reset", L"%i", id, NULL);
	if((resp = dc_gettaggedrespsync(tag)) != NULL)
	{
	    if(resp->code == 502)
		msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	    else if(resp->code != 200)
		msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to reset (%i)"), resp->code);
	    dc_freeresp(resp);
	}
	handleresps();
    } else {
	return;
    }
}

void cb_main_trcancel_activate(GtkWidget *widget, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    int id, tag;
    struct dc_response *resp;
    
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(main_downloads));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 0, &id, -1);
	tag = dc_queuecmd(NULL, NULL, L"cancel", L"%i", id, NULL);
	if((resp = dc_gettaggedrespsync(tag)) != NULL)
	{
	    if(resp->code == 502)
		msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("You do not have permission to do that"));
	    else if(resp->code != 200)
		msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred while trying to cancel (%i)"), resp->code);
	    dc_freeresp(resp);
	}
	handleresps();
    } else {
	return;
    }
}

/* XXX: This is quite a hack, since the calling convention is
 * different for the popup-menu sig and the button-press-event sig. It
 * most certainly works, but I don't know how portable it is. */
gboolean cb_main_srpopup(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash;
    
    if((event != NULL) && (event->button != 3))
	return(FALSE);
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 9, &hash, -1);
	if((hash == NULL) || (*hash == 0))
	{
	    gtk_widget_set_sensitive(main_srhash, FALSE);
	    gtk_widget_set_sensitive(main_srcopy, FALSE);
	} else {
	    if(nextsrch == -1)
		gtk_widget_set_sensitive(main_srhash, TRUE);
	    else
		gtk_widget_set_sensitive(main_srhash, FALSE);
	    gtk_widget_set_sensitive(main_srcopy, TRUE);
	}
	g_free(hash);
    } else {
	return(FALSE);
    }
    if(event == NULL)
	gtk_menu_popup(GTK_MENU(main_srpopup), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time());
    else
	gtk_menu_popup(GTK_MENU(main_srpopup), NULL, NULL, NULL, NULL, event->button, event->time);
    return(FALSE);
}

/* The above hack note goes for this one too. */
gboolean cb_main_trpopup(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *hash;
    
    if((event != NULL) && (event->button != 3))
	return(FALSE);
    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
    if(gtk_tree_selection_get_selected(sel, &model, &iter))
    {
	gtk_tree_model_get(model, &iter, 12, &hash, -1);
	if((hash == NULL) || (*hash == 0))
	{
	    gtk_widget_set_sensitive(main_trhash, FALSE);
	    gtk_widget_set_sensitive(main_trcopy, FALSE);
	} else {
	    if(nextsrch == -1)
		gtk_widget_set_sensitive(main_trhash, TRUE);
	    else
		gtk_widget_set_sensitive(main_trhash, FALSE);
	    gtk_widget_set_sensitive(main_trcopy, TRUE);
	}
	g_free(hash);
    } else {
	return(FALSE);
    }
    if(event == NULL)
	gtk_menu_popup(GTK_MENU(main_trpopup), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time());
    else
	gtk_menu_popup(GTK_MENU(main_trpopup), NULL, NULL, NULL, NULL, event->button, event->time);
    return(FALSE);
}

void cb_reslist_reload_clicked(GtkWidget *widget, gpointer data)
{
    if(lsrestag != -1)
	return;
    gtk_widget_set_sensitive(reslist_delete, FALSE);
    gtk_widget_set_sensitive(reslist_search, FALSE);
    gtk_list_store_clear(reslist);
    lsrestag = dc_queuecmd(NULL, NULL, L"filtercmd", L"lsres", NULL);
    gtk_widget_set_sensitive(reslist_reload, FALSE);
    updatewrite();
}

int rmres(char *id)
{
    int tag, ret;
    struct dc_response *resp;
    
    ret = -1;
    tag = dc_queuecmd(NULL, NULL, L"filtercmd", L"rmres", L"%s", id, NULL);
    if((resp = dc_gettaggedrespsync(tag)) != NULL)
    {
	if(resp->numlines > 0)
	{
	    if(!wcscmp(resp->rlines[0].argv[1], L"ok"))
		ret = 0;
	    else if(!wcsncmp(resp->rlines[0].argv[1], L"err:", 4))
		msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("An error occurred (%ls)"), resp->rlines[0].argv[1] + 4);
	}
	dc_freeresp(resp);
    }
    handleresps();
    return(ret);
}

void cb_reslist_delete_clicked(GtkWidget *widget, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    char *id;
    gboolean locked;
    
    if(nextsrch != -1)
	return;
    if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(reslist_list)), &model, &iter))
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0, &id, 4, &locked, -1);
    if(locked)
    {
	g_free(id);
	return;
    }
    if(!rmres(id))
	gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
    g_free(id);
}

void cb_reslist_search_clicked(GtkWidget *widget, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    char *hash, *buf;
    
    if(nextsrch != -1)
	return;
    if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(reslist_list)), &model, &iter))
	return;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 5, &hash, -1);
    buf = sprintf2("H=%s", hash);
    gtk_entry_set_text(GTK_ENTRY(main_realsrch), buf);
    free(buf);
    g_free(hash);
    cb_main_srchbtn_clicked(widget, NULL);
}

void cb_reslist_list_cchange(GtkWidget *widget, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    gboolean locked;
    char *hash;
    
    if(!gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(reslist_list)), &model, &iter))
    {
	gtk_widget_set_sensitive(reslist_delete, FALSE);
	gtk_widget_set_sensitive(reslist_search, FALSE);
	return;
    }
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 4, &locked, 5, &hash, -1);
    gtk_widget_set_sensitive(reslist_delete, !locked);
    gtk_widget_set_sensitive(reslist_search, hash && *hash);
    g_free(hash);
}

void cb_reslist_list_activate(GtkWidget *widget, GtkTreePath *path, GtkTreeViewColumn *col, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    char *hash, *buf;

    model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
    if(!gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path))
	return;
    if(nextsrch != -1)
	return;
    gtk_tree_model_get(model, &iter, 5, &hash, -1);
    buf = sprintf2("H=%s", hash);
    gtk_entry_set_text(GTK_ENTRY(main_realsrch), buf);
    free(buf);
    g_free(hash);
    cb_main_srchbtn_clicked(widget, NULL);
}

gboolean cb_reslist_list_keypress(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    char *id;
    gboolean locked;
    
    if((event->type == GDK_KEY_PRESS) && (event->keyval == GDK_Delete))
    {
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
	if(gtk_tree_selection_get_selected(sel, &model, &iter))
	{
	    gtk_tree_model_get(model, &iter, 0, &id, 4, &locked, -1);
	    if(!locked)
	    {
		if(!rmres(id))
		    gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
	    }
	    g_free(id);
	}
	return(TRUE);
    }
    return(FALSE);
}

void srchstatupdate(void)
{
    char buf[1024];
    
    if(nextsrch == -1)
    {
	snprintf(buf, 1024, _("Ready to search"));
    } else {
	snprintf(buf, 1024, _("Search scheduled and will be submitted in %i seconds"), (int)(srcheta - time(NULL)));
    }
    if(strcmp(gtk_label_get_text(GTK_LABEL(main_srchstatus)), buf))
	gtk_label_set_text(GTK_LABEL(main_srchstatus), buf);
}

gint srchstatupdatecb(gpointer data)
{
    srchstatupdate();
    return(TRUE);
}

void initchattags(void)
{
    GtkTextTag *tag;
    
    chattags = gtk_text_tag_table_new();
    tag = gtk_text_tag_new("sender");
    g_object_set(tag, "foreground", "blue", NULL);
    gtk_text_tag_table_add(chattags, tag);
}

#include "../dolda-icon.xpm"

int main(int argc, char **argv)
{
    int c, connlocal;
    GtkWidget *wnd;
    PangoFontDescription *monospacefont;
    GtkTreeModel *sortmodel;
    struct passwd *pwent;
    
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    gtk_init(&argc, &argv);
    connlocal = 0;
    while((c = getopt(argc, argv, "lhV")) != -1) {
	switch(c) {
	case 'l':
	    connlocal = 1;
	    break;
	case 'h':
	    printf("usage: dolcon [-hlV]\n");
	    printf("\t-h\tDisplay this help message\n");
	    printf("\t-l\tConnect to the locally running daemon\n");
	    printf("\t-V\tDisplay version info and exit\n");
	    exit(0);
	case 'V':
	    printf("%s", RELEASEINFO);
	    exit(0);
	default:
	    fprintf(stderr, "usage: dolcon [-hlV]\n");
	    exit(1);
	}
    }
    dc_init();
    signal(SIGCHLD, SIG_IGN);
    pubhubaddr = sstrdup("http://www.hublist.org/PublicHubList.xml.bz2");
    dcserver = sstrdup("");
    if((pwent = getpwuid(getuid())) == NULL)
    {
	fprintf(stderr, "could not get your passwd data");
	exit(1);
    }
    connectas = sstrdup(pwent->pw_name);
    gtk_window_set_default_icon(gdk_pixbuf_new_from_xpm_data((const char **)dolda_icon_xpm));
    wnd = create_main_wnd();
    create_reslist_wnd();
    gtk_window_resize(GTK_WINDOW(reslist_wnd), 600, 400);
    initchattags();

    fnmodel = gtk_list_store_new(6, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_fnetnodes), GTK_TREE_MODEL(fnmodel));
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_chatnodes), GTK_TREE_MODEL(fnmodel));
    
    reslist = gtk_list_store_new(6, G_TYPE_STRING, G_TYPE_INT64, G_TYPE_INT64, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(reslist_list), GTK_TREE_MODEL(reslist));
    
    dlmodel = gtk_list_store_new(13, G_TYPE_INT, /* id */
				 G_TYPE_INT,     /* dir */
				 G_TYPE_INT,     /* state */
				 G_TYPE_STRING,  /* peerid */
				 G_TYPE_STRING,  /* peernick */
				 G_TYPE_STRING,  /* path */
				 G_TYPE_INT64,   /* size */
				 G_TYPE_INT64,   /* curpos */
				 G_TYPE_STRING,  /* stock */
				 G_TYPE_FLOAT,   /* percentage */
				 G_TYPE_INT,     /* error */
				 G_TYPE_INT,     /* errortime */
				 G_TYPE_STRING); /* hash */
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_downloads), GTK_TREE_MODEL(dlmodel));

    ulmodel = gtk_list_store_new(13, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT64, G_TYPE_INT64, G_TYPE_STRING, G_TYPE_FLOAT, G_TYPE_INT, G_TYPE_INT, G_TYPE_STRING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_uploads), GTK_TREE_MODEL(ulmodel));

    srchmodel = gtk_tree_store_new(10, G_TYPE_STRING, /* fnetname */
				   G_TYPE_STRING,     /* peerid */
				   G_TYPE_STRING,     /* peername */
				   G_TYPE_STRING,     /* filename */
				   G_TYPE_INT64,      /* size */
				   G_TYPE_INT,        /* slots */
				   G_TYPE_DOUBLE,     /* resptime */
				   G_TYPE_INT,        /* sizenum */
				   G_TYPE_INT,        /* speed */
				   G_TYPE_STRING);    /* hash */
    srchmodelfilter = GTK_TREE_MODEL_FILTER(gtk_tree_model_filter_new(GTK_TREE_MODEL(srchmodel), NULL));
    gtk_tree_model_filter_set_visible_func(srchmodelfilter, srchfilterfunc, NULL, NULL);
    sortmodel = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(srchmodelfilter));
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(sortmodel), 4, GTK_SORT_DESCENDING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(main_srchres), GTK_TREE_MODEL(sortmodel));
    g_object_unref(sortmodel);

    monospacefont = pango_font_description_from_string("Monospace 10");
    gtk_widget_modify_font(main_chatview, monospacefont);
    pango_font_description_free(monospacefont);
    readconfigfile();
    updatesbar(_("Disconnected"));
    gtk_widget_show(wnd);
    if(connlocal)
	dcconnect(dc_srv_local);
    else if(autoconn)
	dcconnect(dcserver);
    g_timeout_add(500, srchstatupdatecb, NULL);
    g_timeout_add(5000, ksupdatecb, NULL);
    g_timeout_add(1000, updatetransfers, NULL);
    gtk_main();
    return(0);
}

#include "mainwnd.gtk"
#include "inpdialog.gtk"
#include "pref.gtk"
#include "reslist.gtk"
