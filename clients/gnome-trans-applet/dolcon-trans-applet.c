/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2005 Fredrik Tolf <fredrik@dolda2000.com>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <string.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/utils.h>
#include <panel-applet.h>
#include <gtk/gtk.h>
#include <time.h>

#include "conduit.h"

struct appletdata
{
    PanelApplet *applet;
    GtkLabel *label;
    GtkProgressBar *pbar;
    GtkTooltips *tips;
    gint tiptimeout;
    struct conduit *conduit;
    struct transfer *curdisplay;
};

static char *ctxtmenu =
"<popup name='button3'>"
"    <menuitem name='Preferences' verb='dca_pref' _label='Preferences' pixtype='stock' pixname='gtk-properties' />"
"    <menuitem name='Cancel transfer' verb='dca_cancel' _label='Cancel transfer' pixtype='stock' pixname='gtk-cancel' />"
"</popup>";

static void run_pref_dialog(BonoboUIComponent *uic, gpointer data, const char *cname)
{
}

static void cancel_transfer(BonoboUIComponent *uic, struct appletdata *data, const char *cname)
{
    if(data->conduit->iface->cancel != NULL)
	data->conduit->iface->cancel(data->conduit, data->curdisplay);
}

static BonoboUIVerb ctxtmenuverbs[] =
{
    BONOBO_UI_VERB("dca_pref", run_pref_dialog),
    BONOBO_UI_VERB("dca_cancel", (void (*)(BonoboUIComponent*, gpointer, const char *))cancel_transfer),
    BONOBO_UI_VERB_END
};

static gint reconncb(struct appletdata *data)
{
    condtryconn(data->conduit);
    return(FALSE);
}

static gboolean updatetip(struct appletdata *data)
{
    int diff, speed, left;
    time_t now;
    char buf[256];
    
    if(data->curdisplay == NULL) {
	gtk_tooltips_set_tip(data->tips, GTK_WIDGET(data->applet), _("No transfer selected"), NULL);
	return(TRUE);
    }
    now = time(NULL);
    if(data->curdisplay->cmptime == 0)
    {
	strcpy(buf, _("Calculating remaining time..."));
    } else {
	diff = data->curdisplay->pos - data->curdisplay->cmpsize;
	speed = diff / (now - data->curdisplay->cmptime);
	if(speed == 0)
	{
	    strcpy(buf, _("Time left: Infinite (Transfer is standing still)"));
	} else {
	    left = (data->curdisplay->size - data->curdisplay->pos) / speed;
	    if(left >= 86400)
		sprintf(buf, _("Time left: %id%02ih%02im"), left / 86400, (left / 3600) % 24, (left / 60) % 60);
	    else if(left >= 3600)
		sprintf(buf, _("Time left: %ih%02im"), left / 3600, (left / 60) % 60);
	    else
		sprintf(buf, _("Time left: %im"), left / 60);
	}
    }
    gtk_tooltips_set_tip(data->tips, GTK_WIDGET(data->applet), buf, NULL);
    return(TRUE);
}

static void update(struct appletdata *data)
{
    char buf[256];
    size_t l;
    
    switch(data->conduit->state)
    {
    case CNDS_IDLE:
	gtk_progress_bar_set_text(data->pbar, _("Not connected"));
	gtk_label_set_text(data->label, "");
	break;
    case CNDS_SYN:
	gtk_progress_bar_set_text(data->pbar, _("Connecting..."));
	gtk_label_set_text(data->label, "");
	break;
    case CNDS_EST:
	if(data->conduit->transfers == NULL)
	{
	    gtk_progress_bar_set_fraction(data->pbar, 0);
	    gtk_progress_bar_set_text(data->pbar, "");
	    gtk_label_set_text(data->label, _("No transfers to display"));
	} else if(data->curdisplay == NULL) {
	    gtk_progress_bar_set_fraction(data->pbar, 0);
	    gtk_progress_bar_set_text(data->pbar, "");
	    gtk_label_set_text(data->label, _("No transfer selected"));
	} else {
	    if((data->curdisplay->pos > 0) && (data->curdisplay->size > 0))
	    {
		sprintf(buf, "%'ji/%'ji", (intmax_t)data->curdisplay->pos, (intmax_t)data->curdisplay->size);
		gtk_progress_bar_set_fraction(data->pbar, (double)data->curdisplay->pos / (double)data->curdisplay->size);
		gtk_progress_bar_set_text(data->pbar, buf);
	    } else {
		gtk_progress_bar_set_fraction(data->pbar, 0);
		gtk_progress_bar_set_text(data->pbar, _("Initializing"));
	    }
	    if((l = strlen(data->curdisplay->tag)) > 50) {
		memcpy(buf, data->curdisplay->tag, 20);
		memcpy(buf + 20, "...", 3);
		memcpy(buf + 23 , data->curdisplay->tag + l - 20, 20);
		buf[43] = 0;
		gtk_label_set_text(data->label, buf);
	    } else {
		gtk_label_set_text(data->label, data->curdisplay->tag);
	    }
	}
	break;
    }
}

static void trsize(struct transfer *transfer, struct appletdata *data)
{
    update(data);
}

static void trpos(struct transfer *transfer, struct appletdata *data)
{
    update(data);
}

static void trnew(struct transfer *transfer, struct appletdata *data)
{
    if(data->curdisplay == NULL)
	data->curdisplay = transfer;
    update(data);
}

static void trfree(struct transfer *transfer, struct appletdata *data)
{
    if(data->curdisplay == transfer)
	data->curdisplay = data->conduit->transfers;
    update(data);
}

static void condstate(struct conduit *conduit, struct appletdata *data)
{
    if(conduit->state == CNDS_IDLE)
	g_timeout_add(10000, (gboolean (*)(gpointer))reconncb, data);
    update(data);
}

static void initcond(void)
{
    static int inited = 0;
    
    if(!inited)
    {
	cb_trsize = (void (*)(struct transfer *, void *))trsize;
	cb_trpos = (void (*)(struct transfer *, void *))trpos;
	cb_trnew = (void (*)(struct transfer *, void *))trnew;
	cb_trfree = (void (*)(struct transfer *, void *))trfree;
	cb_condstate = (void (*)(struct conduit *, void *))condstate;
	inited = 1;
    }
}

static gboolean trview_applet_button_press(GtkWidget *widget, GdkEventButton *event, struct appletdata *data)
{
    if(event->button == 1)
    {
	if(data->curdisplay == NULL)
	    data->curdisplay = data->conduit->transfers;
	else if(data->curdisplay->next == NULL)
	    data->curdisplay = data->conduit->transfers;
	else
	    data->curdisplay = data->curdisplay->next;
	update(data);
    }
    return(FALSE);
}

static gboolean trview_applet_scroll(GtkWidget *widget, GdkEventScroll *event, struct appletdata *data)
{
    struct transfer *tr;
    
    if(event->direction == GDK_SCROLL_DOWN)
    {
	if(data->curdisplay == NULL)
	    data->curdisplay = data->conduit->transfers;
	else if(data->curdisplay->next == NULL)
	    data->curdisplay = data->conduit->transfers;
	else
	    data->curdisplay = data->curdisplay->next;
	update(data);
    } else if(event->direction == GDK_SCROLL_UP) {
	if(data->curdisplay == NULL)
	{
	    data->curdisplay = data->conduit->transfers;
	} else if(data->curdisplay->prev == NULL) {
	    for(tr = data->conduit->transfers; tr->next != NULL; tr = tr->next);
	    data->curdisplay = tr;
	} else {
	    data->curdisplay = data->curdisplay->prev;
	}
	update(data);
    }
    return(TRUE);
}

static void trview_applet_destroy(GtkWidget *widget, struct appletdata *data)
{
    freeconduit(data->conduit);
    g_source_remove(data->tiptimeout);
    g_object_unref(data->applet);
    g_object_unref(data->tips);
    free(data);
}

static gboolean trview_applet_fill(PanelApplet *applet, const gchar *iid, gpointer uudata)
{
    GtkWidget *hbox, *pbar, *label;
    struct appletdata *data;
    
    initcond();
    if(strcmp(iid, "OAFIID:Dolcon_Transferapplet"))
	return(FALSE);
    
    hbox = gtk_hbox_new(FALSE, 0);
    label = gtk_label_new("");
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 5);
    pbar = gtk_progress_bar_new();
    gtk_box_pack_start(GTK_BOX(hbox), pbar, TRUE, TRUE, 0);
    gtk_container_add(GTK_CONTAINER(applet), hbox);
    gtk_widget_show_all(GTK_WIDGET(applet));
    
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    g_object_ref(data->applet = applet);
    data->conduit = newconduit(conduit_dclib, data);
    data->pbar = GTK_PROGRESS_BAR(pbar);
    g_object_ref(data->tips = gtk_tooltips_new());
    data->tiptimeout = g_timeout_add(500, (gboolean (*)(gpointer))updatetip, data);
    data->label = GTK_LABEL(label);
    
    panel_applet_setup_menu(applet, ctxtmenu, ctxtmenuverbs, data);

    g_signal_connect(applet, "button-press-event", (GCallback)trview_applet_button_press, data);
    g_signal_connect(applet, "scroll-event", (GCallback)trview_applet_scroll, data);
    g_signal_connect(applet, "destroy", (GCallback)trview_applet_destroy, data);
    
    condtryconn(data->conduit);
    
    update(data);
    
    return(TRUE);
}

#define GETTEXT_PACKAGE PACKAGE
#define GNOMELOCALEDIR LOCALEDIR

PANEL_APPLET_BONOBO_FACTORY("OAFIID:Dolcon_Transferapplet_Factory",
			    PANEL_TYPE_APPLET,
			    "Doldaconnect Transfer Viewer",
			    "0",
			    trview_applet_fill,
			    NULL);
