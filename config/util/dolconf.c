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
#include <signal.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <locale.h>
#include <libintl.h>
#include <pwd.h>
#include <stdarg.h>
#include <arpa/inet.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <utils.h>

struct validation {
    int (*check)(const char *val);
    char *invmsg;
};

struct cfvar {
    char *name;
    char *rname;
    char *val;
    struct validation *vld;
    GtkWidget **astw, **cfww;
};

char *cfname;
GtkWindow *rootwnd = NULL;
GtkListStore *shares;
int state, dirty = 1;
int ignoreclose = 0;

#define _(text) gettext(text)

#include "dolconf-assistant.gtkh"
#include "dolconf-wnd.gtkh"

int v_nonempty(const char *val)
{
    return(strlen(val) > 0);
}

int v_dcstring(const char *val)
{
    return((strchr(val, ' ') == NULL) &&
	   (strchr(val, '|') == NULL) &&
	   (strchr(val, '$') == NULL));
}

int v_natural(const char *val)
{
    if(!*val)
	return(0);
    for(; *val; val++) {
	if(!isdigit(*val))
	    return(0);
    }
    return(1);
}

int v_integer(const char *val)
{
    int f, d;
    
    for(f = 1, d = 0; *val; val++, f = 0) {
	if(isdigit(*val)) {
	    d = 1;
	} else if(f && (*val == '-')) {
	} else {
	    return(0);
	}
    }
    return(d);
}

int v_ipv4(const char *val)
{
    struct in_addr buf;
    
    return(inet_aton(val, &buf) != 0);
}

#undef _
#define _(text) text

struct validation nonempty = {
    .check = v_nonempty,
    .invmsg = _("%s must not be empty"),
};

struct validation dcstring = {
    .check = v_dcstring,
    .invmsg = _("%s must not contain spaces, `|' or `$'"),
};

struct validation natural = {
    .check = v_natural,
    .invmsg = _("%s must be a natural number"),
};

struct validation integer = {
    .check = v_integer,
    .invmsg = _("%s must be an integer"),
};

struct validation ipv4 = {
    .check = v_ipv4,
    .invmsg = _("%s must be an IP address"),
};

struct validation *vldxlate[] = {
    &nonempty, &dcstring, &natural, &integer, &ipv4,
    NULL
};

struct cfvar config[] = {
    {"cli.defnick", _("Screen name"), "", &dcstring, &ast_nick, &cfw_nick},
    {"net.mode", NULL, "0", &natural},
    {"net.visibleipv4", "IP address", "0.0.0.0", &ipv4, NULL, &cfw_extip},
    {"ui.onlylocal", NULL, "0", &natural},
    {"ui.port", NULL, "-1", &integer},
    {"auth.authless", NULL, "0", &natural},
    {"transfer.slots", _("Upload slots"), "6", &natural},
    {"dc.speedstring", _("Connection speed"), "DSL", &dcstring, NULL, &cfw_cntype},
    {"dc.email", _("E-mail address"), "spam@spam.net", &dcstring, NULL, &cfw_mail},
    {"dc.desc", _("Share description"), "", NULL, &ast_desc, &cfw_desc},
    {"dc.tcpport", _("Direct Connect TCP port"), "0", &natural, NULL, &cfw_tcpport},
    {"dc.udpport", _("Direct Connect UDP port"), "0", &natural, NULL, &cfw_udpport},
    {NULL}
};

#undef _
#define _(text) gettext(text)

struct cfvar *findcfvar(char *name)
{
    struct cfvar *v;
    
    for(v = config; v->name != NULL; v++) {
	if(!strcmp(v->name, name))
	    return(v);
    }
    return(NULL);
}

void setcfvar(char *name, const char *val)
{
    struct cfvar *v;
    
    v = findcfvar(name);
    if(!strcmp(v->val, val))
	return;
    free(v->val);
    v->val = sstrdup(val);
    dirty = 1;
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
    swnd = gtk_message_dialog_new(rootwnd, GTK_DIALOG_MODAL, type, buttons, "%s", buf);
    gtk_window_set_title(GTK_WINDOW(swnd), _("Dolda Connect configurator"));
    resp = gtk_dialog_run(GTK_DIALOG(swnd));
    gtk_widget_destroy(swnd);
    free(buf);
    return(resp);
}

void prepstatic(void)
{
    struct validation **v;
    struct cfvar *c;
    
    for(v = vldxlate; *v != NULL; v++)
	(*v)->invmsg = gettext((*v)->invmsg);
    for(c = config; c->name != NULL; c++) {
	if(c->rname != NULL)
	    c->rname = gettext(c->rname);
	c->val = sstrdup(c->val);
    }
}

char *getword(char **p)
{
    char *buf, *p2, delim;
    size_t len;
    
    if(**p == '\"')
	delim = '\"';
    else
	delim = ' ';
    p2 = *p;
    while((p2 = strchr(p2 + 1, delim)) != NULL) {
	if(p2[-1] != '\\')
	    break;
    }
    if(p2 == NULL)
	p2 = *p + strlen(*p);
    len = p2 - *p - ((*p2 == '\"')?1:0);
    buf = smalloc(len + 1);
    memcpy(buf, *p, len);
    buf[len] = 0;
    *p = p2 + ((*p2 == '\"')?1:0);
    for(p2 = buf; *p2; p2++, len--) {
	if(*p2 == '\\')
	    memmove(p2, p2 + 1, len--);
    }
    return(buf);
}

char *quoteword(char *word)
{
    char *wp, *buf, *bp;
    int dq, numbs, numc;
    
    dq = 0;
    numbs = 0;
    numc = 0;
    if(*word == '\0')
    {
	dq = 1;
    } else {
	for(wp = word; *wp != '\0'; wp++)
	{
	    if(!dq && isspace(*wp))
		dq = 1;
	    if((*wp == '\\') || (*wp == '\"'))
		numbs++;
	    numc++;
	}
    }
    if(!dq && !numbs)
	return(NULL);
    bp = buf = smalloc(sizeof(wchar_t) * (numc + numbs + (dq?2:0) + 1));
    if(dq)
	*(bp++) = '\"';
    for(wp = word; *wp != '\0'; wp++)
    {
	if((*wp == '\\') || (*wp == '\"'))
	    *(bp++) = '\\';
	*(bp++) = *wp;
    }
    if(dq)
	*(bp++) = '\"';
    *(bp++) = '\0';
    return(buf);
}

int readconfig(void)
{
    int rv;
    FILE *cf;
    char lbuf[1024];
    char *key, *val, *p;
    size_t len;
    struct cfvar *var;
    GtkTreeIter iter;
    
    rv = 0;
    if((cf = fopen(cfname, "r")) == NULL) {
	msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("Could not open the configuration file for reading: %s"), strerror(errno));
	return(-1);
    }
    key = val = NULL;
    while(fgets(lbuf, sizeof(lbuf), cf) != NULL) {
	len = strlen(lbuf);
	if(lbuf[len - 1] == '\n')
	    lbuf[len - 1] = 0;
	if(key != NULL) {
	    free(key);
	    key = NULL;
	}
	if(val != NULL) {
	    free(val);
	    val = NULL;
	}
	if(!strncmp(lbuf, "set ", 4)) {
	    p = lbuf + 4;
	    if(((key = getword(&p)) == NULL) || (*(p++) != ' ') || ((val = getword(&p)) == NULL)) {
		rv = 1;
		continue;
	    }
	    for(var = config; var->name != NULL; var++) {
		if(!strcmp(var->name, key)) {
		    free(var->val);
		    var->val = sstrdup(val);
		    break;
		}
	    }
	    if(var->name == NULL)
		rv = 1;
	} else if(!strncmp(lbuf, "share ", 6)) {
	    p = lbuf + 6;
	    if(((key = getword(&p)) == NULL) || (*(p++) != ' ') || ((val = getword(&p)) == NULL)) {
		rv = 1;
		continue;
	    }
	    gtk_list_store_append(shares, &iter);
	    gtk_list_store_set(shares, &iter, 0, key, 1, val, -1);
	} else if(!lbuf[0] || lbuf[0] == '#') {
	} else {
	    rv = 1;
	}
    }
    if(key != NULL)
	free(key);
    if(val != NULL)
	free(val);
    fclose(cf);
    dirty = 0;
    return(rv);
}

void writeconfig(void)
{
    FILE *cf;
    struct cfvar *var;
    GtkTreeIter iter;
    char *buf, *buf2;
    
    if((cf = fopen(cfname, "w")) == NULL) {
	msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("Could not open the configuration file for writing: %s"), strerror(errno));
	return;
    }
    fputs("# This file was generated by dolconf v" VERSION "\n\n", cf);
    for(var = config; var->name != NULL; var++) {
	fprintf(cf, "set %s ", var->name);
	if((buf = quoteword(var->val)) == NULL) {
	    fputs(var->val, cf);
	} else {
	    fputs(buf, cf);
	    free(buf);
	}
	fputc('\n', cf);
    }
    if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(shares), &iter)) {
	fputc('\n', cf);
	do {
	    fputs("share ", cf);
	    gtk_tree_model_get(GTK_TREE_MODEL(shares), &iter, 0, &buf2, -1);
	    if((buf = quoteword(buf2)) == NULL) {
		fputs(buf2, cf);
	    } else {
		fputs(buf, cf);
		free(buf);
	    }
	    g_free(buf2);
	    fputc(' ', cf);
	    gtk_tree_model_get(GTK_TREE_MODEL(shares), &iter, 1, &buf2, -1);
	    if((buf = quoteword(buf2)) == NULL) {
		fputs(buf2, cf);
	    } else {
		fputs(buf, cf);
		free(buf);
	    }
	    g_free(buf2);
	    fputc('\n', cf);
	} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(shares), &iter));
    }
    fclose(cf);
    dirty = 0;
}

void fillcfw(void)
{
    struct cfvar *var;
    
    for(var = config; var->name != NULL; var++) {
	if(var->cfww != NULL)
	    gtk_entry_set_text(GTK_ENTRY(*(var->cfww)), var->val);
    }
    if(atoi(findcfvar("dc.tcpport")->val) || atoi(findcfvar("dc.udpport")->val))
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_orport), TRUE);
    else
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_orport), FALSE);
    if(strcmp(findcfvar("net.visibleipv4")->val, "0.0.0.0"))
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_oraddr), TRUE);
    else
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_oraddr), FALSE);
    if(strcmp(findcfvar("ui.port")->val, "-1")) {
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_uinet), TRUE);
	if(strcmp(findcfvar("auth.authless")->val, "1"))
	    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_authless), FALSE);
	else
	    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_authless), TRUE);
    } else {
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_uinet), FALSE);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cfw_authless), FALSE);
    }
}

void ast2conf(void)
{
    setcfvar("cli.defnick", gtk_entry_get_text(GTK_ENTRY(ast_nick)));
    setcfvar("dc.desc", gtk_entry_get_text(GTK_ENTRY(ast_desc)));
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ast_mode_psv))) {
	setcfvar("net.mode", "1");
    } else {
	setcfvar("net.mode", "0");
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ast_mode_nat))) {
	    setcfvar("net.visibleipv4", gtk_entry_get_text(GTK_ENTRY(ast_extip)));
	    setcfvar("dc.tcpport", gtk_entry_get_text(GTK_ENTRY(ast_udpport)));
	    setcfvar("dc.udpport", gtk_entry_get_text(GTK_ENTRY(ast_tcpport)));
	} else {
	    setcfvar("net.visibleipv4", "0.0.0.0");
	    setcfvar("dc.tcpport", "0");
	    setcfvar("dc.udpport", "0");
	}
    }
}

void cfw2conf(void)
{
    struct cfvar *var;
    const char *val;
    
    for(var = config; var->name != NULL; var++) {
	if(var->cfww != NULL) {
	    val = gtk_entry_get_text(GTK_ENTRY(*(var->cfww)));
	    if(!strcmp(var->val, val))
		continue;
	    free(var->val);
	    var->val = sstrdup(val);
	    dirty = 1;
	}
    }
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cfw_mode_act))) {
	setcfvar("net.mode", "0");
	if(!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cfw_orport))) {
	    setcfvar("dc.tcpport", "0");
	    setcfvar("dc.udpport", "0");
	}
	if(!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cfw_oraddr))) {
	    setcfvar("net.visibleipv4", "0.0.0.0");
	}
    } else {
	setcfvar("net.mode", "1");
    }
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cfw_uinet))) {
	setcfvar("ui.port", "1500");
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cfw_authless)))
	    setcfvar("auth.authless", "1");
	else
	    setcfvar("auth.authless", "0");
    } else {
	setcfvar("ui.port", "-1");
	setcfvar("auth.authless", "0");
    }
}

struct cfvar *cfwvalid(void)
{
    struct cfvar *cv;
    
    for(cv = config; cv->name != NULL; cv++) {
	if((cv->vld != NULL) && !cv->vld->check(cv->val)) {
	    if(cv->rname) {
		return(cv);
	    } else {
		msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Internal error (Auto-generated variable %s has an invalid value \"%s\")"), cv->name, cv->val);
		abort();
	    }
	}
    }
    return(NULL);
}

void astcancel(GtkWidget *widget, gpointer uudata)
{
    if(ignoreclose)
	return;
    gtk_main_quit();
    state = -1;
}

#define bufcats(buf, str) bufcat(buf, str, strlen(str))

void astupdate(GtkWidget *widget, GtkWidget *page, gpointer uudata)
{
    char *s, *buf;
    size_t sdata, ssize;
    struct cfvar *var;
    GtkTreeIter iter;
    
    ast2conf();
    s = NULL;
    sdata = ssize = 0;
    for(var = config; var->name != NULL; var++) {
	if(var->rname == NULL)
	    continue;
	bufcats(s, var->rname);
	bufcats(s, ": ");
	bufcat(s, var->val, strlen(var->val));
	addtobuf(s, '\n');
    }
    if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(shares), &iter)) {
	addtobuf(s, '\n');
	bufcats(s, _("Shares:\n"));
	do {
	    addtobuf(s, '\t');
	    gtk_tree_model_get(GTK_TREE_MODEL(shares), &iter, 1, &buf, -1);
	    bufcats(s, buf);
	    g_free(buf);
	    addtobuf(s, '\n');
	} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(shares), &iter));
    }
    gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(ast_summary)), s, sdata);
    free(s);
}

void cb_ast_wnd_apply(GtkWidget *widget, gpointer uudata)
{
    writeconfig();
    gtk_main_quit();
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ast_action_dolcon)))
	state = 2;
    else if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ast_action_exit)))
	state = -1;
    else
	state = 0;
    ignoreclose = 1;
}

void cb_ast_nick_changed(GtkWidget *widget, gpointer uudata)
{
    if(v_dcstring(gtk_entry_get_text(GTK_ENTRY(ast_nick))))
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page1, TRUE);
    else
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page1, FALSE);
}

int hasshare(int col, char *name)
{
    GtkTreeIter iter;
    char *buf;
    
    if(gtk_tree_model_get_iter_first(GTK_TREE_MODEL(shares), &iter)) {
	do {
	    gtk_tree_model_get(GTK_TREE_MODEL(shares), &iter, col, &buf, -1);
	    if(!strcmp(buf, name)) {
		g_free(buf);
		return(1);
	    }
	    g_free(buf);
	} while(gtk_tree_model_iter_next(GTK_TREE_MODEL(shares), &iter));
    }
    return(0);
}

int shareadd(void)
{
    int i, ret;
    GSList *fns, *next;
    char *fn, *sn, *p;
    GtkTreeIter iter;
    GtkWidget *chd;
    int resp;
    
    chd = gtk_file_chooser_dialog_new(_("Shared directories"), rootwnd, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, NULL);
    gtk_file_chooser_set_local_only(GTK_FILE_CHOOSER(chd), TRUE);
    gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(chd), TRUE);
    resp = gtk_dialog_run(GTK_DIALOG(chd));
    if(resp != GTK_RESPONSE_ACCEPT) {
	gtk_widget_destroy(chd);
	return(0);
    }
    ret = 0;
    fns = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(chd));
    gtk_widget_destroy(chd);
    while(fns != NULL) {
	fn = fns->data;
	if(!hasshare(1, fn)) {
	    if((p = strrchr(fn, '/')) == NULL)
		p = fn;
	    else
		p++;
	    sn = sstrdup(p);
	    if(hasshare(0, sn)) {
		for(i = 2; 1; i++) {
		    free(sn);
		    sn = sprintf2("%s%i", p, i);
		    if(!hasshare(0, sn))
			break;
		}
	    }
	    gtk_list_store_append(shares, &iter);
	    gtk_list_store_set(shares, &iter, 0, sn, 1, fn, -1);
	    free(sn);
	    ret = 1;
	}
	g_free(fn);
	next = fns->next;
	g_slist_free_1(fns);
	fns = next;
    }
    return(ret);
}

void cb_ast_shareadd_clicked(GtkWidget *widget, gpointer uudata)
{
    if(shareadd())
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page2, TRUE);
}

void cb_cfw_shareadd_clicked(GtkWidget *widget, gpointer uudata)
{
    if(shareadd())
	dirty = 1;
}

void cb_ast_sharerem_clicked(GtkWidget *widget, gpointer uudata)
{
    GtkTreeIter iter;
    
    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(ast_sharelist)), NULL, &iter))
	gtk_list_store_remove(shares, &iter);
    if(gtk_tree_model_iter_n_children(GTK_TREE_MODEL(shares), NULL) == 0)
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page2, FALSE);
}

void cb_cfw_sharerem_clicked(GtkWidget *widget, gpointer uudata)
{
    GtkTreeIter iter;
    
    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(cfw_sharelist)), NULL, &iter)) {
	gtk_list_store_remove(shares, &iter);
	dirty = 1;
    }
}

void cb_ast_checkports(GtkWidget *widget, gpointer uudata)
{
    gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page3,
				    v_natural(gtk_entry_get_text(GTK_ENTRY(ast_tcpport))) &&
				    (atoi(gtk_entry_get_text(GTK_ENTRY(ast_tcpport))) >= 1024) &&
				    v_natural(gtk_entry_get_text(GTK_ENTRY(ast_udpport))) &&
				    (atoi(gtk_entry_get_text(GTK_ENTRY(ast_udpport))) >= 1024) &&
				    v_ipv4(gtk_entry_get_text(GTK_ENTRY(ast_extip))));
}

void cb_ast_mode_nat_toggled(GtkWidget *widget, gpointer uudata)
{
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
	gtk_widget_set_sensitive(GTK_WIDGET(ast_portbox), TRUE);
	cb_ast_checkports(widget, NULL);
    } else {
	gtk_widget_set_sensitive(GTK_WIDGET(ast_portbox), FALSE);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page3, TRUE);
    }
}

void cb_cfw_mode_act_toggled(GtkWidget *widget, gpointer uudata)
{
    gtk_widget_set_sensitive(GTK_WIDGET(cfw_natbox), gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void cb_cfw_orport_toggled(GtkWidget *widget, gpointer uudata)
{
    gtk_widget_set_sensitive(GTK_WIDGET(cfw_portbox), gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void cb_cfw_oraddr_toggled(GtkWidget *widget, gpointer uudata)
{
    gtk_widget_set_sensitive(GTK_WIDGET(cfw_addrbox), gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void cb_cfw_uinet_toggled(GtkWidget *widget, gpointer uudata)
{
    gtk_widget_set_sensitive(GTK_WIDGET(cfw_uibox), gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void cb_cfw_hup_activate(GtkWidget *widget, gpointer uudata)
{
    int tag;
    struct dc_response *resp;
    
    if(dc_connectsync2(dc_srv_local, DC_LATEST) < 0) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server"));
	return;
    }
    if(dc_login(NULL, 1, dc_convnone, NULL) != DC_LOGIN_ERR_SUCCESS) {
	dc_disconnect();
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server"));
	return;
    }
    tag = dc_queuecmd(NULL, NULL, L"hup", NULL);
    if((resp = dc_gettaggedrespsync(tag)) != NULL) {
	if(resp->code != 200)
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not connect to server"));
	dc_freeresp(resp);
    }
    dc_disconnect();
}

void cb_cfw_save_activate(GtkWidget *widget, gpointer uudata)
{
    struct cfvar *cv;
    
    if((cv = cfwvalid()) != NULL) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, cv->vld->invmsg, cv->rname);
	return;
    }
    cfw2conf();
    writeconfig();
}

void cb_cfw_quit_activate(GtkWidget *widget, gpointer uudata)
{
    cfw2conf();
    if(dirty) {
	if(msgbox(GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, _("There are unsaved changes. Do you wish to discard the changes and exit anyway?")) == GTK_RESPONSE_NO)
	    return;
    }
    gtk_main_quit();
    state = -1;
}

int main(int argc, char **argv)
{
    int i, c, ex;
    struct passwd *pwd;
    
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    prepstatic();
    dc_init();
    
    gtk_init(&argc, &argv);
    state = -1;
    while((c = getopt(argc, argv, "haw")) != -1) {
	switch(c) {
	case 'a':
	    state = 1;
	    break;
	case 'w':
	    state = 0;
	    break;
	case 'h':
	    printf("usage: dolconf [-haw]\n");
	    printf("\t-h\tDisplay this help message\n");
	    printf("\t-a\tGo directly to the assistant\n");
	    printf("\t-w\tGo directly to the standard window\n");
	    exit(0);
	default:
	    fprintf(stderr, "usage: dolconf [-haw]\n");
	    exit(1);
	}
    }
    
    create_ast_wnd();
    create_cfw_wnd();
    shares = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(ast_sharelist), GTK_TREE_MODEL(shares));
    gtk_tree_view_set_model(GTK_TREE_VIEW(cfw_sharelist), GTK_TREE_MODEL(shares));
    
    cfname = NULL;
    if(getenv("HOME") != NULL) {
	cfname = sprintf2("%s/.doldacond.conf", getenv("HOME"));
    } else {
	if((pwd = getpwuid(getuid())) != NULL)
	    cfname = sprintf2("%s/.doldacond.conf", pwd->pw_dir);
    }
    if(cfname == NULL) {
	msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Could not get your home directory!"));
	exit(1);
    }
    
    ex = !access(cfname, F_OK);
    if(state == -1) {
	if(!ex) {
	    if(msgbox(GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, _("It appears that you have not run this setup program before. Would you like to run the first-time setup assistant?")) == GTK_RESPONSE_YES)
		state = 1;
	    else
		state = 0;
	} else {
	    state = 0;
	}
    }
    
    if(ex && (state == 0)) {
	if(readconfig() == 1) {
	    if(msgbox(GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, _("The configuration file appears to have been edited outside the control of this program. If you continue using this program, all settings not handled by it will be lost. Do you wish to continue?")) == GTK_RESPONSE_NO)
		exit(1);
	}
    }
    
    while(state != -1) {
	if(state == 0) {
	    gtk_window_set_default_size(GTK_WINDOW(cfw_wnd), 500, 350);
	    gtk_widget_show(cfw_wnd);
	    fillcfw();
	    rootwnd = GTK_WINDOW(cfw_wnd);
	    gtk_main();
	    gtk_widget_hide(cfw_wnd);
	    rootwnd = NULL;
	} else if(state == 1) {
	    gtk_window_set_default_size(GTK_WINDOW(ast_wnd), 500, 350);
	    gtk_widget_show(ast_wnd);
	    rootwnd = GTK_WINDOW(ast_wnd);
	    gtk_main();
	    gtk_widget_hide(ast_wnd);
	    ignoreclose = 0;
	    rootwnd = NULL;
	} else if(state == 2) {
	    for(i = 3; i < FD_SETSIZE; i++)
		close(i);
	    execlp("dolcon-launch", "dolcon-launch", NULL);
	    perror("dolcon-launch");
	    exit(127);
	} else {
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("Internal error (Unknown state)"));
	    abort();
	}
    }
    return(0);
}

#include "dolconf-assistant.gtk"
#include "dolconf-wnd.gtk"
