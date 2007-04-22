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
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <locale.h>
#include <libintl.h>
#include <pwd.h>
#include <stdarg.h>

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
};

char *cfname;
GtkWindow *rootwnd = NULL;
GtkListStore *shares;

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

int v_numeric(const char *val)
{
    int f;
    
    for(f = 1; *val; val++, f = 0) {
	if(!isdigit(val) && (!f || (*val != '-')))
	    return(0);
    }
    return(1);
}

#define _(text) text

struct validation nonempty = {
    .check = v_nonempty,
    .invmsg = _("%s must not be empty"),
};

struct validation dcstring = {
    .check = v_dcstring,
    .invmsg = _("%s must not contain spaces, `|' or `$'"),
};

struct validation numeric = {
    .check = v_numeric,
    .invmsg = _("%s must be numeric"),
};

struct validation *vldxlate[] = {
    &nonempty, &dcstring, &numeric,
    NULL
};

struct cfvar config[] = {
    {"cli.defnick", _("Nickname"), "", &dcstring},
    {"net.mode", NULL, "0", &numeric},
    {"ui.onlylocal", NULL, "0", &numeric},
    {"auth.authless", NULL, "0", &numeric},
    {"transfer.slots", _("Upload slots"), "6", &numeric},
    {"dc.speedstring", _("Connection speed"), "DSL", &dcstring},
    {"dc.desc", _("Share description"), "", NULL},
    {NULL}
};

#undef _
#define _(text) gettext(text)

void astcancel(GtkWidget *widget, gpointer uudata);
void astupdate(GtkWidget *widget, GtkWidget *page, gpointer uudata);
void cb_ast_wnd_apply(GtkWidget *widget, gpointer uudata);
void cb_ast_nick_changed(GtkWidget *widget, gpointer uudata);
void cb_ast_shareadd_clicked(GtkWidget *widget, gpointer uudata);
void cb_ast_sharerem_clicked(GtkWidget *widget, gpointer uudata);

#include "dolconf-assistant.gtk"

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
    free(v->val);
    v->val = sstrdup(val);
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
    len = p2 - *p;
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
}

void astcancel(GtkWidget *widget, gpointer uudata)
{
    gtk_main_quit();
}

#define bufcats(buf, str) bufcat(buf, str, strlen(str))

void astupdate(GtkWidget *widget, GtkWidget *page, gpointer uudata)
{
    char *s, *buf;
    size_t sdata, ssize;
    struct cfvar *var;
    GtkTreeIter iter;
    
    setcfvar("cli.defnick", gtk_entry_get_text(GTK_ENTRY(ast_nick)));
    setcfvar("dc.desc", gtk_entry_get_text(GTK_ENTRY(ast_desc)));
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

void cb_ast_shareadd_clicked(GtkWidget *widget, gpointer uudata)
{
    int i;
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
	return;
    }
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
	    gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page2, TRUE);
	}
	g_free(fn);
	next = fns->next;
	g_slist_free_1(fns);
	fns = next;
    }
}

void cb_ast_sharerem_clicked(GtkWidget *widget, gpointer uudata)
{
    GtkTreeIter iter;
    
    if(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(GTK_TREE_VIEW(ast_sharelist)), NULL, &iter))
	gtk_list_store_remove(shares, &iter);
    if(gtk_tree_model_iter_n_children(GTK_TREE_MODEL(shares), NULL) == 0)
	gtk_assistant_set_page_complete(GTK_ASSISTANT(ast_wnd), ast_page2, FALSE);
}

int main(int argc, char **argv)
{
    struct passwd *pwd;
    
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    prepstatic();
    
    gtk_init(&argc, &argv);
    create_ast_wnd();
    shares = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(GTK_TREE_VIEW(ast_sharelist), GTK_TREE_MODEL(shares));
    
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
    
    if(access(cfname, F_OK)) {
	if(msgbox(GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, _("It appears that you have not run this setup program before. Would you like to run the first-time setup assistant?")) == GTK_RESPONSE_YES) {
	    gtk_window_set_default_size(GTK_WINDOW(ast_wnd), 500, 350);
	    gtk_widget_show(ast_wnd);
	}
    } else {
	if(readconfig() == 1) {
	    if(msgbox(GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, _("The configuration file appears to have been edited outside the control of this program. If you continue using this program, all settings not handled by it will be lost. Do you wish to continue?")) == GTK_RESPONSE_NO)
		exit(1);
	}
    }
    gtk_main();
    return(0);
}
