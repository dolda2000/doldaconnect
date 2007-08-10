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
#include <string.h>
#include <gtk/gtk.h>
#include <doldaconnect/utils.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "dolcon.h"
#include "hublist.h"

int pubhuboldhandler(int op, char *buf, size_t len)
{
    static GtkListStore *model = NULL;
    int i;
    char *p, *p2;
    wchar_t *wbuf;
    char *fields[4], *names[3];
    int cols[3];
    GtkTreeIter iter;
    
    switch(op)
    {
    case PHO_INIT:
	model = gtk_list_store_new(4, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT);
	break;
    case PHO_DATA:
	p = buf;
	while((p = memchr((p2 = p), '\n', len - (p - buf))) != NULL)
	{
	    *(p++) = 0;
	    for(i = 0; i < 4; i++) {
		fields[i] = p2;
		if((p2 = strchr(p2, '|')) == NULL)
		    break;
		*(p2++) = 0;
	    }
	    if(i == 4) {
		for(i = 0; i < 4; i++) {
		    if((wbuf = icsmbstowcs(fields[i], DCCHARSET, NULL)) == NULL) {
			fields[i] = sstrdup(_("(Invalid character)"));
		    } else {
			if((fields[i] = icwcstombs(wbuf, "UTF-8")) == NULL)
			    break;
		    }
		}
		if(i == 4) {
		    if(validhub(fields[0], fields[2], NULL)) {
			gtk_list_store_append(model, &iter);
			gtk_list_store_set(model, &iter, 0, fields[1], 1, fields[0], 2, fields[2], 3, atoi(fields[3]), -1);
		    }
		}
		for(i--; i >= 0; i--)
		    free(fields[i]);
	    }
	}
	return(p2 - buf);
	break;
    case PHO_EOF:
	cols[0] = 3; names[0] = _("# users");
	cols[1] = 1; names[1] = _("Name");
	cols[2] = 2; names[2] = _("Description");
	setpubhubmodel(GTK_TREE_MODEL(model), 3, 3, cols, names);
	break;
    case PHO_FINI:
	if(model != NULL)
	    g_object_unref(model);
	model = NULL;
	break;
    }
    return(0);
}
