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
#include <doldaconnect/utils.h>
#include <gtk/gtk.h>

/* "Programming with libxml2 is like the thrilling embrace of an
 * exotic strangler."
 *    --Me */
#include <libxml/parser.h>
#include <libxml/tree.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "dolcon.h"
#include "hublist.h"

static xmlNodePtr findnode(xmlNodePtr node, char *name)
{
    for(; node != NULL; node = node->next)
    {
	if(!strcmp((char *)node->name, name))
	    break;
    }
    return(node);
}

static int checkvalid(xmlNodePtr n)
{
    int match;
    char *name, *descr;
    
    name = (char *)xmlGetProp(n, (xmlChar *)"Name");
    descr = (char *)xmlGetProp(n, (xmlChar *)"Description");
    match = validhub(name, descr, NULL);
    xmlFree(name);
    if(descr != NULL)
	xmlFree(descr);
    return(match);
}

int pubhubxmlhandler(int op, char *buf, size_t len)
{
    static xmlParserCtxtPtr ctxt = NULL;
    int i;
    xmlNodePtr dr, r, cr, c, n;
    int numcols, *cols, sortcol;
    GType type, *types;
    char **names, *name, *stype, *attr;
    GtkListStore *model;
    GtkTreeIter iter;
    
    numcols = 0;
    names = NULL;
    types = NULL;
    switch(op)
    {
    case PHO_INIT:
	break;
    case PHO_DATA:
	if(ctxt == NULL) {
	    ctxt = xmlCreatePushParserCtxt(NULL, NULL, buf, len, NULL);
	    if(ctxt == NULL)
		return(-1);
	} else {
	    xmlParseChunk(ctxt, buf, len, 0);
	}
	return(len);
    case PHO_EOF:
	if(ctxt == NULL)
	{
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("A hub list could not be read from the server"));
	    break;
	}
	xmlParseChunk(ctxt, NULL, 0, 1);
	if(!ctxt->wellFormed)
	{
	    msgbox(GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, _("The hub list is not valid"));
	    break;
	}
	dr = r = cr = NULL;
	dr = xmlDocGetRootElement(ctxt->myDoc);
	if(dr != NULL)
	    r = findnode(dr->children, "Hubs");
	if(r != NULL)
	    cr = findnode(r->children, "Columns");
	if(cr == NULL)
	{
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The hub list cannot be understood"));
	    break;
	}
	for(c = findnode(cr->children, "Column"); c != NULL; c = findnode(c->next, "Column"))
	{
	    name = (char *)xmlGetProp(c, (xmlChar *)"Name");
	    stype = (char *)xmlGetProp(c, (xmlChar *)"Type");
	    type = G_TYPE_INVALID;
	    if(stype != NULL)
	    {
		if(!strcmp(stype, "string"))
		    type = G_TYPE_STRING;
		else if(!strcmp(stype, "int"))
		    type = G_TYPE_INT;
		else if(!strcmp(stype, "bytes"))
		    type = G_TYPE_INT64;
	    }
	    if((name != NULL) && (type != G_TYPE_INVALID))
	    {
		names = srealloc(names, (numcols + 1) * sizeof(*names));
		types = srealloc(types, (numcols + 1) * sizeof(*names));
		names[numcols] = sstrdup(name);
		types[numcols] = type;
		numcols++;
	    }
	    if(name != NULL)
		xmlFree(name);
	    if(stype != NULL)
		xmlFree(stype);
	}
	if(numcols == 0)
	{
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The hub list did not contain any columns"));
	    break;
	}
	for(i = 0; i < numcols; i++)
	{
	    if(!strcmp(names[i], "Address"))
	    {
		name = names[0];
		names[0] = names[i];
		names[i] = name;
		type = types[0];
		types[0] = types[i];
		types[i] = type;
		break;
	    }
	}
	if(i == numcols)
	{
	    msgbox(GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, _("The hub list did not contain the address to any hubs"));
	    break;
	}
	model = gtk_list_store_newv(numcols, types);
	for(n = findnode(r->children, "Hub"); n != NULL; n = findnode(n->next, "Hub"))
	{
	    if(!xmlHasProp(n, (xmlChar *)"Address") || !xmlHasProp(n, (xmlChar *)"Name"))
		continue;
	    if(!checkvalid(n))
		continue;
	    gtk_list_store_append(model, &iter);
	    for(i = 0; i < numcols; i++)
	    {
		attr = (char *)xmlGetProp(n, (xmlChar *)names[i]);
		if(attr != NULL)
		{
		    if(types[i] == G_TYPE_STRING)
			gtk_list_store_set(model, &iter, i, attr, -1);
		    else if(types[i] == G_TYPE_INT)
			gtk_list_store_set(model, &iter, i, atoi(attr), -1);
		    else if(types[i] == G_TYPE_INT64)
			gtk_list_store_set(model, &iter, i, strtoll(attr, NULL, 0), -1);
		    xmlFree(attr);
		}
	    }
	}
	cols = smalloc((numcols - 1) * sizeof(*cols));
	for(i = 1; i < numcols; i++)
	    cols[i - 1] = i;
	sortcol = 0;
	for(i = 0; i < numcols; i++)
	{
	    if(!strcmp(names[i], "Users"))
		sortcol = i;
	}
	setpubhubmodel(GTK_TREE_MODEL(model), sortcol, numcols - 1, cols, names + 1);
	free(cols);
	g_object_unref(model);
	break;
    case PHO_FINI:
	if(ctxt != NULL)
	{
	    if(ctxt->myDoc != NULL)
		xmlFreeDoc(ctxt->myDoc);
	    xmlFreeParserCtxt(ctxt);
	    ctxt = NULL;
	}
	break;
    }
    if(numcols != 0)
    {
	for(i = 0; i < numcols; i++)
	    free(names[i]);
	free(names);
	free(types);
    }
    return(0);
}
