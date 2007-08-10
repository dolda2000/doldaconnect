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

#ifndef _DOLCON_H
#define _DOLCON_H

#include <libintl.h>

#define _(text) gettext(text)
#define DCCHARSET "windows-1252"

int msgbox(int type, int buttons, char *format, ...)
#if defined(__GNUC__)
    __attribute__ ((format (printf, 3, 4)))
#endif
;

void setpubhubmodel(GtkTreeModel *model, int sortcol, int numcols, int *cols, char **names);

#endif
