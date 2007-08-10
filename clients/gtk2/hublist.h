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

#ifndef _HUBLIST_H
#define _HUBLIST_H

#include <regex.h>

#define PHO_INIT 0
#define PHO_DATA 1
#define PHO_EOF 2
#define PHO_FINI 3

void aborthublist(void);
int validhub(char *field, ...);
void fetchhublist(char *url, regex_t *flt);

int pubhubxmlhandler(int op, char *buf, size_t len);
int pubhuboldhandler(int op, char *buf, size_t len);

#endif
