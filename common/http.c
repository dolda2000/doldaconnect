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
#include <sys/socket.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <utils.h>
#include <http.h>

struct hturlinfo *parseurl(char *url)
{
    char *p, *p2, *p3;
    struct hturlinfo *ui;
    
    if(strncmp(url, "http://", 7))
	return(NULL);
    ui = memset(smalloc(sizeof(*ui)), 0, sizeof(*ui));
    p = url + 7;
    if((p2 = strchr(p, '/')) != NULL)
	*(p2++) = 0;
    if((p3 = strrchr(p, ':')) != NULL) {
	*(p3++) = 0;
	ui->port = atoi(p3);
    }
    ui->host = sstrdup(p);
    if(p2 == NULL) {
        ui->path = sstrdup("/");
    } else {
	p = p2;
	if((p2 = strchr(p, '?')) != NULL)
	    *(p2++) = 0;
	ui->path = sstrdup(p);
    }
    if(p2 == NULL) {
	ui->query = sstrdup("");
    } else {
	ui->query = sstrdup(p2);
    }
    return(ui);
}
