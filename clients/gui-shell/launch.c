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
#include <pwd.h>
#include <libintl.h>
#include <signal.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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

int main(int argc, char **argv)
{
    char cf[1024], pf[1024];
    
    if(getenv("HOME") != NULL)
	snprintf(cf, sizeof(cf), "%s/.doldacond.conf", getenv("HOME"));
    else
	snprintf(cf, sizeof(cf), "%s/.doldacond.conf", getpwuid(getuid())->pw_dir);
    if(getenv("HOME") != NULL)
	snprintf(pf, sizeof(pf), "%s/.doldacond.pid", getenv("HOME"));
    else
	snprintf(pf, sizeof(pf), "%s/.doldacond.pid", getpwuid(getuid())->pw_dir);
    if(access(cf, F_OK)) {
	execlp("dolconf", "dolconf", "-a", NULL);
	perror("dolconf");
    } else if(access(pf, F_OK) || !running(pf)) {
	execlp("doldacond-shell", "doldacond-shell", NULL);
	perror("doldacond-shell");
    } else {
	execlp("dolcon", "dolcon", NULL);
	perror("dolcon");
    }
    return(127);
}
