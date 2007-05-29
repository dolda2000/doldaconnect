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
#include <wchar.h>
#include <sys/poll.h>
#include <errno.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

int verbose = 0;
int dcfd;

int main(int argc, char **argv)
{
    int c;
    char *server;
    char *username;
    int authless, authia;
    struct pollfd pfd[2];
    int done;
    char cmdbuf[128];
    int cmddata;
    
    server = username = NULL;
    authless = authia = 1;
    while((c = getopt(argc, argv, "hvIAs:u:")) != -1) {
	switch(c) {
	case 's':
	    server = optarg;
	    break;
	case 'u':
	    username = optarg;
	    break;
	case 'I':
	    authia = 0;
	    break;
	case 'A':
	    authless = 0;
	    break;
	case 'v':
	    verbose++;
	    break;
	default:
	    fprintf(stderr, "usage: dcsh [-AIhv] [-s SERVER] [-u USERNAME]\n");
	    exit((c == 'h')?0:1);
	}
    }
    dc_init();
    
    if(verbose)
	fprintf(stderr, "connecting...\n");
    if((dcfd = dc_connectsync2(server, 1)) < 0) {
	perror("dcsh: could not connect to server");
	exit(1);
    }
    if(verbose)
	fprintf(stderr, "authenticating...\n");
    if(dc_login(username, authless, authia?dc_convtty:dc_convnone, NULL) != DC_LOGIN_ERR_SUCCESS) {
	fprintf(stderr, "dcsh: authentication unsuccessful\n");
	exit(1);
    }
    if(verbose)
	fprintf(stderr, "done\n");
    done = 0;
    cmddata = 0;
    while(!done) {
	pfd[0].fd = dcfd;
	pfd[0].events = POLLIN;
	if(dc_wantwrite())
	    pfd[0].events |= POLLOUT;
	pfd[1].fd = 0;
	pfd[1].events = POLLIN;
	pfd[1].revents = 0;
	if(poll(pfd, (cmddata < sizeof(cmdbuf)) ? 2 : 1, -1) < 0) {
	    if(errno != EINTR) {
		perror("dcsh: poll");
		exit(1);
	    }
	}
	
    }
    return(0);
}
