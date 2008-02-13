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
#include <wchar.h>
#include <sys/poll.h>
#include <string.h>
#include <errno.h>
#include <locale.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

int verbose = 0;
int dcfd;

struct cmd {
    wchar_t *name;
    int (*handler)(int argc, wchar_t **argv);
};

int cmd_lsnodes(int argc, wchar_t **argv)
{
    int i;
    struct dc_response *resp;
    struct dc_intresp *ires;
    
    resp = dc_gettaggedrespsync(dc_queuecmd(NULL, NULL, L"lsnodes", NULL));
    if(resp->code == 200) {
	printf("   ID  NET  USERS S NAME\n");
	while((ires = dc_interpret(resp)) != NULL) {
	    if(wcslen(ires->argv[2].val.str) > 58) {
		for(i = 55; i < 58; i++)
		    ires->argv[2].val.str[i] = L'.';
		ires->argv[2].val.str[i] = L'\0';
	    }
	    printf("%5i %4ls %6i %c %ls\n", ires->argv[0].val.num, ires->argv[1].val.str, ires->argv[3].val.num, "SHED"[ires->argv[4].val.num], ires->argv[2].val.str);
	    dc_freeires(ires);
	}
    } else if(resp->code == 201) {
    } else {
	fprintf(stderr, "dcsh: %ls\n", resp->rlines[0].argv[0]);
    }
    return(0);
}

struct cmd commands[] = {
    {L"hubs", cmd_lsnodes},
    {NULL, NULL}
};

int run1(int argc, wchar_t **argv)
{
    struct cmd *c;
    
    for(c = commands; c->handler != NULL; c++) {
	if(!wcscmp(argv[0], c->name))
	    return(c->handler(argc, argv));
    }
    fprintf(stderr, "dcsh: no such command\n");
    return(1);
}

int runchar(int argc, char **argv) {
    int i, rv;
    wchar_t **wargv, *buf;
    size_t wargvsize, wargvdata;
    
    wargv = NULL;
    wargvsize = wargvdata = 0;
    for(i = 0; i < argc; i++) {
	if((buf = icmbstowcs(argv[i], NULL)) == NULL) {
	    fprintf(stderr, "dcsh: could not convert %s to wide char: %s\n", argv[i], strerror(errno));
	    for(i = 0; i < wargvdata; i++)
		free(wargv[i]);
	    if(wargv != NULL)
		free(wargv);
	    return(1);
	}
	addtobuf(wargv, buf);
    }
    addtobuf(wargv, NULL);
    rv = run1(wargvdata - 1, wargv);
    dc_freewcsarr(wargv);
    return(rv);
}

int shell(void)
{
    int ret, argc;
    wchar_t *buf, **argv;
    char *p, cmdbuf[128];
    int cmddata;
    struct pollfd pfd[2];

    cmddata = 0;
    while(1) {
	pfd[0].fd = dcfd;
	pfd[0].events = POLLIN;
	if(dc_wantwrite())
	    pfd[0].events |= POLLOUT;
	pfd[1].fd = 0;
	pfd[1].events = POLLIN;
	pfd[1].revents = 0;
	if(poll(pfd, 2, -1) < 0) {
	    if(errno != EINTR) {
		perror("dcsh: poll");
		exit(1);
	    }
	}
	if(((pfd[0].revents & POLLIN) && dc_handleread()) || ((pfd[0].revents & POLLOUT) && dc_handlewrite()))
	    return(1);
	if(pfd[1].revents) {
	    ret = read(0, cmdbuf + cmddata, sizeof(cmdbuf) - cmddata);
	    if(ret < 0) {
		fprintf(stderr, "dcsh: stdin: %s\n", strerror(errno));
		return(1);
	    } else if(ret == 0) {
		return(0);
	    }
	    cmddata += ret;
	    while((p = memchr(cmdbuf, '\n', cmddata)) != NULL) {
		*(p++) = 0;
		if((buf = icmbstowcs(cmdbuf, NULL)) == NULL) {
		    fprintf(stderr, "dcsh: could not convert command to wide chars: %s\n", strerror(errno));
		} else {
		    argv = dc_lexsexpr(buf);
		    free(buf);
		    for(argc = 0; argv[argc] != NULL; argc++);
		    if(argc > 0)
			run1(argc, argv);
		    dc_freewcsarr(argv);
		}
		memmove(cmdbuf, p, cmddata -= (p - cmdbuf));
	    }
	}
    }
}

int main(int argc, char **argv)
{
    int c, rv;
    char *server;
    char *username;
    int authless, authia;
    
    setlocale(LC_ALL, "");
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
	    fprintf(stderr, "usage: dcsh [-AIhv] [-s SERVER] [-u USERNAME] [COMMAND [ARGS...]]\n");
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
    if(optind < argc)
	rv = runchar(argc - optind, argv + optind);
    else
	rv = shell();
    return(rv);
}
