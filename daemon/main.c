/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2004 Fredrik Tolf (fredrik@dolda2000.com)
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
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <locale.h>
#include <signal.h>
#include <getopt.h>
#include <time.h>
#include <pwd.h>
#include <grp.h>
#include <sys/wait.h>
#include <stdarg.h>
#include <fcntl.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "utils.h"
#include "log.h"
#include "conf.h"
#include "module.h"
#include "net.h"
#include "client.h"
#include "sysevents.h"
#include "auth.h"

struct module *modchain = NULL;
static struct timer *timers = NULL;
static struct child *children = NULL;
volatile int running;
volatile int reinit;
static volatile int childrendone = 0;

struct timer *timercallback(double at, void (*func)(int, void *), void *data)
{
    struct timer *new;
    
    new = smalloc(sizeof(*new));
    new->at = at;
    new->func = func;
    new->data = data;
    new->next = timers;
    new->prev = NULL;
    if(timers != NULL)
	timers->prev = new;
    timers = new;
    return(new);
}

void canceltimer(struct timer *timer)
{
    if(timer->next != NULL)
	timer->next->prev = timer->prev;
    if(timer->prev != NULL)
	timer->prev->next = timer->next;
    if(timer == timers)
	timers = timer->next;
    timer->func(1, timer->data);
    free(timer);
}

void childcallback(pid_t pid, void (*func)(pid_t, int, void *), void *data)
{
    struct child *new;
    
    new = smalloc(sizeof(*new));
    new->pid = pid;
    new->callback = func;
    new->data = data;
    new->finished = 0;
    new->prev = NULL;
    new->next = children;
    if(children != NULL)
	children->prev = new;
    children = new;
}

static void preinit(int hup)
{
    struct module *mod;
    
    for(mod = modchain; mod != NULL; mod = mod->next)
    {
	if(mod->preinit)
	    mod->preinit(hup);
	if(!hup && ((mod->conf.vars != NULL) || (mod->conf.cmds != NULL)))
	    confregmod(&mod->conf);
    }
}

static void init(int hup)
{
    struct module *mod;
    
    for(mod = modchain; mod != NULL; mod = mod->next)
    {
	if(mod->init && mod->init(hup))
	{
	    flog(LOG_CRIT, "initialization of \"%s\" failed", mod->name);
	    exit(1);
	}
    }
}

static void terminate(void)
{
    struct module *mod;
    
    for(mod = modchain; mod != NULL; mod = mod->next)
    {
	if(mod->terminate)
	    mod->terminate();
    }
}

static void handler(int signum)
{
    pid_t pid;
    int status;
    struct child *child;
    FILE *dumpfile;
    extern int numfnetnodes, numtransfers, numdcpeers;
    
    switch(signum)
    {
    case SIGHUP:
	reinit = 1;
	break;
    case SIGINT:
    case SIGTERM:
	running = 0;
	break;
    case SIGCHLD:
	while((pid = waitpid(-1, &status, WNOHANG)) > 0)
	{
	    for(child = children; child != NULL; child = child->next)
	    {
		if(child->pid == pid)
		{
		    child->finished = 1;
		    child->status = status;
		}
	    }
	    childrendone = 1;
	}
	break;
    case SIGUSR1:
	flog(LOG_NOTICE, "forking and dumping core upon SIGUSR1");
	if(fork() == 0)
	    abort();
	break;
    case SIGUSR2:
	flog(LOG_NOTICE, "dumping memstats to /tmp/dc-mem upon SIGUSR2");
	if((dumpfile = fopen("/tmp/dc-mem", "w")) == NULL) {
	    flog(LOG_ERR, "could not dump stats: %s", strerror(errno));
	    break;
	}
	fprintf(dumpfile, "%i %i %i\n", numfnetnodes, numtransfers, numdcpeers);
	fclose(dumpfile);
	break;
    }
}

pid_t forksess(uid_t user, struct authhandle *auth, void (*ccbfunc)(pid_t, int, void *), void *data, ...)
{
    int i, o;
    int cpipe[2];
    struct
    {
	int tfd;
	int fd;
    } *files;
    int maxfd, type, numfiles;
    int acc;
    int *ibuf;
    struct passwd *pwent;
    pid_t pid;
    char *buf;
    va_list args;
    sigset_t sigset;
    int ret, status;
    
    if((pwent = getpwuid(user)) == NULL)
    {
	flog(LOG_WARNING, "no passwd entry for uid %i, cannot fork session", user);
	errno = EACCES;
	return(-1);
    }
    if((geteuid() != 0) && (user != geteuid()))
    {
	flog(LOG_WARNING, "cannot fork non-owning session when not running as root (EUID is %i, target UID is %i)", geteuid(), user);
	errno = EPERM;
	return(-1);
    }
    va_start(args, data);
    numfiles = 0;
    files = NULL;
    maxfd = 0;
    while((type = va_arg(args, int)) != FD_END)
    {
	files = srealloc(files, sizeof(*files) * (numfiles + 1));
	files[numfiles].fd = va_arg(args, int);
	if(files[numfiles].fd > maxfd)
	    maxfd = files[numfiles].fd;
	acc = va_arg(args, int);
	if(type == FD_PIPE)
	{
	    if(pipe(cpipe) < 0)
	    {
		flog(LOG_CRIT, "could not create pipe(!): %s", strerror(errno));
		for(i = 0; i < numfiles; i++)
		    close(files[i].tfd);
		return(-1);
	    }
	    ibuf = va_arg(args, int *);
	    if(acc == O_WRONLY)
	    {
		*ibuf = cpipe[1];
		files[numfiles].tfd = cpipe[0];
	    } else {
		*ibuf = cpipe[0];
		files[numfiles].tfd = cpipe[1];
	    }
	} else if(type == FD_FILE) {
	    buf = va_arg(args, char *);
	    if((files[numfiles].tfd = open(buf, acc)) < 0)
	    {
		flog(LOG_CRIT, "could not open file \"%s\": %s", buf, strerror(errno));
		for(i = 0; i < numfiles; i++)
		    close(files[i].tfd);
		return(-1);
	    }
	}
	if(files[numfiles].tfd > maxfd)
	    maxfd = files[numfiles].tfd;
	numfiles++;
    }
    va_end(args);
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGCHLD);
    sigprocmask(SIG_BLOCK, &sigset, NULL);
    if((pid = fork()) < 0)
    {
	flog(LOG_WARNING, "could not fork(!) in forksess(): %s", strerror(errno));
	for(i = 0; i < numfiles; i++)
	    close(files[i].tfd);
	sigprocmask(SIG_UNBLOCK, &sigset, NULL);
    }
    if(pid == 0)
    {
	sigprocmask(SIG_UNBLOCK, &sigset, NULL);
	signal(SIGPIPE, SIG_DFL);
	signal(SIGCHLD, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGTERM, SIG_DFL);
	signal(SIGHUP, SIG_DFL);
	for(i = 0; i < numfiles; i++)
	{
	    if(dup2(files[i].tfd, maxfd + i + 1) < 0)
		exit(127);
	    files[i].tfd = maxfd + i + 1;
	}
	for(i = 0; i < numfiles; i++)
	{
	    if(dup2(files[i].tfd, files[i].fd) < 0)
		exit(127);
	}
	initlog();
	for(i = 0; i < FD_SETSIZE; i++)
	{
	    if(i <= maxfd)
	    {
		for(o = 0; o < numfiles; o++)
		{
		    if(i == files[o].fd)
			break;
		}
		if(o == numfiles)
		    close(i);
	    } else {
		close(i);
	    }
	}
	setpgrp();
	signal(SIGHUP, SIG_IGN);
	errno = 0;
	if((authopensess(auth)) != AUTH_SUCCESS)
	{
	    flog(LOG_WARNING, "could not open session for user %s: %s", pwent->pw_name, (errno == 0)?"Unknown error - should be logged above":strerror(errno));
	    exit(127);
	}
	if((pid = fork()) < 0)
	{
	    authclosesess(auth);
	    exit(127);
	}
	if(pid == 0)
	{
	    if(geteuid() == 0)
	    {
		if(initgroups(pwent->pw_name, pwent->pw_gid))
		{
		    flog(LOG_WARNING, "could not initgroups: %s", strerror(errno));
		    exit(127);
		}
		if(setgid(pwent->pw_gid))
		{
		    flog(LOG_WARNING, "could not setgid: %s", strerror(errno));
		    exit(127);
		}
		if(setuid(pwent->pw_uid))
		{
		    flog(LOG_WARNING, "could not setuid: %s", strerror(errno));
		    exit(127);
		}
	    }
	    putenv(sprintf2("HOME=%s", pwent->pw_dir));
	    putenv(sprintf2("SHELL=%s", pwent->pw_shell));
	    putenv(sprintf2("USER=%s", pwent->pw_name));
	    putenv(sprintf2("LOGNAME=%s", pwent->pw_name));
	    putenv(sprintf2("PATH=%s/bin:/usr/local/bin:/bin:/usr/bin", pwent->pw_dir));
	    chdir(pwent->pw_dir);
	    return(0);
	}
	for(i = 0; i < numfiles; i++)
	    close(files[i].fd);
	while(((ret = waitpid(pid, &status, 0)) != pid) && (ret >= 0));
	authclosesess(auth);
	if(ret < 0)
	{
	    flog(LOG_WARNING, "waitpid(%i) said \"%s\"", pid, strerror(errno));
	    exit(127);
	}
	if(!WIFEXITED(status))
	    exit(127);
	exit(WEXITSTATUS(status));
    }
    for(i = 0; i < numfiles; i++)
	close(files[i].tfd);
    if(files != NULL)
	free(files);
    if(ccbfunc != NULL)
	childcallback(pid, ccbfunc, data);
    sigprocmask(SIG_UNBLOCK, &sigset, NULL);
    return(pid);
}

int main(int argc, char **argv)
{
    int c;
    int nofork;
    char *configfile;
    char *pidfile;
    FILE *pfstream, *confstream;
    int delay;
    struct module *mod;
    struct timer *timer, *ntimer;
    struct child *child;
    double now;
    
    nofork = 0;
    syslogfac = LOG_DAEMON;
    configfile = NULL;
    pidfile = NULL;
    while((c = getopt(argc, argv, "p:C:f:hns")) != -1)
    {
	switch(c)
	{
	case 'p':
	    pidfile = optarg;
	    break;
	case 'C':
	    configfile = optarg;
	    break;
	case 'f':
	    if(!strcmp(optarg, "auth"))
		syslogfac = LOG_AUTH;
	    else if(!strcmp(optarg, "authpriv"))
		syslogfac = LOG_AUTHPRIV;
	    else if(!strcmp(optarg, "cron"))
		syslogfac = LOG_CRON;
	    else if(!strcmp(optarg, "daemon"))
		syslogfac = LOG_DAEMON;
	    else if(!strcmp(optarg, "ftp"))
		syslogfac = LOG_FTP;
	    else if(!strcmp(optarg, "kern"))
		syslogfac = LOG_KERN;
	    else if(!strcmp(optarg, "lpr"))
		syslogfac = LOG_LPR;
	    else if(!strcmp(optarg, "mail"))
		syslogfac = LOG_MAIL;
	    else if(!strcmp(optarg, "news"))
		syslogfac = LOG_NEWS;
	    else if(!strcmp(optarg, "syslog"))
		syslogfac = LOG_SYSLOG;
	    else if(!strcmp(optarg, "user"))
		syslogfac = LOG_USER;
	    else if(!strcmp(optarg, "uucp"))
		syslogfac = LOG_UUCP;
	    else if(!strncmp(optarg, "local", 5) && (strlen(optarg) == 6))
		syslogfac = LOG_LOCAL0 + (optarg[5] - '0');
	    else
		fprintf(stderr, "unknown syslog facility %s, using daemon\n", optarg);
	    break;
	case 'n':
	    nofork = 1;
	    break;
	case 's':
	    logtosyslog = 1;
	    logtostderr = 0;
	case 'h':
	case ':':
	case '?':
	default:
	    printf("usage: doldacond [-hns] [-C configfile] [-p pidfile] [-f facility]\n");
	    exit(c != 'h');
	}
    }
    setlocale(LC_ALL, "");
    initlog();
    signal(SIGPIPE, SIG_IGN);
    signal(SIGHUP, handler);
    signal(SIGINT, handler);
    signal(SIGTERM, handler);
    signal(SIGCHLD, handler);
    signal(SIGUSR1, handler);
    signal(SIGUSR2, handler);
    preinit(0);
    if(configfile == NULL)
    {
	if((configfile = findconfigfile()) == NULL)
	{
	    flog(LOG_CRIT, "could not find a configuration file");
	    exit(1);
	}
    }
    pfstream = NULL;
    if(pidfile != NULL)
    {
	if((pfstream = fopen(pidfile, "w")) == NULL)
	{
	    flog(LOG_CRIT, "could not open specified PID file %s: %s", pidfile, strerror(errno));
	    exit(1);
	}
    }
    if((confstream = fopen(configfile, "r")) == NULL)
    {
	flog(LOG_CRIT, "could not open configuration file %s: %s", configfile, strerror(errno));
	exit(1);
    }
    readconfig(confstream);
    fclose(confstream);
    init(0);
    if(!nofork)
    {
	logtosyslog = 1;
	daemon(0, 0);
	flog(LOG_INFO, "daemonized");
	logtostderr = 0;
    }
    if(pfstream != NULL) {
	fprintf(pfstream, "%i\n", getpid());
	fclose(pfstream);
    }
    running = 1;
    reinit = 0;
    while(running)
    {
	if(reinit)
	{
	    if((confstream = fopen(configfile, "r")) == NULL)
	    {
		flog(LOG_ERR, "could not open configuration file %s: %s (ignoring HUP)", configfile, strerror(errno));
	    } else {
		preinit(1);
		readconfig(confstream);
		fclose(confstream);
		init(1);
	    }
	    reinit = 0;
	}
	delay = 1000; /* -1; */
	for(mod = modchain; mod != NULL; mod = mod->next)
	{
	    if(mod->run && mod->run())
		delay = 0;
	}
	if(!running)
	    delay = 0;
	if(delay != 0)
	{
	    now = ntime();
	    for(timer = timers; timer != NULL; timer = timer->next)
	    {
		if((delay == -1) || ((int)((timer->at - now) * 1000.0) < delay))
		    delay = (int)((timer->at - now) * 1000.0);
	    }
	}
	if(childrendone)
	{
	    delay = 0;
	    childrendone = 0;
	}
	pollsocks(delay);
	now = ntime();
	for(timer = timers; timer != NULL; timer = ntimer)
	{
	    ntimer = timer->next;
	    if(now < timer->at)
		continue;
	    if(timer->prev != NULL)
		timer->prev->next = timer->next;
	    if(timer->next != NULL)
		timer->next->prev = timer->prev;
	    if(timer == timers)
		timers = timer->next;
	    timer->func(0, timer->data);
	    free(timer);
	}
	do
	{
	    for(child = children; child != NULL; child = child->next)
	    {
		if(child->finished)
		{
		    child->callback(child->pid, child->status, child->data);
		    if(child == children)
			children = child->next;
		    if(child->prev != NULL)
			child->prev->next = child->next;
		    if(child->next != NULL)
			child->next->prev = child->prev;
		    free(child);
		    break;
		}
	    }
	} while(child != NULL);
    }
    flog(LOG_INFO, "terminating...");
    terminate();
    return(0);
}
