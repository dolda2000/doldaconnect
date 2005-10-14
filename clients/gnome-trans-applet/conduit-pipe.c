#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <panel-applet.h>
#include <doldaconnect/utils.h>

#include "conduit.h"

#define SUBPROCCMD "ksu fredrik -q -e /home/fredrik/bin/dctrmon"

struct data
{
    pid_t subproc;
    int fd;
    int gdktag;
    char *inbuf;
    size_t inbufsize, inbufdata;
};

static void pipefdcb(struct conduit *conduit, int fd, GdkInputCondition condition)
{
    struct data *data;
    int ret;
    char *p, *p2, *cmd;
    char **args;
    size_t argssize, argsdata;
    struct transfer *transfer;
    
    data = (struct data *)conduit->cdata;
    if(conduit->state == CNDS_SYN)
	condconnected(conduit);
    sizebuf2(data->inbuf, data->inbufdata + 80, 1);
    ret = read(data->fd, data->inbuf + data->inbufdata, data->inbufsize - data->inbufdata);
    if(ret < 0)
    {
	if((errno == EINTR) || (errno == EAGAIN)) /* Shouldn't happen, but, oh well... */
	    return;
	perror("conduit-pipe: read");
	gdk_input_remove(data->gdktag);
	close(data->fd);
	kill(-data->subproc, SIGHUP);
	data->gdktag = -1;
	data->fd = -1;
	data->subproc = 0;
	conddisconn(conduit);
    }
    if(ret == 0)
    {
	gdk_input_remove(data->gdktag);
	close(data->fd);
	kill(-data->subproc, SIGHUP);
	data->gdktag = -1;
	data->fd = -1;
	data->subproc = 0;
	conddisconn(conduit);
    }
    data->inbufdata += ret;
    while((p = memchr(data->inbuf, '\n', data->inbufdata)) != NULL)
    {
	*p = 0;
	cmd = sstrdup(data->inbuf);
	memmove(data->inbuf, p + 1, data->inbufdata -= (p - data->inbuf) + 1);
	args = NULL;
	argssize = argsdata = 0;
	p = cmd;
	do
	{
	    if((p2 = strchr(p, '\t')) != NULL)
		*(p2++) = 0;
	    addtobuf(args, p);
	    p = p2;
	} while(p2 != NULL);
	if(!strcmp(args[0], "N") && (argsdata >= 4))
	{
	    transfer = newtransfer(conduit, args[1], atoi(args[2]), atoi(args[3]));
	}
	if(!strcmp(args[0], "D") && (argsdata >= 2))
	{
	    if((transfer = findtransferbytag(conduit, args[1])) != NULL)
		freetransfer(transfer);
	}
	if(!strcmp(args[0], "S") && (argsdata >= 3))
	{
	    if((transfer = findtransferbytag(conduit, args[1])) != NULL)
		transfersetsize(transfer, atoi(args[2]));
	}
	if(!strcmp(args[0], "P") && (argsdata >= 3))
	{
	    if((transfer = findtransferbytag(conduit, args[1])) != NULL)
		transfersetpos(transfer, atoi(args[2]));
	}
	free(args);
	free(cmd);
    }
}

static int init(struct conduit *conduit)
{
    static int inited = 0;
    struct data *data;
    
    if(!inited)
    {
	signal(SIGCHLD, SIG_IGN);
	inited = 1;
    }
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    data->fd = -1;
    data->inbuf = NULL;
    data->inbufsize = data->inbufdata = 0;
    data->gdktag = -1;
    conduit->cdata = data;
    return(0);
}

static int connect(struct conduit *conduit)
{
    struct data *data;
    pid_t pid;
    int pfd[2];
    
    data = conduit->cdata;
    if(pipe(pfd))
	return(-1);
    if((pid = fork()) < 0)
    {
	close(pfd[0]);
	close(pfd[1]);
	return(-1);
    }
    if(!pid)
    {
	int devnull;
	
	setpgrp();
	if((devnull = open("/dev/null", O_RDWR)) < 0)
	    exit(127);
	close(pfd[0]);
	dup2(pfd[1], 1);
	close(pfd[1]);
	dup2(devnull, 0);
	close(devnull);
	/* Leave stderr as is */
	execl("/bin/sh", "sh", "-c", SUBPROCCMD, NULL);
	exit(127);
    }
    close(pfd[1]);
    fcntl(pfd[0], F_SETFL, fcntl(pfd[0], F_GETFL) | O_NONBLOCK);
    data->subproc = pid;
    data->fd = pfd[0];
    data->gdktag = gdk_input_add(pfd[0], GDK_INPUT_READ, (void (*)(gpointer, gint, GdkInputCondition))pipefdcb, conduit);
    data->inbufdata = 0;
    return(0);
}

static void destroy(struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(data == NULL)
	return;
    if(data->gdktag >= 0)
	gdk_input_remove(data->gdktag);
    if(data->subproc > 0)
	kill(-data->subproc, SIGHUP);
    if(data->fd >= 0)
	close(data->fd);
    if(data->inbuf != NULL)
	free(data->inbuf);
    free(data);
    conduit->cdata = NULL;
}

static struct conduitiface st_conduit_pipe =
{
    .init = init,
    .connect = connect,
    .destroy = destroy,
};

struct conduitiface *conduit_pipe = &st_conduit_pipe;
