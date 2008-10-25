#include <stdio.h>
#include <stdlib.h>
#include <sys/poll.h>
#include <time.h>
#include <sys/time.h>

#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

int done;
double btime;

void authcallback(int err, wchar_t *reason, void *data)
{
    printf("auth: %f\n", ntime() - btime);
    printf("Logged in: %i\n", err);
    exit(0);
    dc_queuecmd(NULL, NULL, L"quit", NULL);
}

int main(int argc, char **argv)
{
    struct pollfd pfd;
    int fd;
    struct dc_response *resp;
    
    btime = ntime();
    dc_init();
    printf("init: %f\n", ntime() - btime);
    fd = dc_connect(NULL);
    done = 0;
    while(!done)
    {
	pfd.fd = fd;
	pfd.events = POLLIN;
	if(dc_wantwrite())
	    pfd.events = POLLOUT;
	if(poll(&pfd, 1, -1) < 0)
	{
	    perror("poll");
	    exit(1);
	}
	if((pfd.revents & POLLIN) && dc_handleread())
	    done = 1;
	if((pfd.revents & POLLOUT) && dc_handlewrite())
	    done = 1;
	while((resp = dc_getresp()) != NULL)
	{
	    if(!wcscmp(resp->cmdname, L".connect"))
	    {
		printf("conn: %f\n", ntime() - btime);
		printf("Connected: %i\n", resp->code);
		if(resp->code == 201)
		    dc_loginasync(NULL, 1, NULL, authcallback, NULL);
	    }
	    dc_freeresp(resp);
	}
    }
    printf("fini: %f\n", ntime() - btime);
    dc_cleanup();
    printf("exit: %f\n", ntime() - btime);
    return(0);
}
