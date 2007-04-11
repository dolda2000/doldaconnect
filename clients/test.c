#include <stdio.h>
#include <stdlib.h>
#include <sys/poll.h>

#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>

void authcallback(int err, wchar_t *reason, void *data)
{
    printf("Logged in: %i\n", err);
}

int main(int argc, char **argv)
{
    int i;
    struct pollfd pfd;
    int fd, done;
    struct dc_response *resp;
    struct dc_intresp *ires;
    
    dc_init();
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
	    if(resp->cmdname == NULL)
	    {
		printf("Connected\n");
		dc_loginasync(NULL, 0, NULL, authcallback, NULL);
	    }
	    dc_freeresp(resp);
	}
    }
    dc_cleanup();
    return(0);
}
