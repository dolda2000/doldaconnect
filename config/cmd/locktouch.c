#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

int main(int argc, char **argv)
{
    int fd;
    
    if(argc < 2)
    {
	fprintf(stderr, "usage: locktouch lockfile\n");
	exit(1);
    }
    if((fd = open(argv[1], O_CREAT | O_EXCL, 0666)) < 0)
    {
	if(errno != EEXIST)
	{
	    perror(argv[1]);
	    exit(2);
	}
	exit(1);
    }
    close(fd);
    return(0);
}
