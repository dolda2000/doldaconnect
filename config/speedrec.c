#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/file.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <signal.h>

char buf[65536];
volatile int eof;

void sighandler(int sig)
{
    eof = 1;
}

int main(int argc, char **argv)
{
    int i;
    int ret, fd;
    time_t starttime, endtime;
    long long numbytes;
    size_t datalen;
    FILE *recfile;
    int thisrec, numrecs, numuses, maxrec;
    int recs[5];
    
    if(argc < 2)
    {
	fprintf(stderr, "usage: speedrec recfile\n");
	exit(1);
    }
    numbytes = 0;
    starttime = endtime = 0;
    datalen = 0;
    eof = 0;
    signal(SIGHUP, sighandler);
    signal(SIGINT, sighandler);
    signal(SIGTERM, sighandler);
    while(1)
    {
	ret = read(0, buf, sizeof(buf));
	if((ret < 0) && (errno != EINTR))
	{
	    perror("cannot read");
	    exit(1);
	}
	if((ret == 0) || eof)
	    break;
	datalen = ret;
	numbytes += ret;
	while(datalen > 0)
	{
	    ret = write(1, buf, datalen);
	    if((ret < 0) && (errno != EINTR))
	    {
		perror("cannot write");
		exit(1);
	    }
	    memmove(buf, buf + ret, datalen -= ret);
	}
    }
    if((starttime == 0) || (endtime == 0) || (endtime == starttime))
	exit(0);
    if(numbytes == 0)
	exit(0);
    thisrec = (int)(numbytes / ((long long)(endtime - starttime)));
    if((fd = open(argv[1], O_RDWR | O_CREAT, 0666)) < 0)
    {
	perror(argv[1]);
	exit(1);
    }
    recfile = fdopen(fd, "r+");
    close(0);
    close(1);
    flock(fd, LOCK_EX);
    if(fscanf(recfile, "%i\n", &numuses) < 1)
	numuses = 0;
    if(fscanf(recfile, "%i\n", &maxrec) < 1)
	maxrec = 0;
    if(fscanf(recfile, "%i\n", &numrecs) < 1)
	numrecs = 0;
    for(i = 0; i < numrecs; i++)
	fscanf(recfile, "%i\n", &recs[i]);
    if(numrecs == 5)
    {
	for(i = 0; i < 4; i++)
	    recs[i] = recs[i + 1];
	numrecs = 4;
    }
    recs[numrecs++] = thisrec;
    rewind(recfile);
    ftruncate(fd, 0);
    fprintf(recfile, "%i\n", numuses + 1);
    fprintf(recfile, "%i\n", (thisrec > maxrec)?thisrec:maxrec);
    fprintf(recfile, "%i\n", numrecs);
    for(i = 0; i < numrecs; i++)
	fprintf(recfile, "%i\n", recs[i]);
    flock(fd, LOCK_UN);
    fclose(recfile);
    return(0);
}
