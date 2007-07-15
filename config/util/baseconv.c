#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "utils.h"

int main(int argc, char **argv)
{
    char buf[25];
    int ret, linelen;
    char *decbuf, *encbuf;
    size_t data, buflen;
    char *(*decfn)(char *, size_t *);
    char *(*encfn)(char *, size_t);
    
    if(argc < 3) {
	fprintf(stderr, "usage: baseconv [4568] [4568]\n");
	fprintf(stderr, "\tenter data on stdin\n");
	exit(1);
    }
    data = 0;
    if(!strcmp(argv[1], "4")) {
	decfn = hexdecode;
    } else if(!strcmp(argv[1], "5")) {
	decfn = base32decode;
    } else if(!strcmp(argv[1], "6")) {
	decfn = base64decode;
    } else if(!strcmp(argv[1], "8")) {
	decfn = NULL;
    } else {
	fprintf(stderr, "unknown decoding: %s\n", argv[1]);
	exit(1);
    }
    if(!strcmp(argv[2], "4")) {
	encfn = hexencode;
    } else if(!strcmp(argv[2], "5")) {
	encfn = base32encode;
    } else if(!strcmp(argv[2], "6")) {
	encfn = base64encode;
    } else if(!strcmp(argv[2], "8")) {
	encfn = NULL;
    } else {
	fprintf(stderr, "unknown encoding: %s\n", argv[1]);
	exit(1);
    }
    linelen = 0;
    while((ret = read(0, buf + data, 24 - data)) >= 0) {
	if(((data += ret) == 24) || (ret == 0)) {
	    if(decfn == NULL) {
		decbuf = memcpy(smalloc(data), buf, data);
		buflen = data;
	    } else {
		buf[data] = 0;
		if((decbuf = decfn(buf, &buflen)) == NULL) {
		    fprintf(stderr, "invalid input\n");
		    exit(1);
		}
	    }
	    if(encfn == NULL) {
		encbuf = memcpy(smalloc(buflen), decbuf, buflen);
		fwrite(encbuf, 1, buflen, stdout);
	    } else {
		encbuf = encfn(decbuf, buflen);
		buflen = strlen(encbuf);
		if(linelen + buflen > 60) {
		    fwrite(encbuf, 1, 60 - linelen, stdout);
		    fwrite("\n", 1, 1, stdout);
		    memmove(encbuf, encbuf + 60 - linelen, buflen -= 60 - linelen);
		}
		fwrite(encbuf, 1, buflen, stdout);
		linelen += buflen;
	    }
	    fflush(stdout);
	    free(encbuf);
	    free(decbuf);
	    data = 0;
	}
	if(ret == 0)
	    break;
    }
    if(ret < 0) {
	perror("read");
	exit(1);
    }
    return(0);
}
