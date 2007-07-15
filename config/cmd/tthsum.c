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

/* This frankenfile is built from bits and pieces of the daemon. The
 * copying and pasting is very ugly, but it doesn't *really*
 * matter. */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <tiger.h>
#include <utils.h>

char buf[32768];

static char *base64enc2(char *data, size_t datalen)
{
    static char *res = NULL;
    
    if(res != NULL)
	free(res);
    res = base64encode(data, datalen);
    return(res);
}

static char *base64dec2(char *data, size_t *datalen)
{
    static char *res = NULL;
    
    if(res != NULL)
	free(res);
    res = base64decode(data, datalen);
    return(res);
}

int main(int argc, char **argv)
{
    int i, ret;
    size_t size;
    int c, fd, outfd;
    int len, len2;
    int filter, output;
    struct tigertreehash tth;
    char *dec, *enc;
    char res[24];
    char *statefile;
    FILE *state;
    int progress, bytes;
    struct stat sb;
    
    filter = 0;
    output = 4;
    outfd = 3;
    progress = 0;
    statefile = NULL;
    while((c = getopt(argc, argv, "phf456F:s:")) != -1) {
	switch(c) {
	case '4':
	case '5':
	case '6':
	    output = c - '0';
	    break;
	case 'p':
	    progress = 1;
	    break;
	case 'f':
	    filter = 1;
	    break;
	case 'F':
	    outfd = atoi(optarg);
	    break;
	case 's':
	    statefile = optarg;
	    break;
	case 'h':
	case ':':
	case '?':
	default:
	    fprintf(stderr, "usage: tigersum [-h456] FILE...\n");
	    fprintf(stderr, "       tigersum [-h456] [-F OUTFD] [-s STATEFILE] -f\n");
	    exit((c == 'h')?0:1);
	}
    }
    if(filter) {
	inittigertree(&tth);
	if(statefile != NULL)
	{
	    if(((state = fopen(statefile, "r")) == NULL) && (errno != ENOENT)) {
		fprintf(stderr, "tigersum: %s: %s\n", statefile, strerror(errno));
		exit(1);
	    }
	    if(state != NULL) {
		if(fgets(buf, sizeof(buf), state) == NULL) {
		    fprintf(stderr, "tigersum: %s: could not read entire state\n", statefile);
		    exit(1);
		}
		tth.blocks = atoi(buf);
		if(fgets(buf, sizeof(buf), state) == NULL) {
		    fprintf(stderr, "tigersum: %s: could not read entire state\n", statefile);
		    exit(1);
		}
		tth.depth = atoi(buf);
		for(i = 0; i < tth.depth; i++) {
		    if(fgets(buf, sizeof(buf), state) == NULL) {
			fprintf(stderr, "tigersum: %s: could not read entire state\n", statefile);
			exit(1);
		    }
		    dec = base64dec2(buf, &size);
		    if(size != 24) {
			fprintf(stderr, "tigersum: %s: illegal state\n", statefile);
			exit(1);
		    }
		    memcpy(tth.stack[i], dec, 24);
		}
		if(fgets(buf, sizeof(buf), state) == NULL) {
		    fprintf(stderr, "tigersum: %s: could not read entire state\n", statefile);
		    exit(1);
		}
		tth.offset = atoi(buf);
		if(fgets(buf, sizeof(buf), state) == NULL) {
		    fprintf(stderr, "tigersum: %s: could not read entire state\n", statefile);
		    exit(1);
		}
		dec = base64dec2(buf, &size);
		if(size != tth.offset) {
		    fprintf(stderr, "tigersum: %s: illegal state\n", statefile);
		    exit(1);
		}
		memcpy(&tth.block, dec, tth.offset);
		fclose(state);
		unlink(statefile);
	    }
	}
	while(1) {
	    ret = read(0, buf, sizeof(buf));
	    if(ret < 0) {
		perror("tigersum: read");
		exit(1);
	    }
	    if(ret == 0)
		break;
	    len = ret;
	    for(len2 = 0; len2 < len; len2 += ret) {
		if((ret = write(1, buf, ret)) < 0) {
		    perror("tigersum: write");
		    exit(1);
		}
	    }
	    dotigertree(&tth, buf, len);
	}
	if(statefile != NULL) {
	    if((state = fopen(statefile, "w")) == NULL) {
		fprintf(stderr, "tigersum: %s: %s\n", statefile, strerror(errno));
		exit(1);
	    }
	    fprintf(state, "%i\n", tth.blocks);
	    fprintf(state, "%i\n", tth.depth);
	    for(i = 0; i < tth.depth; i++) {
		enc = base64enc2(tth.stack[i], 24);
		fprintf(state, "%s\n", enc);
	    }
	    fprintf(state, "%i\n", tth.offset);
	    enc = base64enc2(tth.block, tth.offset);
	    fputs(enc, state);
	    fputc('\n', state);
	    fclose(state);
	}
	synctigertree(&tth);
	restigertree(&tth, res);
	if(output == 4)
	    enc = hexencode(res, 24);
	else if(output == 5)
	    enc = base32encode(res, 24);
	else if(output == 6)
	    enc = base64encode(res, 24);
	for(len = 0; len < strlen(enc); len += ret) {
	    if((ret = write(outfd, enc + len, strlen(enc) - len)) < 0) {
		perror("tigersum: output");
		exit(1);
	    }
	}
	free(enc);
	write(outfd, "\n", 1);
    } else {
	for(i = optind; i < argc; i++) {
	    if(!strcmp(argv[i], "-")) {
		fd = 0;
	    } else {
		if((fd = open(argv[i], O_RDONLY)) < 0) {
		    fprintf(stderr, "tigersum: %s: %s\n", argv[i], strerror(errno));
		    exit(1);
		}
	    }
	    if(progress) {
		fstat(fd, &sb);
		if(!S_ISREG(sb.st_mode))
		    sb.st_size = -1;
		bytes = 0;
	    }
	    inittigertree(&tth);
	    while(1) {
		ret = read(fd, buf, sizeof(buf));
		if(ret < 0) {
		    perror("tigersum: read");
		    exit(1);
		}
		if(progress) {
		    if((bytes == 0) || ((bytes & ~0xFFFFF) != ((bytes + ret) & ~0xFFFFF))) {
			bytes += ret;
			fprintf(stderr, "\033[1G");
			if(argc - optind > 1)
			    fprintf(stderr, "%s: ", argv[i]);
			if(sb.st_size < 0) {
			    fprintf(stderr, "%i", bytes);
			} else {
			    fprintf(stderr, "%i%%", (int)(((float)bytes / (float)sb.st_size) * 100.0));
			}
			fprintf(stderr, "\033[K");
			fflush(stderr);
		    } else {
			bytes += ret;
		    }
		}
		if(ret == 0)
		    break;
		dotigertree(&tth, buf, ret);
	    }
	    if(progress)
		fprintf(stderr, "\n");
	    synctigertree(&tth);
	    restigertree(&tth, res);
	    if(output == 4)
		enc = hexencode(res, 24);
	    else if(output == 5)
		enc = base32encode(res, 24);
	    else if(output == 6)
		enc = base64encode(res, 24);
	    if(argc - optind > 1)
		printf("%s %s\n", enc, argv[i]);
	    else
		printf("%s\n", enc);
	    free(enc);
	    fflush(stdout);
	    close(fd);
	}
    }
    return(0);
}
