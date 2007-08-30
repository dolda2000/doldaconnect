/*
 *  Dolda Connect - Modular multiuser Direct Connect-style client
 *  Copyright (C) 2004 Fredrik Tolf <fredrik@dolda2000.com>
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
#include <wchar.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "client.h"
#include "conf.h"
#include "log.h"
#include "utils.h"
#include "module.h"
#include "net.h"
#include "sysevents.h"
#include <tiger.h>

struct scanstate
{
    struct scanstate *next;
    struct sharecache *node;
    DIR *dd;
};

struct scanqueue
{
    struct scanqueue *next;
    struct scanstate *state;
};

static int conf_share(int argc, wchar_t **argv);
static void freecache(struct sharecache *node);
static void checkhashes(void);
static void writehashcache(int now);

static struct configvar myvars[] =
{
    /** The default nick name to use. The nickname can also be
     * specified for individual hubs, overriding this setting. */
    {CONF_VAR_STRING, "defnick", {.str = L"DoldaConnect user"}},
    /** When scanning shares, this bitmask is consulted for every
     * regular file. Unless the file's mode has the bits specified by
     * this mask set, it will not be shared. */
    {CONF_VAR_INT, "scanfilemask", {.num = 0004}},
    /** When scanning shares, this bitmask is consulted for every
     * directory encountered. Unless the directory's mode has the bits
     * specified by this mask set, it will be ignored and any files
     * under it will not be shared. */
    {CONF_VAR_INT, "scandirmask", {.num = 0005}},
    /** The filename to use for the hash cache (see the FILES section
     * for more information). */
    {CONF_VAR_STRING, "hashcache", {.str = L"dc-hashcache"}},
    /** Writes of the hash cache and file lists are delayed for an
     * amount of time, in order to minimize the time spent on I/O wait
     * while hashing many small files. This variable sets the amount
     * of time, in seconds. */
    {CONF_VAR_INT, "hashwritedelay", {.num = 300}},
    /** The amount of time, in seconds, to wait before automatically
     * rescanning the shared directories for changes. Set to zero (the
     * default) to disable automatic rescanning. (Broken shares are
     * always rescanned upon detection, regardless of this
     * setting.) */
    {CONF_VAR_INT, "rescandelay", {.num = 0}},
    {CONF_VAR_END}
};

static struct configcmd mycmds[] = 
{
    {"share", conf_share},
    {NULL}
};

static struct scanstate *scanjob = NULL;
static struct scanqueue *scanqueue = NULL;
static struct sharepoint *shares = NULL;
static struct hashcache *hashcache = NULL;
static struct timer *hashwritetimer = NULL;
/* Set initially to -1, but changed to 0 the first time run() is
 * called. This is to avoid forking a hash job before daemonizing,
 * since that would make the daemon unable to wait() for the hash
 * job. */
static pid_t hashjob = -1;
struct sharecache *shareroot = NULL;
static struct timer *scantimer = NULL;
unsigned long long sharesize = 0;
GCBCHAIN(sharechangecb, unsigned long long);

static int conf_share(int argc, wchar_t **argv)
{
    struct sharepoint *share;
    char *b;
    
    if(argc < 3)
    {
	flog(LOG_WARNING, "not enough arguments given for share command");
	return(1);
    }
    if((b = icwcstombs(argv[2], NULL)) == NULL)
    {
	flog(LOG_WARNING, "could not convert wcs path (%ls) to current locale's charset: %s", argv[2], strerror(errno));
	return(1);
    }
    for(share = shares; share != NULL; share = share->next)
    {
	if(!strcmp(share->path, b) && !wcscmp(share->name, argv[1]))
	{
	    share->delete = 0;
	    free(b);
	    return(0);
	}
    }
    share = smalloc(sizeof(*share));
    share->path = b;
    share->delete = 0;
    share->name = swcsdup(argv[1]);
    share->next = shares;
    share->prev = NULL;
    if(shares != NULL)
	shares->prev = share;
    shares = share;
    return(0);
}

static void dumpsharecache(struct sharecache *node, int l)
{
    int i;
    
    for(; node != NULL; node = node->next)
    {
	for(i = 0; i < l; i++)
	    putc('\t', stdout);
	printf("%ls\n", node->name);
	if(node->f.b.type == FILE_DIR)
	    dumpsharecache(node->child, l + 1);
    }
}

struct hash *newhash(wchar_t *algo, size_t len, char *buf)
{
    struct hash *ret;
    
    ret = smalloc(sizeof(*ret));
    memset(ret, 0, sizeof(*ret));
    ret->algo = swcsdup(algo);
    ret->len = len;
    ret->buf = memcpy(smalloc(len), buf, len);
    return(ret);
}

void freehash(struct hash *hash)
{
    free(hash->algo);
    free(hash->buf);
    free(hash);
}

struct hash *duphash(struct hash *hash)
{
    return(newhash(hash->algo, hash->len, hash->buf));
}

struct hash *parsehash(wchar_t *text)
{
    wchar_t *p;
    char *mbsbuf, *decbuf;
    size_t buflen;
    struct hash *ret;
    
    if((p = wcschr(text, L':')) == NULL)
	return(NULL);
    *(p++) = L'\0';
    if((mbsbuf = icwcstombs(p, "US-ASCII")) == NULL)
	return(NULL);
    decbuf = base64decode(mbsbuf, &buflen);
    free(mbsbuf);
    if(decbuf == NULL)
	return(NULL);
    ret = newhash(text, buflen, decbuf);
    free(decbuf);
    return(ret);
}

wchar_t *unparsehash(struct hash *hash)
{
    static wchar_t *buf = NULL;
    wchar_t *whbuf;
    char *hbuf;
    size_t bufsize, bufdata;
    
    if(buf != NULL)
	free(buf);
    buf = NULL;
    bufsize = bufdata = 0;
    hbuf = base64encode(hash->buf, hash->len);
    if((whbuf = icmbstowcs(hbuf, "US-ASCII")) == NULL)
    {
	flog(LOG_CRIT, "bug! could not convert base64 from us-ascii: %s", strerror(errno));
	abort();
    }
    free(hbuf);
    bufcat(buf, hash->algo, wcslen(hash->algo));
    addtobuf(buf, ':');
    bufcat(buf, whbuf, wcslen(whbuf));
    free(whbuf);
    addtobuf(buf, 0);
    return(buf);
}

int hashcmp(struct hash *h1, struct hash *h2)
{
    if(wcscmp(h1->algo, h2->algo))
	return(0);
    if(h1->len != h2->len)
	return(0);
    if(memcmp(h1->buf, h2->buf, h1->len))
	return(0);
    return(1);
}

static struct hashcache *newhashcache(void)
{
    struct hashcache *new;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    new->next = hashcache;
    new->prev = NULL;
    if(hashcache != NULL)
	hashcache->prev = new;
    hashcache = new;
    return(new);
}

static void freehashcache(struct hashcache *hc)
{
    if(hc->next != NULL)
	hc->next->prev = hc->prev;
    if(hc->prev != NULL)
	hc->prev->next = hc->next;
    if(hc == hashcache)
	hashcache = hc->next;
    free(hc);
}

static struct hashcache *findhashcache(dev_t dev, ino_t inode)
{
    struct hashcache *hc;
    
    for(hc = hashcache; hc != NULL; hc = hc->next)
    {
	if((hc->dev == dev) && (hc->inode == inode))
	    return(hc);
    }
    return(NULL);
}

static void readhashcache(void)
{
    int i, wc, line;
    char *hcname;
    FILE *stream;
    char linebuf[256];
    char *p, *p2, *wv[32], *hash;
    struct hashcache *hc;
    size_t len;
    
    if((hcname = findfile(icswcstombs(confgetstr("cli", "hashcache"), NULL, NULL), NULL, 0)) == NULL)
	return;
    if((stream = fopen(hcname, "r")) == NULL)
    {
	flog(LOG_WARNING, "could not open hash cache %s: %s", hcname, strerror(errno));
	return;
    }
    while(hashcache != NULL)
	freehashcache(hashcache);
    line = 0;
    while(!feof(stream))
    {
	fgets(linebuf, sizeof(linebuf), stream);
	line++;
	for(p = linebuf; *p; p++)
	{
	    if(*p == '\n')
		*p = ' ';
	}
	if(linebuf[0] == '#')
	    continue;
	for(wc = 0, p = linebuf; (wc < 32) && ((p2 = strchr(p, ' ')) != NULL); p = p2 + 1)
	{
	    if(p2 == p)
		continue;
	    *p2 = 0;
	    wv[wc++] = p;
	}
	if(wc < 3)
	    continue;
	hc = newhashcache();
	hc->dev = strtoll(wv[0], NULL, 10);
	hc->inode = strtoll(wv[1], NULL, 10);
	hc->mtime = strtoll(wv[2], NULL, 10);
	for(i = 3; i < wc; i++)
	{
	    if(!strcmp(wv[i], "tth"))
	    {
		if(++i >= wc)
		    continue;
		hash = base64decode(wv[i], &len);
		if(len != 24)
		{
		    free(hash);
		    continue;
		}
		memcpy(hc->tth, hash, 24);
		free(hash);
	    }
	}
    }
    fclose(stream);
}

static void hashtimercb(int cancelled, void *uudata)
{
    hashwritetimer = NULL;
    if(!cancelled)
	writehashcache(1);
}

static void writehashcache(int now)
{
    char *buf;
    char *hcname;
    FILE *stream;
    struct hashcache *hc;
    
    if(!now)
    {
	if(hashwritetimer == NULL)
	    hashwritetimer = timercallback(ntime() + confgetint("cli", "hashwritedelay"), (void (*)(int, void *))hashtimercb, NULL);
	return;
    }
    if(hashwritetimer != NULL)
	canceltimer(hashwritetimer);
    hcname = findfile(icswcstombs(confgetstr("cli", "hashcache"), NULL, NULL), NULL, 1);
    if((stream = fopen(hcname, "w")) == NULL)
    {
	flog(LOG_WARNING, "could not write hash cache %s: %s", hcname, strerror(errno));
	return;
    }
    fprintf(stream, "# Dolda Connect hash cache file\n");
    fprintf(stream, "# Generated automatically, do not edit\n");
    fprintf(stream, "# Format: DEVICE INODE MTIME [HASH...]\n");
    fprintf(stream, "# HASH := HASHTYPE HASHVAL\n");
    fprintf(stream, "# HASHTYPE can currently only be `tth'\n");
    for(hc = hashcache; hc != NULL; hc = hc->next)
    {
	buf = base64encode(hc->tth, 24);
	fprintf(stream, "%lli %lli %li tth %s\n", (long long)hc->dev, (long long)hc->inode, hc->mtime, buf);
	free(buf);
    }
    fclose(stream);
}

static void hashread(struct socket *sk, void *uudata)
{
    static char *hashbuf;
    static size_t hashbufsize = 0, hashbufdata = 0;
    char *buf, *p, *p2, *lp;
    size_t bufsize;
    char *wv[32];
    int wc;
    dev_t dev;
    ino_t inode;
    time_t mtime;
    struct hashcache *hc;
    
    if((buf = sockgetinbuf(sk, &bufsize)) == NULL)
	return;
    bufcat(hashbuf, buf, bufsize);
    free(buf);
    while((lp = memchr(hashbuf, '\n', hashbufdata)) != NULL)
    {
	*(lp++) = 0;
	wc = 0;
	p = hashbuf;
	while(1)
	{
	    while((p2 = strchr(p, ' ')) == p)
		p++;
	    wv[wc++] = p;
	    if(p2 == NULL)
	    {
		break;
	    } else {
		*p2 = 0;
		p = p2 + 1;
	    }
	}
	if(wc != 4)
	{
	    flog(LOG_ERR, "BUG: unexpected number of words (%i) arrived from hashing process", wc);
	} else {
	    dev = strtoll(wv[0], NULL, 10);
	    inode = strtoll(wv[1], NULL, 10);
	    mtime = strtol(wv[2], NULL, 10);
	    if((hc = findhashcache(dev, inode)) == NULL)
	    {
		hc = newhashcache();
		hc->dev = dev;
		hc->inode = inode;
	    }
	    hc->mtime = mtime;
	    buf = base64decode(wv[3], NULL);
	    memcpy(hc->tth, buf, 24);
	    free(buf);
	    writehashcache(0);
	}
	memmove(hashbuf, lp, hashbufdata -= (lp - hashbuf));
    }
}

static void hashexit(pid_t pid, int status, struct socket *outsock)
{
    if(pid != hashjob)
	flog(LOG_ERR, "BUG: hashing process changed PID?! old: %i new %i", hashjob, pid);
    if(status)
	flog(LOG_WARNING, "hashing process exited with non-zero status: %i", status);
    hashjob = 0;
    checkhashes();
    putsock(outsock);
}

static int hashfile(char *path)
{
    int i, ret;
    int fd;
    int pfd[2];
    char buf[4096];
    struct stat sb;
    struct tigertreehash tth;
    char digest[24];
    struct socket *outsock;
    
    if((fd = open(path, O_RDONLY)) < 0)
    {
	flog(LOG_WARNING, "could not open %s for hashing: %s", path, strerror(errno));
	return(1);
    }
    if(fstat(fd, &sb) < 0)
    {
	flog(LOG_WARNING, "could not stat %s while hashing: %s", path, strerror(errno));
	close(fd);
	return(1);
    }
    if(pipe(pfd) < 0)
    {
	flog(LOG_WARNING, "could not create pipe(!): %s", strerror(errno));
	close(fd);
	return(1);
    }
    hashjob = fork();
    if(hashjob < 0)
    {
	flog(LOG_WARNING, "could not fork(!) hashing process: %s", strerror(errno));
	close(fd);
	close(pfd[0]);
	close(pfd[1]);
	return(1);
    }
    if(hashjob == 0)
    {
	nice(10);
	signal(SIGHUP, SIG_DFL);
	fd = dup2(fd, 4);
	pfd[1] = dup2(pfd[1], 3);
	dup2(fd, 0);
	dup2(pfd[1], 1);
	for(i = 3; i < FD_SETSIZE; i++)
	    close(i);
	initlog();
	inittigertree(&tth);
	while((ret = read(0, buf, 4096)) > 0)
	    dotigertree(&tth, buf, ret);
	if(ret < 0)
	{
	    flog(LOG_WARNING, "could not read from %s while hashing: %s", path, strerror(errno));
	    exit(1);
	}
	synctigertree(&tth);
	restigertree(&tth, digest);
	ret = snprintf(buf, sizeof(buf), "%lli %lli %li %s\n", (long long)sb.st_dev, (long long)sb.st_ino, sb.st_mtime, base64encode(digest, 24));
	write(1, buf, ret);
	exit(0);
    }
    close(fd);
    close(pfd[1]);
    outsock = wrapsock(pfd[0]);
    outsock->readcb = hashread;
    childcallback(hashjob, (void (*)(pid_t, int, void *))hashexit, outsock);
    return(0);
}

/*
 * Call only when hashjob == 0
 */
static void checkhashes(void)
{
    struct sharecache *node, *next;
    struct hashcache *hc;
    char *path;
    
    node = shareroot->child;
    for(node = shareroot->child; node != NULL; node = next)
    {
	next = nextscnode(node);
	if(node->f.b.type != FILE_REG)
	    continue;
	if(!node->f.b.hastth)
	{
	    if(((hc = findhashcache(node->dev, node->inode)) != NULL) && (hc->mtime == node->mtime))
	    {
		memcpy(node->hashtth, hc->tth, 24);
		node->f.b.hastth = 1;
		GCBCHAINDOCB(sharechangecb, sharesize);
	    } else {
		path = getfspath(node);
		if(hashfile(path))
		{
		    flog(LOG_WARNING, "could not hash %s, unsharing it", path);
		    freecache(node);
		    free(path);
		    flog(LOG_INFO, "sharing %lli bytes", sharesize);
		    continue;
		}
		free(path);
		return;
	    }
	}
    }
}

struct sharecache *nextscnode(struct sharecache *node)
{
    if(node->child != NULL)
	return(node->child);
    while(node->next == NULL)
    {
	node = node->parent;
	if(node == shareroot)
	    return(NULL);
    }
    return(node->next);
}

static void freescan(struct scanstate *job)
{
    if(job->dd != NULL)
	closedir(job->dd);
    free(job);
}

/* No need for optimization; lookup isn't really that common */
struct sharecache *findcache(struct sharecache *parent, wchar_t *name)
{
    struct sharecache *node;
    
    for(node = parent->child; node != NULL; node = node->next)
    {
	if(!wcscmp(node->name, name))
	    return(node);
    }
    return(NULL);
}

static void attachcache(struct sharecache *parent, struct sharecache *node)
{
    node->parent = parent;
    node->next = parent->child;
    if(parent->child != NULL)
	parent->child->prev = node;
    parent->child = node;
}

static void detachcache(struct sharecache *node)
{
    if(node->next != NULL)
	node->next->prev = node->prev;
    if(node->prev != NULL)
	node->prev->next = node->next;
    if((node->parent != NULL) && (node->parent->child == node))
	node->parent->child = node->next;
    node->parent = NULL;
    node->next = NULL;
    node->prev = NULL;
}

static void freecache(struct sharecache *node)
{
    struct sharecache *cur, *next;
    struct scanqueue *q, *nq, **fq;
    
    detachcache(node);
    fq = &scanqueue;
    for(q = scanqueue; q != NULL; q = nq)
    {
	nq = q->next;
	if(q->state->node == node)
	{
	    flog(LOG_DEBUG, "freed node %ls cancelled queued scan", node->name);
	    freescan(q->state);
	    *fq = q->next;
	    free(q);
	    continue;
	}
	fq = &q->next;
    }
    if(node->child != NULL)
    {
	for(cur = node->child; cur != NULL; cur = next)
	{
	    next = cur->next;
	    freecache(cur);
	}
    }
    CBCHAINDOCB(node, share_delete, node);
    CBCHAINFREE(node, share_delete);
    sharesize -= node->size;
    if(node->path != NULL)
	free(node->path);
    if(node->name != NULL)
	free(node->name);
    free(node);
}

static void freesharepoint(struct sharepoint *share)
{
    struct sharecache *node;
    
    if(share->next != NULL)
	share->next->prev = share->prev;
    if(share->prev != NULL)
	share->prev->next = share->next;
    if(share == shares)
	shares = share->next;
    if((node = findcache(shareroot, share->name)) != NULL)
	freecache(node);
    free(share->path);
    free(share->name);
    free(share);
}

static struct sharecache *newcache(void)
{
    struct sharecache *new;
    
    new = smalloc(sizeof(*new));
    memset(new, 0, sizeof(*new));
    CBCHAININIT(new, share_delete);
    return(new);
}

char *getfspath(struct sharecache *node)
{
    char *buf, *mbsname;
    size_t bufsize;
    
    buf = smalloc(bufsize = 64);
    *buf = 0;
    while(node != NULL)
    {
	if(node->path != NULL)
	{
	    if(bufsize < strlen(node->path) + strlen(buf) + 1)
		buf = srealloc(buf, strlen(node->path) + strlen(buf) + 1);
	    memmove(buf + strlen(node->path), buf, strlen(buf) + 1);
	    memcpy(buf, node->path, strlen(node->path));
	    return(buf);
	}
	if((mbsname = icwcstombs(node->name, NULL)) == NULL)
	{
	    flog(LOG_WARNING, "could not map unicode share name (%ls) into filesystem charset: %s", node->name, strerror(errno));
	    free(buf);
	    return(NULL);
	}
	while(bufsize < strlen(mbsname) + 1 + strlen(buf) + 1)
	    buf = srealloc(buf, bufsize *= 2);
	memmove(buf + strlen(mbsname) + 1, buf, strlen(buf) + 1);
	memcpy(buf + 1, mbsname, strlen(mbsname));
	*buf = '/';
	free(mbsname);
	node = node->parent;
    }
    buf = srealloc(buf, strlen(buf) + 1);
    return(buf);
}

static int checknode(struct sharecache *node)
{
    char *path;
    struct stat sb;
    
    if(node->parent == NULL)
    {
	return(1);
    } else {
	if(!checknode(node->parent))
	    return(0);
	path = getfspath(node);
	if(stat(path, &sb) < 0)
	{
	    flog(LOG_INFO, "%s was found to be broken (%s); scheduling rescan of parent", path, strerror(errno));
	    queuescan(node->parent);
	    return(0);
	} else {
	    return(1);
	}
    }
}

int opensharecache(struct sharecache *node)
{
    char *path;
    int fd, errbak;
    
    path = getfspath(node);
    fd = open(path, O_RDONLY);
    errbak = errno;
    if(fd < 0)
    {
	flog(LOG_WARNING, "could not open %s: %s", path, strerror(errbak));
	checknode(node);
    }
    free(path);
    errno = errbak;
    return(fd);
}

static struct scanstate *newscan(struct sharecache *node)
{
    struct scanstate *new;
    
    new = smalloc(sizeof(*new));
    new->next = NULL;
    new->node = node;
    new->dd = NULL;
    return(new);
}

void queuescan(struct sharecache *node)
{
    struct scanqueue *new;
    
    new = smalloc(sizeof(*new));
    new->state = newscan(node);
    new->next = scanqueue;
    scanqueue = new;
}

/* For internal use in doscan() */
static void removestale(struct sharecache *node)
{
    struct sharecache *cur, *next;
    
    for(cur = node->child; cur != NULL; cur = next)
    {
	next = cur->next;
	if(!cur->f.b.found)
	    freecache(cur);
    }
}

/* For internal use in doscan() */
static void jobdone(void)
{
    struct scanstate *jbuf;
    
    jbuf = scanjob;
    scanjob = jbuf->next;
    freescan(jbuf);
    if(scanjob != NULL)
	fchdir(dirfd(scanjob->dd));
}

int doscan(int quantum)
{
    char *path;
    wchar_t *wcs;
    int type;
    struct sharecache *n;
    struct scanstate *jbuf;
    struct scanqueue *qbuf;
    struct dirent *de;
    struct stat sb;
    struct hashcache *hc;
    int dmask, fmask;
    static int busybefore = 0;
    
    dmask = confgetint("cli", "scandirmask");
    fmask = confgetint("cli", "scanfilemask");
    if((scanjob != NULL) && (scanjob->dd != NULL))
    {
	while(fchdir(dirfd(scanjob->dd)) < 0)
	{
	    flog(LOG_WARNING, "could not fchdir to fd %i: %s", dirfd(scanjob->dd), strerror(errno));
	    removestale(scanjob->node);
	    jobdone();
	}
    }
    while(quantum-- > 0)
    {
	if(scanjob != NULL)
	{
	    busybefore = 1;
	} else {
	    while(scanjob == NULL)
	    {
		if(scanqueue == NULL)
		{
		    if(busybefore)
		    {
			flog(LOG_INFO, "sharing %lli bytes", sharesize);
			busybefore = 0;
			GCBCHAINDOCB(sharechangecb, sharesize);
			if(hashjob == 0)
			    checkhashes();
		    }
		    return(0);
		}
		busybefore = 1;
		scanjob = scanqueue->state;
		qbuf = scanqueue;
		scanqueue = qbuf->next;
		free(qbuf);
		for(n = scanjob->node->child; n != NULL; n = n->next)
		    n->f.b.found = 0;
	    }
	}
	if(scanjob->dd == NULL)
	{
	    path = getfspath(scanjob->node);
	    if((scanjob->dd = opendir(path)) == NULL)
	    {
		flog(LOG_WARNING, "cannot open directory %s for scanning: %s, deleting from share", path, strerror(errno));
		freecache(scanjob->node);
		free(path);
		jobdone();
		continue;
	    }
	    free(path);
	    if(fchdir(dirfd(scanjob->dd)) < 0)
	    {
		flog(LOG_WARNING, "could not fchdir to fd %i: %s", dirfd(scanjob->dd), strerror(errno));
		jobdone();
		continue;
	    }
	}
	if((de = readdir(scanjob->dd)) == NULL)
	{
	    removestale(scanjob->node);
	    jobdone();
	    continue;
	}
	if(*de->d_name == '.')
	    continue;
	if((wcs = icmbstowcs(de->d_name, NULL)) == NULL)
	{
	    flog(LOG_WARNING, "file name %s has cannot be converted to wchar: %s", de->d_name, strerror(errno));
	    continue;
	}
	n = findcache(scanjob->node, wcs);
	if(stat(de->d_name, &sb) < 0)
	{
	    free(wcs);
	    if(n != NULL)
	    {
		flog(LOG_WARNING, "could not stat %s: %s, deleting from share", de->d_name, strerror(errno));
		freecache(n);
	    } else {
		flog(LOG_WARNING, "could not stat %s: %s", de->d_name, strerror(errno));
	    }
	    continue;
	}
	if(S_ISDIR(sb.st_mode))
	{
	    if(~sb.st_mode & dmask)
	    {
		free(wcs);
		continue;
	    }
	    type = FILE_DIR;
	} else if(S_ISREG(sb.st_mode)) {
	    if(~sb.st_mode & fmask)
	    {
		free(wcs);
		continue;
	    }
	    type = FILE_REG;
	} else {
	    flog(LOG_WARNING, "unhandled file type: 0%o", sb.st_mode);
	    free(wcs);
	    continue;
	}
	if(n != NULL)
	{
	    if((n->f.b.type != type) || (n->mtime != sb.st_mtime) || ((type == FILE_REG) && (n->size != sb.st_size)))
	    {
		freecache(n);
		n = NULL;
	    }
	}
	if(n == NULL)
	{
	    n = newcache();
	    n->name = wcs;
	    if(S_ISREG(sb.st_mode))
	    {
		sharesize += (n->size = sb.st_size);
	    } else {
		n->size = 0;
	    }
	    n->mtime = sb.st_mtime;
	    n->dev = sb.st_dev;
	    n->inode = sb.st_ino;
	    n->f.b.type = type;
	    attachcache(scanjob->node, n);
	} else {
	    free(wcs);
	}
	n->f.b.found = 1;
	if(n->f.b.type == FILE_DIR)
	{
	    jbuf = newscan(n);
	    jbuf->next = scanjob;
	    scanjob = jbuf;
	} else if(n->f.b.type == FILE_REG) {
	    if(n->f.b.hastth && (n->mtime != sb.st_mtime))
		n->f.b.hastth = 0;
	    if(!n->f.b.hastth)
	    {
		if((hc = findhashcache(sb.st_dev, sb.st_ino)) != NULL)
		{
		    if(hc->mtime == n->mtime)
		    {
			n->f.b.hastth = 1;
			memcpy(n->hashtth, hc->tth, 24);
		    } else {
			freehashcache(hc);
		    }
		}
	    }
	}
    }
    return(1);
}

static void rescancb(int cancelled, void *uudata)
{
    scantimer = NULL;
    if(!cancelled)
    {
	if(scanqueue == NULL)
	    scanshares();
	else if(confgetint("cli", "rescandelay") > 0)
	    scantimer = timercallback(ntime() + confgetint("cli", "rescandelay"), (void (*)(int, void *))rescancb, NULL);
    }
}

void scanshares(void)
{
    struct sharepoint *cur;
    struct sharecache *node;
    struct stat sb;
    
    for(cur = shares; cur != NULL; cur = cur->next)
    {
	if((node = findcache(shareroot, cur->name)) == NULL)
	{
	    if(stat(cur->path, &sb))
	    {
		flog(LOG_WARNING, "could not stat share \"%ls\": %s", cur->name, strerror(errno));
		continue;
	    }
	    if(!S_ISDIR(sb.st_mode))
	    {
		flog(LOG_WARNING, "%s is not a directory; won't share it", cur->path);
		continue;
	    }
	    node = newcache();
	    node->name = swcsdup(cur->name);
	    node->path = sstrdup(cur->path);
	    if(node->path[strlen(node->path) - 1] == '/')
		node->path[strlen(node->path) - 1] = 0;
	    node->f.b.type = FILE_DIR;
	    attachcache(shareroot, node);
	}
	queuescan(node);
    }
    if(scantimer != NULL)
	canceltimer(scantimer);
    if(confgetint("cli", "rescandelay") > 0)
	scantimer = timercallback(ntime() + confgetint("cli", "rescandelay"), (void (*)(int, void *))rescancb, NULL);
}

static void preinit(int hup)
{
    struct sharepoint *cur;
    
    if(hup)
    {
	for(cur = shares; cur != NULL; cur = cur->next)
	    cur->delete = 1;
    } else {
	shareroot = newcache();
	shareroot->name = swcsdup(L"");
	shareroot->f.b.type = FILE_DIR;
    }
}

static int rsdelayupdate(struct configvar *var, void *uudata)
{
    if(scantimer != NULL)
	canceltimer(scantimer);
    if(confgetint("cli", "rescandelay") > 0)
	scantimer = timercallback(ntime() + var->val.num, (void (*)(int, void *))rescancb, NULL);
    return(0);
}

static int init(int hup)
{
    struct sharepoint *cur, *next;
    
    readhashcache();
    for(cur = shares; cur != NULL; cur = next)
    {
	next = cur->next;
	if(cur->delete)
	    freesharepoint(cur);
    }
    scanshares();
    if(!hup)
    {
	while(doscan(100));
	CBREG(confgetvar("cli", "rescandelay"), conf_update, rsdelayupdate, NULL, NULL);
    }
    return(0);
}

static int run(void)
{
    if(hashjob == -1)
    {
	hashjob = 0;
	checkhashes();
    }
    return(doscan(10));
}

static void terminate(void)
{
    if(hashjob != 0)
	kill(hashjob, SIGHUP);
    while(shares != NULL)
	freesharepoint(shares);
    freecache(shareroot);
}

static struct module me =
{
    .name = "cli",
    .conf =
    {
	.vars = myvars,
	.cmds = mycmds
    },
    .preinit = preinit,
    .init = init,
    .run = run,
    .terminate = terminate
};

MODULE(me)
