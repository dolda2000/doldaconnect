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
#ifndef _UTILS_H
#define _UTILS_H

#include <stdarg.h>
#include <stdlib.h>
#include <malloc.h>
#include <assert.h>
#ifdef DAEMON
#include "log.h"
#endif

struct wcspair {
    struct wcspair *next;
    wchar_t *key;
    wchar_t *val;
};


/* "Safe" functions */
#ifdef DAEMON
#define LOGOOM(size) flog(LOG_CRIT, "%s (%s:%i): out of memory (alloc %i)", __FUNCTION__, __FILE__, __LINE__, (size))
#define smalloc(size) ({void *__result__; ((__result__ = malloc(size)) == NULL)?({LOGOOM(size); abort(); (void *)0;}):__result__;})
#define srealloc(ptr, size) ({void *__result__; ((__result__ = realloc((ptr), (size))) == NULL)?({LOGOOM(size); abort(); (void *)0;}):__result__;})
#define swcsdup(wcs) ((wchar_t *)wcscpy(smalloc(sizeof(wchar_t) * (wcslen(wcs) + 1)), (wcs)))
#define sstrdup(str) ((char *)strcpy(smalloc(strlen(str) + 1), (str)))
#else
#define LOGOOM(size)
#define smalloc(size) ({void *__result__; ((__result__ = malloc(size)) == NULL)?({exit(-1); (void *)0;}):__result__;})
#define srealloc(ptr, size) ({void *__result__; ((__result__ = realloc((ptr), (size))) == NULL)?({exit(-1); (void *)0;}):__result__;})
#define swcsdup(wcs) ((wchar_t *)wcscpy(smalloc(sizeof(wchar_t) * (wcslen(wcs) + 1)), (wcs)))
#define sstrdup(str) ((char *)strcpy(smalloc(strlen(str) + 1), (str)))
#endif

#define CBCHAIN(name, args...) \
struct cbchain_ ## name { \
    struct cbchain_ ## name *next, *prev; \
    int (*func)(args, void *data); \
    void (*destroy)(void *data); \
    void *data; \
    int running, free; \
} * name

#define GCBCHAIN(name, args...) \
struct cbchain_ ## name * name = NULL

#define EGCBCHAIN(name, args...) \
extern struct cbchain_ ## name { \
    struct cbchain_ ## name *next, *prev; \
    int (*func)(args, void *data); \
    void *data; \
} * name

extern int vswprintf (wchar_t *__restrict __s, size_t __n,
		      __const wchar_t *__restrict __format,
		      __gnuc_va_list __arg);
extern int swprintf (wchar_t *__restrict __s, size_t __n,
		     __const wchar_t *__restrict __format, ...);

char *vsprintf2(char *format, va_list al);
char *sprintf2(char *format, ...)
#if defined(__GNUC__) && 0
    __attribute__ ((format (printf, 1, 2)))
#endif
;
wchar_t *vswprintf2(wchar_t *format, va_list al);
wchar_t *swprintf2(wchar_t *format, ...);
int havecharset(char *charset);
wchar_t *icmbstowcs(char *mbs, char *charset);
wchar_t *icsmbstowcs(char *mbs, char *charset, wchar_t *def);
char *icwcstombs(wchar_t *wcs, char *charset);
char *icswcstombs(wchar_t *wcs, char *charset, char *def);
wchar_t *wcstolower(wchar_t *wcs);
wchar_t ucptowc(int ucp);
void _sizebuf(void **buf, size_t *bufsize, size_t reqsize, size_t elsize, int algo);
double ntime(void);
int wcsexists(wchar_t *h, wchar_t *n);
#ifndef HAVE_WCSCASECMP
int wcscasecmp(const wchar_t *s1, const wchar_t *s2);
#endif
char *hexencode(char *data, size_t datalen);
char *hexdecode(char *data, size_t *len);
char *base64encode(char *data, size_t datalen);
char *base64decode(char *data, size_t *datalen);
char *base32encode(char *data, size_t datalen);
char *base32decode(char *data, size_t *datalen);
void _freeparr(void **arr);
int _parrlen(void **arr);
char *findfile(char *gname, char *uname, char *homedir, int filldef);
struct wcspair *newwcspair(wchar_t *key, wchar_t *val, struct wcspair **list);
void freewcspair(struct wcspair *pair, struct wcspair **list);
wchar_t *wpfind(struct wcspair *list, wchar_t *key);

#define sizebuf(b, bs, rs, es, a) _sizebuf((void **)(b), (bs), (rs), (es), (a))
#define sizebuf2(b, rs, a) _sizebuf((void **)(&(b)), &(b ## size), (rs), sizeof(*(b)), (a))
#define addtobuf(b, c) \
do { \
    _sizebuf((void **)(&(b)), &(b ## size), (b ## data) + 1, sizeof(*(b)), 1); \
    (b)[(b ## data)++] = (c); \
} while(0)
#define bufcat(d, s, n) \
do { \
    size_t __bufcat_size__; \
    __bufcat_size__ = (n); \
    _sizebuf((void **)(&(d)), &(d ## size), (d ## data) + __bufcat_size__, sizeof(*(d)), 1); \
    memcpy((d) + (d ## data), (s), sizeof(*(d)) * __bufcat_size__); \
    (d ## data) += __bufcat_size__; \
} while (0)

#define freeparr(parr) _freeparr((void **)(parr))
#define parrlen(parr) _parrlen((void **)(parr))

#define CBREG(obj, name, funca, destroya, dataa) \
do { \
    struct cbchain_ ## name *__new_cb__; \
    __new_cb__ = smalloc(sizeof(*__new_cb__)); \
    __new_cb__->running = __new_cb__->free = 0; \
    __new_cb__->func = funca; \
    __new_cb__->destroy = destroya; \
    __new_cb__->data = dataa; \
    __new_cb__->prev = NULL; \
    __new_cb__->next = (obj)->name; \
    if((obj)->name != NULL) { \
        (obj)->name->prev = __new_cb__; \
    } \
    (obj)->name = __new_cb__; \
} while(0)

#define CBUNREG(obj, name, funca, dataa) \
({ \
    struct cbchain_ ## name *__cur__; \
    int __found__ = 0; \
    for(__cur__ = (obj)->name; __cur__ != NULL; __cur__ = __cur__->next) { \
        if(((void *)(__cur__->func) == (void *)(funca)) && (__cur__->data == (void *)(dataa))) { \
            if(__cur__->destroy != NULL) \
                __cur__->destroy(__cur__->data); \
            if(__cur__->prev != NULL) \
                __cur__->prev->next = __cur__->next; \
            if(__cur__->next != NULL) \
                __cur__->next->prev = __cur__->prev; \
            if(__cur__ == (obj)->name) \
                (obj)->name = __cur__->next; \
            free(__cur__); \
            __found__ = 1; \
            break; \
        } \
    } \
__found__;})

#define GCBREG(name, funca, dataa) \
do { \
    struct cbchain_ ## name *__new_cb__; \
    __new_cb__ = smalloc(sizeof(*__new_cb__)); \
    __new_cb__->func = funca; \
    __new_cb__->data = dataa; \
    __new_cb__->prev = NULL; \
    __new_cb__->next = name; \
    if(name != NULL) { \
        name->prev = __new_cb__; \
    } \
    name = __new_cb__; \
} while(0)

#define CBCHAININIT(obj, name) (obj)->name = NULL

#define CBCHAINFREE(obj, name) \
do { \
    struct cbchain_ ## name *__cur__; \
    while((__cur__ = (obj)->name) != NULL) { \
        (obj)->name = __cur__->next; \
        if(__cur__->running) { \
            __cur__->free = 1; \
        } else { \
            if(__cur__->destroy != NULL) \
                __cur__->destroy(__cur__->data); \
            free(__cur__); \
       } \
    } \
} while(0)

#define CBCHAINDOCB(obj, name, args...) \
do { \
    struct cbchain_ ## name *__cur__, *__next__; \
    int __res__; \
    for(__cur__ = (obj)->name; __cur__ != NULL; __cur__ = __next__) { \
        __next__ = __cur__->next; \
        __cur__->running = 1; \
        __res__ = __cur__->func(args, __cur__->data); \
        __cur__->running = 0; \
        if(__cur__->free) { \
            free(__cur__); \
            break; \
        } \
        if(__res__) { \
            if(__cur__->next != NULL) \
                __cur__->next->prev = __cur__->prev; \
            if(__cur__->prev != NULL) \
                __cur__->prev->next = __cur__->next; \
            if(__cur__ == (obj)->name) \
                (obj)->name = __cur__->next; \
            free(__cur__); \
        } \
    } \
} while(0)

#define GCBCHAINDOCB(name, args...) \
({ \
    struct cbchain_ ## name *__cur__; \
    int __ret__; \
    __ret__ = 0; \
    for(__cur__ = name; __cur__ != NULL; __cur__ = __cur__->next) { \
        if(__cur__->func(args, __cur__->data)) { \
            __ret__ = 1; \
            break; \
        } \
    } \
    __ret__; \
})

#endif
