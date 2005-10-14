#ifndef _UTILS_H
#define _UTILS_H

#include <stdarg.h>
#include <stdlib.h>

/* "Safe" functions */
#define smalloc(size) ({void *__result__; ((__result__ = malloc(size)) == NULL)?({exit(-1); (void *)0;}):__result__;})
#define srealloc(ptr, size) ({void *__result__; ((__result__ = realloc((ptr), (size))) == NULL)?({exit(-1); (void *)0;}):__result__;})
#define swcsdup(wcs) ((wchar_t *)wcscpy(smalloc(sizeof(wchar_t) * (wcslen(wcs) + 1)), (wcs)))
#define sstrdup(str) ((char *)strcpy(smalloc(strlen(str) + 1), (str)))

char *vsprintf2(char *format, va_list al);
char *sprintf2(char *format, ...);
wchar_t *vswprintf2(wchar_t *format, va_list al);
wchar_t *swprintf2(wchar_t *format, ...);
wchar_t *icmbstowcs(char *mbs, char *charset);
wchar_t *icsmbstowcs(char *mbs, char *charset, wchar_t *def);
char *icwcstombs(wchar_t *wcs, char *charset);
char *icswcstombs(wchar_t *wcs, char *charset, char *def);
wchar_t *wcstolower(wchar_t *wcs);
void _sizebuf(void **buf, size_t *bufsize, size_t reqsize, size_t elsize, int algo);

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

#endif
