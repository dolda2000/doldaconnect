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
#include <stdarg.h>
#include <stdio.h>
#include <wchar.h>
#include <iconv.h>
#include <errno.h>
#include <string.h>
#include <wctype.h>
#include <langinfo.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/time.h>
#include <netinet/in.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <utils.h>
#include <log.h>

struct treeiter {
    struct {
	struct btree *n;
	int s;
    } *st;
    size_t stsize;
    int sp;
};

static char *base64set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static int base64rev[] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};
static char *base32set = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
static int base32rev[] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

char *vsprintf2(char *format, va_list al)
{
    int ret;
    char *buf;
    va_list al2;
    
    va_copy(al2, al);
    ret = vsnprintf(NULL, 0, format, al2);
    va_end(al2);
    if((buf = malloc(ret + 1)) == NULL)
    {
	LOGOOM(ret + 1);
	return(NULL);
    }
    va_copy(al2, al);
    vsnprintf(buf, ret + 1, format, al2);
    va_end(al2);
    return(buf);
}

char *sprintf2(char *format, ...)
{
    va_list args;
    char *buf;
    
    va_start(args, format);
    buf = vsprintf2(format, args);
    va_end(args);
    return(buf);
}

wchar_t *vswprintf2(wchar_t *format, va_list al)
{
    int ret;
    wchar_t *buf;
    size_t bufsize;
    va_list al2;
    
    buf = smalloc(sizeof(wchar_t) * (bufsize = 1024));
    while(1)
    {
	va_copy(al2, al);
	ret = vswprintf(buf, bufsize, format, al2);
	va_end(al2);
	if(ret >= 0)
	    break;
	buf = srealloc(buf, sizeof(wchar_t) * (bufsize *= 2));
    }
    if(bufsize > ret + 1)
	buf = srealloc(buf, sizeof(wchar_t) * (ret + 1));
    return(buf);
}

wchar_t *swprintf2(wchar_t *format, ...)
{
    va_list args;
    wchar_t *buf;
    
    va_start(args, format);
    buf = vswprintf2(format, args);
    va_end(args);
    return(buf);
}

int havecharset(char *charset)
{
    iconv_t cd;
    
    if((cd = iconv_open("wchar_t", charset)) == (iconv_t)-1)
	return(0);
    iconv_close(cd);
    if((cd = iconv_open(charset, "wchar_t")) == (iconv_t)-1)
	return(0);
    iconv_close(cd);
    return(1);
}

wchar_t *icmbstowcs(char *mbs, char *charset)
{
    int ret;
    char *buf;
    char *p, *p2;
    size_t len1, len2, bufsize, data;
    iconv_t cd;
    
    len1 = strlen(mbs) + 1;
    bufsize = len2 = len1 * sizeof(wchar_t);
    if((buf = malloc(bufsize)) == NULL)
    {
	LOGOOM(bufsize);
	return(NULL);
    }
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open("wchar_t", charset)) == (iconv_t)-1)
    {
#ifdef DAEMON
	flog(LOG_ERR, "icmbstowcs: could not open iconv structure for %s: %s", charset, strerror(errno));
#endif
	free(buf);
	return(NULL);
    }
    p = buf;
    while(len1 > 0)
    {
	ret = iconv(cd, &mbs, &len1, &p, &len2);
	if(ret < 0)
	{
	    if(errno == E2BIG)
	    {
		data = p - buf;
		len2 += 128;
		bufsize += 128;
		if((p2 = realloc(buf, bufsize)) == NULL)
		{
		    LOGOOM(bufsize);
		    free(buf);
		    iconv_close(cd);
		    return(NULL);
		}
		buf = p2;
		p = buf + data;
	    } else {
		free(buf);
		iconv_close(cd);
		return(NULL);
	    }
	}
    }
    if(len2 > 0)
	buf = realloc(buf, p - buf);
    iconv_close(cd);
    return((wchar_t *)buf);
}

wchar_t *icsmbstowcs(char *mbs, char *charset, wchar_t *def)
{
    static wchar_t *buf = NULL;
    
    if(buf != NULL)
	free(buf);
    if((buf = icmbstowcs(mbs, charset)) == NULL)
    {
	if((def != NULL) && (*def == L'~'))
	{
#ifdef DAEMON
	    flog(LOG_WARNING, "icsmbstowcs: could not convert wcs string into charset %s: %s", charset, strerror(errno));
#endif
	    def++;
	}
	return(def);
    }
    return(buf);
}

char *icwcstombs(wchar_t *wcs, char *charset)
{
    int ret;
    char *buf;
    char *p, *p2;
    size_t len1, len2, bufsize, data;
    iconv_t cd;
    
    len1 = sizeof(wchar_t) * (wcslen(wcs) + 1);
    bufsize = len2 = len1;
    if((buf = malloc(bufsize)) == NULL)
    {
#ifdef DAEMON
	LOGOOM(bufsize);
#endif
	return(NULL);
    }
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open(charset, "wchar_t")) == (iconv_t)-1)
    {
#ifdef DAEMON
	flog(LOG_ERR, "icwcstombs: could not open iconv structure for %s: %s", charset, strerror(errno));
#endif
	free(buf);
	return(NULL);
    }
    p = buf;
    while(len1 > 0)
    {
	ret = iconv(cd, (char **)&wcs, &len1, &p, &len2);
	if(ret < 0)
	{
	    if(errno == E2BIG)
	    {
		data = p - buf;
		len2 += 128;
		bufsize += 128;
		if((p2 = realloc(buf, bufsize)) == NULL)
		{
		    LOGOOM(bufsize);
		    free(buf);
		    iconv_close(cd);
		    return(NULL);
		}
		buf = p2;
		p = buf + data;
	    } else {
		free(buf);
		iconv_close(cd);
		return(NULL);
	    }
	}
    }
    if(len2 > 0)
	buf = realloc(buf, p - buf);
    iconv_close(cd);
    return(buf);
}

char *icswcstombs(wchar_t *wcs, char *charset, char *def)
{
    static char *buf = NULL;
    
    if(buf != NULL)
	free(buf);
    if((buf = icwcstombs(wcs, charset)) == NULL)
    {
	if((def != NULL) && (*def == '~'))
	{
#ifdef DAEMON
	    flog(LOG_WARNING, "icswcstombs: could not convert mbs string from charset %s: %s", charset, strerror(errno));
#endif
	    def++;
	}
	return(def);
    }
    return(buf);
}

wchar_t *wcstolower(wchar_t *wcs)
{
    wchar_t *p;
    
    for(p = wcs; *p != L'\0'; p++)
	*p = towlower(*p);
    return(wcs);
}

wchar_t ucptowc(int ucp)
{
    int ret;
    unsigned long ucpbuf;
    char *buf;
    char *mbsp, *p, *p2;
    wchar_t res;
    size_t len1, len2, bufsize, data;
    iconv_t cd;
    
    ucpbuf = htonl(ucp);
    mbsp = (char *)&ucpbuf;
    len1 = 4;
    bufsize = len2 = len1 * sizeof(wchar_t);
    if((buf = malloc(bufsize)) == NULL)
    {
	LOGOOM(bufsize);
	return(L'\0');
    }
    if((cd = iconv_open("wchar_t", "UCS-4BE")) == (iconv_t)-1)
    {
#ifdef DAEMON
	flog(LOG_ERR, "ucptowc: could not open iconv structure for UCS-4BE: %s", strerror(errno));
#endif
	free(buf);
	return(L'\0');
    }
    p = buf;
    while(len1 > 0)
    {
	ret = iconv(cd, &mbsp, &len1, &p, &len2);
	if(ret < 0)
	{
	    if(errno == E2BIG)
	    {
		data = p - buf;
		len2 += 128;
		bufsize += 128;
		if((p2 = realloc(buf, bufsize)) == NULL)
		{
		    LOGOOM(bufsize);
		    free(buf);
		    iconv_close(cd);
		    return(L'\0');
		}
		buf = p2;
		p = buf + data;
	    } else {
		free(buf);
		iconv_close(cd);
		return(L'\0');
	    }
	}
    }
    if(len2 > 0)
	buf = realloc(buf, p - buf);
    iconv_close(cd);
    res = *(wchar_t *)buf;
    free(buf);
    return(res);
}

void _sizebuf(void **buf, size_t *bufsize, size_t reqsize, size_t elsize, int algo)
{
    if(*bufsize >= reqsize)
	return;
    switch(algo)
    {
    case 0:
	*buf = srealloc(*buf, elsize * ((*bufsize) = reqsize));
	break;
    case 1:
	if(*bufsize == 0)
	    *bufsize = 1;
	while(*bufsize < reqsize)
	    *bufsize <<= 1;
	*buf = srealloc(*buf, elsize * (*bufsize));
	break;
    }
}

double ntime(void)
{
    struct timeval tv;
    
    gettimeofday(&tv, NULL);
    return((double)tv.tv_sec + ((double)tv.tv_usec / 1000000.0));
}

wchar_t *wcslower(wchar_t *wcs)
{
    wchar_t *p;
    
    for(p = wcs; *p != L'\0'; p++)
	*p = towlower(*p);
    return(wcs);
}

#ifndef HAVE_WCSCASECMP
int wcscasecmp(const wchar_t *s1, const wchar_t *s2)
{
    for(; (towlower(*s1) == towlower(*s2)) && (*s1 != L'\0'); s1++, s2++);
    return(towlower(*s1) - towlower(*s2));
}
#endif

char *hexencode(char *data, size_t datalen)
{
    char *buf, this;
    size_t bufsize, bufdata;
    int dig;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(; datalen > 0; datalen--, data++)
    {
	dig = (*data & 0xF0) >> 4;
	if(dig > 9)
	    this = 'A' + dig - 10;
	else
	    this = dig + '0';
	addtobuf(buf, this);
	dig = *data & 0x0F;
	if(dig > 9)
	    this = 'A' + dig - 10;
	else
	    this = dig + '0';
	addtobuf(buf, this);
    }
    addtobuf(buf, 0);
    return(buf);
}

char *hexdecode(char *data, size_t *len)
{
    char *buf, this, bit;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(bit = 4, this = 0; *data; data++)
    {
	if((*data >= 'A') && (*data <= 'F'))
	{
	    this |= (this & 0x0F) | ((*data - 'A' + 10) << bit);
	} else if((*data >= 'a') && (*data <= 'f')) {
	    this |= (this & 0x0F) | ((*data - 'a' + 10) << bit);
	} else if((*data >= '0') && (*data <= '9')) {
	    this |= (this & 0x0F) | ((*data - '0') << bit);
	} else if(*data == '\n') {
	    continue;
	} else {
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	if(bit == 4) {
	    bit = 0;
	} else {
	    bit = 4;
	    addtobuf(buf, this);
	    this = 0;
	}
    }
    if(bit != 4) {
	if(buf != NULL)
	    free(buf);
	return(NULL);
    }
    addtobuf(buf, 0);
    if(len != NULL)
	*len = bufdata - 1;
    return(buf);
}

char *base64encode(char *data, size_t datalen)
{
    char *buf;
    size_t bufsize, bufdata;
    
    if(datalen == 0)
	return(sstrdup(""));
    buf = NULL;
    bufsize = bufdata = 0;
    while(datalen >= 3)
    {
	addtobuf(buf, base64set[(data[0] & 0xfc) >> 2]);
	addtobuf(buf, base64set[((data[0] & 0x03) << 4) | ((data[1] & 0xf0) >> 4)]);
	addtobuf(buf, base64set[((data[1] & 0x0f) << 2) | ((data[2] & 0xc0) >> 6)]);
	addtobuf(buf, base64set[data[2] & 0x3f]);
	datalen -= 3;
	data += 3;
    }
    if(datalen == 1)
    {
	addtobuf(buf, base64set[(data[0] & 0xfc) >> 2]);
	addtobuf(buf, base64set[(data[0] & 0x03) << 4]);
	bufcat(buf, "==", 2);
    }
    if(datalen == 2)
    {
	addtobuf(buf, base64set[(data[0] & 0xfc) >> 2]);
	addtobuf(buf, base64set[((data[0] & 0x03) << 4) | ((data[1] & 0xf0) >> 4)]);
	addtobuf(buf, base64set[(data[1] & 0x0f) << 2]);
	addtobuf(buf, '=');
    }
    addtobuf(buf, 0);
    return(buf);
}

char *base64decode(char *data, size_t *datalen)
{
    int b, c;
    char *buf, cur;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    cur = 0;
    b = 8;
    for(; *data > 0; data++)
    {
	c = (int)(unsigned char)*data;
	if(c == '=')
	    break;
	if(c == '\n')
	    continue;
	if(base64rev[c] == -1)
	{
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	b -= 6;
	if(b <= 0)
	{
	    cur |= base64rev[c] >> -b;
	    addtobuf(buf, cur);
	    b += 8;
	    cur = 0;
	}
	cur |= base64rev[c] << b;
    }
    if(datalen != NULL)
	*datalen = bufdata;
    addtobuf(buf, 0);
    return(buf);
}

char *base32encode(char *data, size_t datalen)
{
    char *buf;
    size_t bufsize, bufdata;
    
    if(datalen == 0)
	return(sstrdup(""));
    buf = NULL;
    bufsize = bufdata = 0;
    while(datalen >= 5)
    {
	addtobuf(buf, base32set[((data[0] & 0xf8) >> 3)]);
	addtobuf(buf, base32set[((data[0] & 0x07) << 2) | ((data[1] & 0xc0) >> 6)]);
	addtobuf(buf, base32set[((data[1] & 0x3e) >> 1)]);
	addtobuf(buf, base32set[((data[1] & 0x01) << 4) | ((data[2] & 0xf0) >> 4)]);
	addtobuf(buf, base32set[((data[2] & 0x0f) << 1) | ((data[3] & 0x80) >> 7)]);
	addtobuf(buf, base32set[((data[3] & 0x7c) >> 2)]);
	addtobuf(buf, base32set[((data[3] & 0x03) << 3) | ((data[4] & 0xe0) >> 5)]);
	addtobuf(buf, base32set[data[4] & 0x1f]);
	datalen -= 5;
	data += 5;
    }
    if(datalen == 1)
    {
	addtobuf(buf, base32set[((data[0] & 0xf8) >> 3)]);
	addtobuf(buf, base32set[((data[0] & 0x07) << 2)]);
	bufcat(buf, "======", 6);
    }
    if(datalen == 2)
    {
	addtobuf(buf, base32set[((data[0] & 0xf8) >> 3)]);
	addtobuf(buf, base32set[((data[0] & 0x07) << 2) | ((data[1] & 0xc0) >> 6)]);
	addtobuf(buf, base32set[((data[1] & 0x3e) >> 1)]);
	addtobuf(buf, base32set[((data[1] & 0x01) << 4)]);
	bufcat(buf, "====", 4);
    }
    if(datalen == 3)
    {
	addtobuf(buf, base32set[((data[0] & 0xf8) >> 3)]);
	addtobuf(buf, base32set[((data[0] & 0x07) << 2) | ((data[1] & 0xc0) >> 6)]);
	addtobuf(buf, base32set[((data[1] & 0x3e) >> 1)]);
	addtobuf(buf, base32set[((data[1] & 0x01) << 4) | ((data[2] & 0xf0) >> 4)]);
	addtobuf(buf, base32set[((data[2] & 0x0f) << 1)]);
	bufcat(buf, "===", 3);
    }
    if(datalen == 4)
    {
	addtobuf(buf, base32set[((data[0] & 0xf8) >> 3)]);
	addtobuf(buf, base32set[((data[0] & 0x07) << 2) | ((data[1] & 0xc0) >> 6)]);
	addtobuf(buf, base32set[((data[1] & 0x3e) >> 1)]);
	addtobuf(buf, base32set[((data[1] & 0x01) << 4) | ((data[2] & 0xf0) >> 4)]);
	addtobuf(buf, base32set[((data[2] & 0x0f) << 1) | ((data[3] & 0x80) >> 7)]);
	addtobuf(buf, base32set[((data[3] & 0x7c) >> 2)]);
	addtobuf(buf, base32set[((data[3] & 0x03) << 3)]);
	bufcat(buf, "=", 1);
    }
    addtobuf(buf, 0);
    return(buf);
}

char *base32decode(char *data, size_t *datalen)
{
    int b, c;
    char *buf, cur;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    cur = 0;
    b = 8;
    for(; *data > 0; data++)
    {
	c = (int)(unsigned char)*data;
	if(c == '=')
	    break;
	if(c == '\n')
	    continue;
	if(base32rev[c] == -1)
	{
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	b -= 5;
	if(b <= 0)
	{
	    cur |= base32rev[c] >> -b;
	    addtobuf(buf, cur);
	    b += 8;
	    cur = 0;
	}
	cur |= base32rev[c] << b;
    }
    if(datalen != NULL)
	*datalen = bufdata;
    addtobuf(buf, 0);
    return(buf);
}

void _freeparr(void **arr)
{
    void **buf;
    
    if(arr == NULL)
	return;
    for(buf = arr; *buf != NULL; buf++)
	free(*buf);
    free(arr);
}

int _parrlen(void **arr)
{
    int i;
    
    if(arr == NULL)
	return(0);
    for(i = 0; *arr != NULL; arr++)
	i++;
    return(i);
}

char *getetcpath(char *binpath)
{
    int f;
    char *etcpath, *p;
    size_t etcpathsize, etcpathdata;

    etcpath = NULL;
    etcpathsize = etcpathdata = 0;
    f = 1;
    do
    {
	if(f)
	    f = 0;
	else
	    binpath++;
	for(p = binpath; *p && (*p != ':'); p++);
	for(; (p >= binpath) && (*p != '/'); p--);
	if(p >= binpath)
	{
	    if(etcpathdata > 0)
		addtobuf(etcpath, ':');
	    bufcat(etcpath, binpath, p - binpath + 1);
	    bufcat(etcpath, "etc", 3);
	}
    } while((binpath = strchr(binpath, ':')) != NULL);
    addtobuf(etcpath, 0);
    return(etcpath);
}

char *findfile(char *name, char *homedir, int filldef)
{
    char *path, *binpath, *etcpath, *p;
    struct passwd *pw;
    int mode, homeonly;
    
    if(name == NULL)
	return(NULL);

    mode = R_OK | (filldef ? W_OK : 0);
    homeonly = homedir != NULL;

    if(!strchr(name, '/'))
    {
	if(homedir == NULL)
	    homedir = getenv("HOME");
	if((homedir == NULL) && ((pw = getpwuid(getuid())) != NULL))
	    homedir = pw->pw_dir;
	if((homedir != NULL) && ((path = sprintf2("%s/.%s", homedir, name)) != NULL))
	{
	    if(!access(path, mode))
		return(path);
	    free(path);
	}
    }
    
    if(!homeonly)
    {
	if(strchr(name, '/') != NULL)
	{
	    if(!access(name, mode))
		return(sstrdup(name));
	} else {
	    if((binpath = getenv("PATH")) == NULL)
		etcpath = sstrdup("/usr/local/etc:/etc:/usr/etc");
	    else
		etcpath = getetcpath(binpath);
	    for(p = strtok(etcpath, ":"); p != NULL; p = strtok(NULL, ":"))
	    {
		if((path = sprintf2("%s/%s", p, name)) != NULL)
		{
		    if(!access(path, mode))
		    {
			free(etcpath);
			return(path);
		    }
		    free(path);
		}
	    }
	    free(etcpath);
	}
    }
    
    if(filldef) {
	if(homedir)
	    return(sprintf2("%s/.%s", homedir, name));
	return(sprintf2("/etc/%s", name));
    } else {
	return(NULL);
    }
}

struct strpair *newstrpair(char *key, char *val, struct strpair **list)
{
    struct strpair *pair;
    
    pair = smalloc(sizeof(*pair));
    memset(pair, 0, sizeof(*pair));
    if(key != NULL)
	pair->key = sstrdup(key);
    if(val != NULL)
	pair->val = sstrdup(val);
    if(list != NULL)
    {
	pair->next = *list;
	*list = pair;
    }
    return(pair);
}

void freestrpair(struct strpair *pair, struct strpair **list)
{
    struct strpair *cur;
    
    for(cur = *list; cur != NULL; list = &(cur->next), cur = cur->next)
    {
	if(cur == pair)
	{
	    *list = cur->next;
	    break;
	}
    }
    free(pair->key);
    free(pair->val);
    free(pair);
}

char *spfind(struct strpair *list, char *key)
{
    for(; list != NULL; list = list->next)
    {
	if(!strcmp(list->key, key))
	    return(list->val);
    }
    return(NULL);
}

struct wcspair *newwcspair(wchar_t *key, wchar_t *val, struct wcspair **list)
{
    struct wcspair *pair;
    
    pair = smalloc(sizeof(*pair));
    memset(pair, 0, sizeof(*pair));
    if(key != NULL)
	pair->key = swcsdup(key);
    if(val != NULL)
	pair->val = swcsdup(val);
    if(list != NULL)
    {
	pair->next = *list;
	*list = pair;
    }
    return(pair);
}

void freewcspair(struct wcspair *pair, struct wcspair **list)
{
    struct wcspair *cur;
    
    for(cur = *list; cur != NULL; list = &(cur->next), cur = cur->next)
    {
	if(cur == pair)
	{
	    *list = cur->next;
	    break;
	}
    }
    free(pair->key);
    free(pair->val);
    free(pair);
}

wchar_t *wpfind(struct wcspair *list, wchar_t *key)
{
    for(; list != NULL; list = list->next)
    {
	if(!wcscmp(list->key, key))
	    return(list->val);
    }
    return(NULL);
}

static int btheight(struct btree *tree)
{
    if(tree == NULL)
	return(0);
    return(tree->h);
}

static void btsetheight(struct btree *tree)
{
    int lh, rh;
    
    if(tree == NULL)
	return;
    lh = btheight(tree->l);
    rh = btheight(tree->r);
    tree->h = ((lh > rh)?lh:rh) + 1;
}

static void bbtrl(struct btree **tree);

static void bbtrr(struct btree **tree)
{
    struct btree *m, *l, *r;
    
    if(btheight((*tree)->l->r) > btheight((*tree)->l->l))
	bbtrl(&(*tree)->l);
    r = (*tree);
    l = r->l;
    m = l->r;
    r->l = m;
    btsetheight(r);
    l->r = r;
    btsetheight(l);
    *tree = l;
}

static void bbtrl(struct btree **tree)
{
    struct btree *m, *l, *r;
    
    if(btheight((*tree)->r->l) > btheight((*tree)->r->r))
	bbtrr(&(*tree)->r);
    l = (*tree);
    r = l->r;
    m = r->l;
    l->r = m;
    btsetheight(l);
    r->l = l;
    btsetheight(r);
    *tree = r;
}

int bbtreedel(struct btree **tree, void *item, int (*cmp)(void *, void *))
{
    int c, r;
    struct btree *s, **sp, *o;

    if(*tree == NULL)
	return(0);
    if((c = cmp(item, (*tree)->d)) < 0) {
	r = bbtreedel(&(*tree)->l, item, cmp);
    } else if(c > 0) {
	r = bbtreedel(&(*tree)->r, item, cmp);
    } else {
	r = 1;
	o = *tree;
	if(((*tree)->r != NULL) && ((*tree)->l != NULL)) {
	    sp = &(*tree)->r;
	    s = *sp;
	    while(s->l != NULL) {
		sp = &(s->l);
		s = *sp;
	    }
	    *sp = NULL;
	    s->l = (*tree)->l;
	    s->r = (*tree)->r;
	    *tree = s;
	} else if((*tree)->l != NULL) {
	    *tree = (*tree)->l;
	} else if((*tree)->r != NULL) {
	    *tree = (*tree)->r;
	} else {
	    *tree = NULL;
	}
	free(o);
    }
    if(*tree != NULL) {
	btsetheight(*tree);
	if(btheight((*tree)->l) > btheight((*tree)->r) + 1)
	    bbtrr(tree);
	if(btheight((*tree)->r) > btheight((*tree)->l) + 1)
	    bbtrl(tree);
    }
    return(r);
}

int bbtreeput(struct btree **tree, void *item, int (*cmp)(void *, void *))
{
    int c, r;
    
    if(*tree == NULL) {
	*tree = smalloc(sizeof(**tree));
	(*tree)->l = (*tree)->r = NULL;
	(*tree)->d = item;
	(*tree)->h = 1;
	return(1);
    }
    if((c = cmp(item, (*tree)->d)) < 0)
	r = bbtreeput(&(*tree)->l, item, cmp);
    else if(c > 0)
	r = bbtreeput(&(*tree)->r, item, cmp);
    else
	return(0);
    btsetheight(*tree);
    if(btheight((*tree)->l) > btheight((*tree)->r) + 1)
	bbtrr(tree);
    if(btheight((*tree)->r) > btheight((*tree)->l) + 1)
	bbtrl(tree);
    return(r);
}

void *btreeget(struct btree *tree, void *key, int (*cmp)(void *, void *))
{
    int c;
    
    while(1) {
	if(tree == NULL)
	    return(NULL);
	c = cmp(key, tree->d);
	if(c < 0)
	    tree = tree->l;
	else if(c > 0)
	    tree = tree->r;
	else
	    return(tree->d);
    }
}

void btreefree(struct btree *tree)
{
    if(tree == NULL)
	return;
    btreefree(tree->l);
    btreefree(tree->r);
    free(tree);
}

static void *btreenext(void **iterp)
{
    struct treeiter *iter;
    int s;
    struct btree *n;
    
    if((iter = *iterp) == NULL)
	return(NULL);
    while(1) {
	s = iter->st[iter->sp].s;
	n = iter->st[iter->sp].n;
	if(s == 0) {
	    iter->st[iter->sp].s = 1;
	    if(n->l != NULL) {
		iter->sp++;
		sizebuf2(iter->st, iter->sp + 1, 1);
		iter->st[iter->sp].s = 0;
		iter->st[iter->sp].n = n->l;
	    }
	} else if(s == 1) {
	    iter->st[iter->sp].s = 2;
	    return(iter->st[iter->sp].n->d);
	} else if(s == 2) {
	    iter->st[iter->sp].s = 3;
	    if(n->r != NULL) {
		iter->sp++;
		sizebuf2(iter->st, iter->sp + 1, 1);
		iter->st[iter->sp].s = 0;
		iter->st[iter->sp].n = n->r;
	    }
	} else {
	    iter->sp--;
	    if(iter->sp < 0) {
		free(iter->st);
		free(iter);
		*iterp = NULL;
		return(NULL);
	    }
	}
    }
}

static void *btreefirst(struct btree *tree, void **iterp)
{
    struct treeiter *iter;
    
    if(tree == NULL) {
	*iterp = NULL;
	return(NULL);
    }
    *iterp = iter = memset(smalloc(sizeof(*iter)), 0, sizeof(*iter));
    sizebuf2(iter->st, 1, 1);
    iter->st[0].n = tree;
    iter->st[0].s = 0;
    return(btreenext(iterp));
}

static void btreestop(void **iterp)
{
    struct treeiter *iter;
    
    if((iter = *iterp) == NULL)
	return;
    if(iter->st != NULL)
	free(iter->st);
    free(iter);
    *iterp = NULL;
}

void *btreeiter(struct btree *tree)
{
    static void *iter = NULL;
    
    if(tree == NULL) {
	return(btreenext(&iter));
    } else {
	if(iter != NULL)
	    btreestop(&iter);
	return(btreefirst(tree, &iter));
    }
}
