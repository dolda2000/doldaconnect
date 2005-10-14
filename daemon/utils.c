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
#include <malloc.h>
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
#include "utils.h"
#include "log.h"

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
    
    ret = vsnprintf(NULL, 0, format, al);
    if((buf = malloc(ret + 1)) == NULL)
    {
	LOGOOM(ret + 1);
	return(NULL);
    }
    vsnprintf(buf, ret + 1, format, al);
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
    
    buf = smalloc(sizeof(wchar_t) * (bufsize = 1024));
    while((ret = vswprintf(buf, bufsize, format, al)) < 0)
	buf = srealloc(buf, sizeof(wchar_t) * (bufsize *= 2));
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
	flog(LOG_ERR, "icmbstowcs: could not open iconv structure for %s: %s", charset, strerror(errno));
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
	if(*def == '~')
	{
	    flog(LOG_WARNING, "icsmbstowcs: could not convert wcs string into charset %s: %s", charset, strerror(errno));
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
	LOGOOM(bufsize);
	return(NULL);
    }
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open(charset, "wchar_t")) == (iconv_t)-1)
    {
	flog(LOG_ERR, "icwcstombs: could not open iconv structure for %s: %s", charset, strerror(errno));
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
	if(*def == '~')
	{
	    flog(LOG_WARNING, "icswcstombs: could not convert mbs string from charset %s: %s", charset, strerror(errno));
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
	flog(LOG_ERR, "ucptowc: could not open iconv structure for UCS-4BE: %s", strerror(errno));
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

int wcsexists(wchar_t *h, wchar_t *n)
{
    int i, o, nl, hl;
    wchar_t *ln, *lh;
    
    ln = alloca(sizeof(*ln) * (nl = wcslen(n)));
    for(i = 0; i < nl; i++)
	ln[i] = towlower(n[i]);
    lh = alloca(sizeof(*lh) * (hl = wcslen(h)));
    if(nl > hl)
	return(0);
    for(i = 0; i < nl; i++)
	lh[i] = towlower(h[i]);
    i = 0;
    while(1)
    {
	for(o = 0; o < nl; o++)
	{
	    if(lh[i + o] != ln[o])
		break;
	}
	if(o == nl)
	    return(1);
	if(i == hl - nl)
	    return(0);
	lh[i + nl] = towlower(h[i + nl]);
	i++;
    }
}

#ifndef HAVE_WCSCASECMP
int wcscasecmp(const wchar_t *s1, const wchar_t *s2)
{
    while(towlower(*s1) == towlower(*s2))
    {
	if(*s1 == L'\0')
	    return(0);
    }
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
    char *buf, this;
    size_t bufsize, bufdata;
    
    buf = NULL;
    bufsize = bufdata = 0;
    for(; *data; data++)
    {
	if((*data >= 'A') && (*data <= 'F'))
	{
	    this = (this & 0x0F) | ((*data - 'A' + 10) << 4);
	} else if((*data >= '0') && (*data <= '9')) {
	    this = (this & 0x0F) | ((*data - '0') << 4);
	} else {
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	data++;
	if(!*data)
	{
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	if((*data >= 'A') && (*data <= 'F'))
	{
	    this = (this & 0xF0) | (*data - 'A' + 10);
	} else if((*data >= '0') && (*data <= '9')) {
	    this = (this & 0xF0) | (*data - '0');
	} else {
	    if(buf != NULL)
		free(buf);
	    return(NULL);
	}
	addtobuf(buf, this);
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
    char *etcpath, *p;
    size_t etcpathsize, etcpathdata;

    etcpath = NULL;
    etcpathsize = etcpathdata = 0;
    do
    {
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

char *findfile(char *gname, char *uname, char *homedir)
{
    char *path, *binpath, *etcpath, *p;
    
    if((homedir != NULL) && ((path = sprintf2("%s/.%s", homedir, uname)) != NULL))
    {
	if(!access(path, F_OK))
	    return(path);
	free(path);
    }
    if(gname != NULL)
    {
	if(strchr(gname, '/') != NULL)
	{
	    if(!access(gname, F_OK))
		return(sstrdup(gname));
	} else {
	    if((binpath = getenv("PATH")) == NULL)
		etcpath = sstrdup("/usr/local/etc:/etc:/usr/etc");
	    else
		etcpath = getetcpath(binpath);
	    for(p = strtok(etcpath, ":"); p != NULL; p = strtok(NULL, ":"))
	    {
		if((path = sprintf2("%s/%s", p, gname)) != NULL)
		{
		    if(!access(path, F_OK))
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
    return(NULL);
}
