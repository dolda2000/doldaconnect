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

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <wchar.h>
#include <iconv.h>
#include <errno.h>
#include <string.h>
#include <wctype.h>
#include <langinfo.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <doldaconnect/utils.h>

extern int vswprintf (wchar_t *__restrict __s, size_t __n,
		      __const wchar_t *__restrict __format,
		      __gnuc_va_list __arg);

char *vsprintf2(char *format, va_list al)
{
    int ret;
    char *buf;
    
    ret = vsnprintf(NULL, 0, format, al);
    if((buf = malloc(ret + 1)) == NULL)
    {
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
	return(NULL);
    }
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open("wchar_t", charset)) == (iconv_t)-1)
    {
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
		    free(buf);
		    return(NULL);
		}
		buf = p2;
		p = buf + data;
	    } else {
		free(buf);
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
	return(def);
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
	return(NULL);
    }
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open(charset, "wchar_t")) == (iconv_t)-1)
    {
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
		    free(buf);
		    return(NULL);
		}
		buf = p2;
		p = buf + data;
	    } else {
		free(buf);
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
	return(def);
    return(buf);
}

wchar_t *wcstolower(wchar_t *wcs)
{
    wchar_t *p;
    
    for(p = wcs; *p != L'\0'; p++)
	*p = towlower(*p);
    return(wcs);
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
	    *bufsize *= 2;
	*buf = srealloc(*buf, elsize * (*bufsize));
	break;
    }
}
