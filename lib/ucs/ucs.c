/*
 *  This file is part of Dolda Connect
 *  Copyright (C) 2008 Fredrik Tolf <fredrik@dolda2000.com>
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

#include <ucs.h>
#include <string.h>
#include <utils.h>
#include <errno.h>
#include <endian.h>
#include <iconv.h>
#include <langinfo.h>

#if BYTE_ORDER == BIG_ENDIAN
#define MYCHARSET "UCS-4BE"
#elif BYTE_ORDER == LITTLE_ENDIAN
#define MYCHARSET "UCS-4LE"
#else
#error "Endian is neither big nor little"
#endif

uchar_t *asctoucs(char *ascii)
{
    int i;
    size_t l;
    uchar_t *ret;
    
    l = strlen(ascii);
    ret = smalloc((l + 1) * sizeof(*ret));
    for(i = 0; i < l; i++) {
	if((ret[i] = ascii[i]) >= 128) {
	    free(ret);
	    errno = EILSEQ;
	    return(NULL);
	}
    }
    return(ret);
}

uchar_t *asctosucs(char *ascii)
{
    static uchar_t *ring[16];
    static int cur;
    
    if(ring[cur] != NULL)
	free(ring[cur]);
    if((ring[cur] = asctoucs(ascii)) == NULL)
	return(NULL);
    return(ring[cur++]);
}

uchar_t *mbstoucs(char *mbs, char *charset)
{
    int ret;
    char *buf;
    char *p, *p2;
    size_t len1, len2, bufsize, data;
    iconv_t cd;
    
    len1 = strlen(mbs) + 1;
    bufsize = len2 = len1 * sizeof(wchar_t);
    if((buf = malloc(bufsize)) == NULL)
	return(NULL);
    if(charset == NULL)
	charset = nl_langinfo(CODESET);
    if((cd = iconv_open(MYCHARSET, charset)) == (iconv_t)-1) {
	free(buf);
	return(NULL);
    }
    p = buf;
    while(len1 > 0) {
	ret = iconv(cd, &mbs, &len1, &p, &len2);
	if(ret < 0) {
	    if(errno == E2BIG) {
		data = p - buf;
		len2 += 128;
		bufsize += 128;
		if((p2 = realloc(buf, bufsize)) == NULL)
		{
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
    return((uchar_t *)buf);
}
