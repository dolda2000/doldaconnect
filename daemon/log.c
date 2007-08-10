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
#include <syslog.h>
#include <stdio.h>
#include <stdarg.h>
#include <sys/uio.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "utils.h"

int logtostderr, logtosyslog;
int syslogfac = LOG_DAEMON;
static int inited = 0;

void flog(int priority, char *format, ...)
{
    va_list args;
    char *b;
    struct iovec iov[2];
    
    va_start(args, format);
    if((b = vsprintf2(format, args)) == NULL)
    {
	if(logtostderr)
	    fputs("No memory available for logging\n", stderr);
	if(logtosyslog)
	    syslog(LOG_CRIT, "No memory available for logging");
    }
    va_end(args);
    if(logtostderr)
    {
	iov[0].iov_base = b;
	iov[0].iov_len = strlen(b);
	iov[1].iov_base = "\n";
	iov[1].iov_len = 1;
	writev(2, iov, 2);
    }
    if(logtosyslog)
	syslog(syslogfac | priority, "%s", b);
    free(b);
}

void initlog(void)
{
    if(inited)
    {
	closelog();
	openlog("doldacond", LOG_PID, syslogfac);
    } else {
	openlog("doldacond", LOG_PID, syslogfac);
	logtostderr = 1;
	logtosyslog = 0;
	inited = 1;
    }
}
