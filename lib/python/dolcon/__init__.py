#  Dolda Connect - Modular multiuser Direct Connect-style client
#  Copyright (C) 2007 Fredrik Tolf <fredrik@dolda2000.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from dolmod import *
import os

def login(useauthless = True, **kw):
    """A convenience function for loginasync.

    This function will initiate an asynchronous login per the
    loginasync command, and then run a select loop while waiting for
    it to complete. It will return a tuple (res, reason), where res is
    the result code, and reason is an explanatory text for any error.

    res can be any of the following:
     * success:  Login completed successfully
     * nologin:  No authentication mechanism could be negotiated
     * server:   An error occurred on the server
     * user:     An error occurred in the library
     * conv:     The password conversation mechanism failed
     * authfail: The server refused the login (due to e.g. bad credentials)
    """
    result = [None]
    def mycb(*v):
        result[0] = v
    loginasync(mycb, useauthless, **kw)
    while result[0] is None:
        select()
    return result[0]

def mustconnect(host, revision = latest):
    """A convenience function for connect.

    This function will connect to the given host, perform a select
    loop, and ensure that the server approves of the connection. If
    any of these steps fail, an exception is raised. If successful,
    the file descriptor for the server connection is returned.
    """
    if host is None:
        fd = connect()
    else:
        fd = connect(host)
    while True:
        resp = getresp()
        if resp is not None and resp.getcmd() == u".connect":
            break
        select()
    if resp.getcode() != 201:
        raise RuntimeError, resp.intresp()[0][0]
    if not checkproto(resp, revision):
        raise RuntimeError, resp
    return fd

def cnl(host = None, useauthless = True, revision = latest, **kw):
    """A convenience function for connect and loginasync.

    This function will connect to the given server, or try the default
    servers if none given, and authenticate to the server. If any of
    the steps fail, an exception is raised.
    """
    fd = mustconnect(host, revision)
    err, reason = login(useauthless, **kw)
    if err != "success":
        raise RuntimeError, (err, reason)
    return fd
    
def ecmd(*args):
    """A convenience function for qcmd.

    This function will queue the given command, and then wait in a
    select loop until the command has been carried out. The return
    value is a Response object, corresponding to the reponse from the
    server.
    """
    tag = qcmd(*args)
    while True:
        resp = getresp(tag)
        if resp is not None:
            break;
        select()
    return resp

def ecmda(code, *args):
    """A convenience function for ecmd.

    This function does essentially the same as ecmd, but it will also
    check so that the response has the given numerical code. If not,
    an exception is raised.
    """
    resp = ecmd(*args)
    if resp.getcode() != code:
        raise ValueError, resp.getcode()
    return resp

def ecmds(*args):
    """Another convenience function for ecmd.

    Like ecmda, but will fail on all 5xx codes, and succeed on all
    others.
    """
    resp = ecmd(*args)
    if resp.getcode() >= 500 and resp.getcode() < 600:
        raise ValueError, tuple(resp.extract()[0])
    return resp

def getresps():
    """A generator function which will iterate over all responses from
    getresp.
    """
    while True:
        resp = getresp()
        if resp is None:
            break
        else:
            yield resp
