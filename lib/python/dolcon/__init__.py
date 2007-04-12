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

    This function will connect to the given server, or the server in
    the environment variable $DCSERVER if none is given, or, if that
    fails, localhost, and authenticate to the server. If any of the
    steps fail, an exception is raised.
    """
    if host is None:
        host = os.getenv("DCSERVER")
    if host is None:
        host = "localhost"
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
