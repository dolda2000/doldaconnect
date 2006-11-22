from dolmod import *

def login(useauthless = True, **kw):
    result = [None]
    def mycb(*v):
        result[0] = v
    loginasync(mycb, useauthless, **kw)
    while result[0] is None:
        select()
    return result[0]

def mustconnect(host, port = -1):
    connect(host, port)
    while True:
        resp = getresp()
        if resp is not None and resp.getcmd() == u".connect":
            break
        select()
    if resp.getcode() != 200:
        raise RuntimeError, resp.intresp()[0][0]

def cnl(host, port = -1, useauthless = True, **kw):
    mustconnect(host, port)
    err, reason = login(useauthless, **kw)
    if err != "success":
        raise RuntimeError, (err, reason)
    
def ecmd(*args):
    tag = qcmd(*args)
    while True:
        resp = getresp(tag)
        if resp is not None:
            break;
        select()
    return resp

def ecmda(code, *args):
    resp = ecmd(*args)
    if resp.getcode() != code:
        raise ValueError, resp.getcode()
    return resp
