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
#include <Python.h>
#include <sys/poll.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>
#include <wchar.h>

struct respobj {
    PyObject_HEAD
    struct dc_response *resp;
};

static int fd = -1;

static void resptype_del(struct respobj *self)
{
    dc_freeresp(self->resp);
    self->ob_type->tp_free((PyObject *)self);
}

static PyObject *resp_getcode(struct respobj *self)
{
    return(Py_BuildValue("i", self->resp->code));
}

static PyObject *resp_gettag(struct respobj *self)
{
    return(Py_BuildValue("i", self->resp->tag));
}

static PyObject *resp_getcmd(struct respobj *self)
{
    return(PyUnicode_FromWideChar(self->resp->cmdname, wcslen(self->resp->cmdname)));
}

static PyObject *resp_extract(struct respobj *self)
{
    int i, o;
    PyObject *ret, *sl;
    wchar_t *c;
    
    ret = PyList_New(self->resp->numlines);
    for(i = 0; i < self->resp->numlines; i++) {
	sl = PyList_New(self->resp->rlines[i].argc);
	for(o = 0; o < self->resp->rlines[i].argc; o++) {
	    c = self->resp->rlines[i].argv[o];
	    PyList_SetItem(sl, o, PyUnicode_FromWideChar(c, wcslen(c)));
	}
	PyList_SetItem(ret, i, sl);
    }
    return(ret);
}

static PyObject *resp_intresp(struct respobj *self)
{
    int i;
    PyObject *ret, *sl;
    struct dc_intresp *ires;
    
    ret = PyList_New(0);
    self->resp->curline = 0;
    while((ires = dc_interpret(self->resp)) != NULL) {
	sl = PyList_New(ires->argc);
	for(i = 0; i < ires->argc; i++) {
	    switch(ires->argv[i].type) {
	    case 1:
		PyList_SetItem(sl, i, PyUnicode_FromWideChar(ires->argv[i].val.str, wcslen(ires->argv[i].val.str)));
		break;
	    case 2:
		PyList_SetItem(sl, i, PyInt_FromLong(ires->argv[i].val.num));
		break;
	    case 3:
		PyList_SetItem(sl, i, PyFloat_FromDouble(ires->argv[i].val.flnum));
		break;
	    case 4:
		PyList_SetItem(sl, i, PyLong_FromLongLong(ires->argv[i].val.lnum));
		break;
	    }
	}
	dc_freeires(ires);
	PyList_Append(ret, sl);
    }
    return(ret);
}

static PyMethodDef respmethods[] = {
    {"getcode", (PyCFunction)resp_getcode, METH_NOARGS,
     "Get the numerical code from the response"},
    {"gettag", (PyCFunction)resp_gettag, METH_NOARGS,
     "Get the tag from the response"},
    {"getcmd", (PyCFunction)resp_getcmd, METH_NOARGS,
     "Get the command name from the response"},
    {"extract", (PyCFunction)resp_extract, METH_NOARGS,
     "Extract the lines and columns from the response"},
    {"intresp", (PyCFunction)resp_intresp, METH_NOARGS,
     "Interpret all the lines from the response with native datatypes"},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject resptype = {
    PyObject_HEAD_INIT(NULL)
    .tp_name = "dolcon.Response",
    .tp_basicsize = sizeof(struct respobj),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_doc = "Dolda Connect response objects",
    .tp_dealloc = (destructor)resptype_del,
    .tp_methods = respmethods,
};

static struct respobj *makeresp(struct dc_response *resp)
{
    struct respobj *new;
    
    new = (struct respobj *)resptype.tp_alloc(&resptype, 0);
    new->resp = resp;
    return(new);
}

static PyObject *mod_connect(PyObject *self, PyObject *args)
{
    char *host;
    
    host = NULL;
    if(!PyArg_ParseTuple(args, "|s", &host))
	return(NULL);
    if(fd >= 0)
	dc_disconnect();
    if((fd = dc_connect(host)) < 0) {
	PyErr_SetFromErrno(PyExc_OSError);
	return(NULL);
    }
    return(Py_BuildValue("i", fd));
}

static PyObject *mod_disconnect(PyObject *self, PyObject *args)
{
    if(fd != -1) {
	dc_disconnect();
	fd = -1;
    }
    Py_RETURN_NONE;
}

static PyObject *mod_connected(PyObject *self, PyObject *args)
{
    if(fd == -1)
	Py_RETURN_FALSE;
    else
	Py_RETURN_TRUE;
}

static PyObject *mod_select(PyObject *self, PyObject *args)
{
    struct pollfd pfd;
    int timeout, ret;
    
    timeout = -1;
    if(!PyArg_ParseTuple(args, "|i", &timeout))
	return(NULL);
    if(fd < 0) {
	PyErr_SetString(PyExc_RuntimeError, "Not connected");
	return(NULL);
    }
    pfd.fd = fd;
    pfd.events = POLLIN;
    if(dc_wantwrite())
	pfd.events |= POLLOUT;
    if((ret = poll(&pfd, 1, timeout)) < 0) {
	if(errno == EINTR)
	    Py_RETURN_FALSE;
	dc_disconnect();
	fd = -1;
	PyErr_SetFromErrno(PyExc_OSError);
	return(NULL);
    }
    if(((pfd.revents & POLLIN) && dc_handleread()) || ((pfd.revents & POLLOUT) && dc_handlewrite())) {
	fd = -1;
	if(errno == 0)
	    Py_RETURN_FALSE;
	PyErr_SetFromErrno(PyExc_OSError);
	return(NULL);
    }
    if(ret > 0)
	Py_RETURN_TRUE;
    Py_RETURN_FALSE;
}

static PyObject *mod_getresp(PyObject *self, PyObject *args)
{
    int tag;
    struct dc_response *resp;
    
    tag = -1;
    if(!PyArg_ParseTuple(args, "|i", &tag))
	return(NULL);
    if(tag == -1)
	resp = dc_getresp();
    else
	resp = dc_gettaggedresp(tag);
    if(resp == NULL)
	Py_RETURN_NONE;
    return((PyObject *)makeresp(resp));
}

static int qcmd_cb(struct dc_response *resp)
{
    PyObject *pycb, *args, *ret;
    
    pycb = resp->data;
    args = Py_BuildValue("(N)", makeresp(resp));
    ret = PyObject_Call(pycb, args, NULL);
    Py_DECREF(args);
    Py_DECREF(ret);
    Py_DECREF(pycb);
    return(2);
}

static PyObject *mod_qcmd(PyObject *self, PyObject *args, PyObject *kwargs)
{
    int i, tag;
    wchar_t **toks, *tok, *cmd;
    size_t tokssize, toksdata, toksize;
    PyObject *c, *n, *cb, *ret;
    
    toks = NULL;
    tokssize = toksdata = 0;
    cmd = NULL;
    ret = NULL;
    for(i = 0; i < PySequence_Size(args); i++) {
	if((c = PySequence_GetItem(args, i)) == NULL)
	    goto out;
	if(!PyUnicode_Check(c)) {
	    n = PyUnicode_FromObject(c);
	    Py_DECREF(c);
	    if((c = n) == NULL)
		goto out;
	}
	tok = smalloc((toksize = (PyUnicode_GetSize(c) + 1)) * sizeof(*tok));
	tok[PyUnicode_AsWideChar((PyUnicodeObject *)c, tok, toksize)] = L'\0';
	Py_DECREF(c);
	if(cmd == NULL)
	    cmd = tok;
	else
	    addtobuf(toks, tok);
    }
    if(cmd == NULL) {
	PyErr_SetString(PyExc_TypeError, "qcmd needs at least 1 argument");
	goto out;
    }
    addtobuf(toks, NULL);
    ret = NULL;
    if(PyMapping_HasKeyString(kwargs, "cb")) {
	cb = PyMapping_GetItemString(kwargs, "cb");
	if(PyCallable_Check(cb)) {
	    ret = PyInt_FromLong(dc_queuecmd(qcmd_cb, cb, cmd, L"%a", toks, NULL));
	} else {
	    PyErr_SetString(PyExc_TypeError, "Callback must be callable");
	    Py_DECREF(cb);
	}
    } else {
	ret = PyInt_FromLong(dc_queuecmd(NULL, NULL, cmd, L"%a", toks, NULL));
    }

out:
    dc_freewcsarr(toks);
    if(cmd != NULL)
	free(cmd);
    return(ret);
}

static void login_cb(int err, wchar_t *reason, PyObject *cb)
{
    char *errstr;
    PyObject *args, *pyerr, *pyreason, *ret;
    
    switch(err) {
    case DC_LOGIN_ERR_SUCCESS:
	errstr = "success";
	break;
    case DC_LOGIN_ERR_NOLOGIN:
	errstr = "nologin";
	break;
    case DC_LOGIN_ERR_SERVER:
	errstr = "server";
	break;
    case DC_LOGIN_ERR_USER:
	errstr = "user";
	break;
    case DC_LOGIN_ERR_CONV:
	errstr = "conv";
	break;
    case DC_LOGIN_ERR_AUTHFAIL:
	errstr = "authfail";
	break;
    }
    pyerr = PyString_FromString(errstr);
    if(reason == NULL)
	Py_INCREF(pyreason = Py_None);
    else
	pyreason = PyUnicode_FromWideChar(reason, wcslen(reason));
    args = PyTuple_Pack(2, pyerr, pyreason);
    Py_DECREF(pyerr); Py_DECREF(pyreason);
    ret = PyObject_Call(cb, args, NULL);
    Py_DECREF(cb);
    Py_DECREF(args);
    Py_DECREF(ret);
}

static PyObject *mod_loginasync(PyObject *self, PyObject *args, PyObject *kwargs)
{
    int useauthless;
    char *username;
    PyObject *o, *cb, *conv;
    
    username = NULL;
    conv = NULL;
    useauthless = 1;
    if(!PyArg_ParseTuple(args, "O|i", &cb, &useauthless))
	return(NULL);
    if(!PyCallable_Check(cb)) {
	PyErr_SetString(PyExc_TypeError, "Callback must be callable");
	return(NULL);
    }
    if(PyMapping_HasKeyString(kwargs, "username")) {
	o = PyMapping_GetItemString(kwargs, "username");
	username = PyString_AsString(o);
	Py_DECREF(o);
	if(username == NULL)
	    return(NULL);
    }
    if(PyMapping_HasKeyString(kwargs, "conv")) {
	conv = PyMapping_GetItemString(kwargs, "conv");
	if(!PyCallable_Check(conv)) {
	    PyErr_SetString(PyExc_TypeError, "Conv callback must be callable");
	    return(NULL);
	}
	PyErr_SetString(PyExc_NotImplementedError, "Custom conv functions are not yet supported by the Python interface");
	return(NULL);
    }
    Py_INCREF(cb);
    dc_loginasync(username, useauthless, NULL, (void (*)(int, wchar_t *, void *))login_cb, cb);
    Py_RETURN_NONE;
}

static PyObject *mod_lexsexpr(PyObject *self, PyObject *args)
{
    PyObject *arg, *se, *ret;
    wchar_t **arr, **ap, *buf;
    size_t bufsize;
    
    if(!PyArg_ParseTuple(args, "O", &arg))
	return(NULL);
    if((se = PyUnicode_FromObject(arg)) == NULL)
	return(NULL);
    buf = smalloc((bufsize = (PyUnicode_GetSize(se) + 1)) * sizeof(*buf));
    buf[PyUnicode_AsWideChar((PyUnicodeObject *)se, buf, bufsize)] = L'\0';
    arr = dc_lexsexpr(buf);
    free(buf);
    Py_DECREF(se);
    ret = PyList_New(0);
    if(arr != NULL) {
	for(ap = arr; *ap; ap++)
	    PyList_Append(ret, PyUnicode_FromWideChar(*ap, wcslen(*ap)));
	dc_freewcsarr(arr);
    }
    return(ret);
}

static PyObject *mod_wantwrite(PyObject *self)
{
    if(dc_wantwrite())
	Py_RETURN_TRUE;
    else
	Py_RETURN_FALSE;
}

static PyObject *mod_checkproto(PyObject *self, PyObject *args)
{
    PyObject *tmp;
    struct respobj *resp;
    int version;
    
    version = DC_LATEST;
    if(!PyArg_ParseTuple(args, "O|i", &tmp, &version))
	return(NULL);
    if(!PyObject_TypeCheck(tmp, &resptype)) {
	PyErr_SetString(PyExc_TypeError, "first argument must be a response object");
	return(NULL);
    }
    resp = (struct respobj *)tmp;
    if(dc_checkprotocol(resp->resp, version))
	Py_RETURN_FALSE;
    else
	Py_RETURN_TRUE;
}

static PyMethodDef methods[] = {
    {"connect", mod_connect, METH_VARARGS,
     "Connect to a Dolda Connect server"},
    {"disconnect", mod_disconnect, METH_VARARGS,
     "Disconnect from the server"},
    {"connected", mod_connected, METH_VARARGS,
     "Return a boolean indicated whether there currently is a connection to a server"},
    {"select", mod_select, METH_VARARGS,
     "Handle data from the server connection, optionally blocking until something happens"},
    {"getresp", mod_getresp, METH_VARARGS,
     "Get a queued response object"},
    {"qcmd", (PyCFunction)mod_qcmd, METH_VARARGS | METH_KEYWORDS,
     "Queue a command to be sent to the server"},
    {"loginasync", (PyCFunction)mod_loginasync, METH_VARARGS | METH_KEYWORDS,
     "Perform an asynchronous login procedure"},
    {"lexsexpr", mod_lexsexpr, METH_VARARGS,
     "Use a standard algorithm to lex a search expression"},
    {"wantwrite", (PyCFunction)mod_wantwrite, METH_NOARGS,
     "Return a boolean indicating whether there is output to be fed to the server"},
    {"checkproto", (PyCFunction)mod_checkproto, METH_VARARGS,
     "Check so that the connect stanza returned by the server indicates support for the correct revision of the protocol"},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initdolmod(void)
{
    PyObject *m;
    
    if(PyType_Ready(&resptype) < 0)
	return;
    m = Py_InitModule("dolmod", methods);
    Py_INCREF(&resptype);
    PyModule_AddObject(m, "Response", (PyObject *)&resptype);
    PyModule_AddObject(m, "latest", Py_BuildValue("i", DC_LATEST));
    dc_init();
}
