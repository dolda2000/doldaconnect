#include <stdlib.h>
#include <stdio.h>
#include <sys/poll.h>
#include <errno.h>
#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

struct respsmob
{
    struct dc_response *resp;
};

struct scmcb
{
    SCM subr;
};

static int fd = -1;
static scm_t_bits resptype;

static SCM scm_dc_connect(SCM host)
{
    char *chost;
    
    if(fd >= 0)
	dc_disconnect();
    if((host == SCM_UNDEFINED) || (host == SCM_BOOL_F))
    {
	chost = NULL;
    } else {
	SCM_ASSERT(SCM_STRINGP(host), host, SCM_ARG1, "dc-connect");
	chost = SCM_STRING_CHARS(host);
    }
    if((fd = dc_connect(chost)) < 0)
	scm_syserror("dc-connect");
    return(SCM_MAKINUM(fd));
}

static SCM scm_dc_disconnect(void)
{
    dc_disconnect();
    return(SCM_MAKINUM(0));
}

static SCM scm_dc_connected(void)
{
    return((fd == -1)?SCM_BOOL_F:SCM_BOOL_T);
}

static SCM scm_dc_select(SCM timeout)
{
    struct pollfd pfd;
    int cto, ret, enob;
    
    if(timeout == SCM_UNDEFINED)
    {
	cto = -1;
    } else {
	SCM_ASSERT(SCM_INUMP(timeout), timeout, SCM_ARG1, "dc-select");
	cto = SCM_INUM(timeout);
    }
    if(fd < 0)
	scm_syserror_msg("dc-select", "Not connected", SCM_EOL, ENOTCONN);
    pfd.fd = fd;
    pfd.events = POLLIN;
    if(dc_wantwrite())
	pfd.events |= POLLOUT;
    if((ret = poll(&pfd, 1, cto)) < 0)
    {
	if(errno == EINTR)
	    return(SCM_BOOL_F);
	enob = errno;
	dc_disconnect();
	errno = enob;
	scm_syserror("dc-select");
    }
    if(((pfd.revents & POLLIN) && dc_handleread()) || ((pfd.revents & POLLOUT) && dc_handlewrite()))
    {
	if(errno == 0)
	{
	    fd = -1;
	    return(SCM_BOOL_F);
	}
	scm_syserror("dc-select");
    }
    return(ret?SCM_BOOL_T:SCM_BOOL_F);
}

static SCM makerespsmob(struct dc_response *resp)
{
    struct respsmob *data;
    
    data = scm_must_malloc(sizeof(*data), "respsmob");
    data->resp = resp;
    SCM_RETURN_NEWSMOB(resptype, data);
}

static SCM scm_dc_getresp(SCM tag)
{
    struct dc_response *resp;
    SCM ret;
    
    if(tag == SCM_UNDEFINED)
    {
	if((resp = dc_getresp()) == NULL)
	    return(SCM_BOOL_F);
    } else {
	SCM_ASSERT(SCM_INUMP(tag), tag, SCM_ARG1, "dc-getresp");
	if((resp = dc_gettaggedresp(SCM_INUM(tag))) == NULL)
	    return(SCM_BOOL_F);
    }
    ret = makerespsmob(resp);
    return(ret);
}

static SCM scm_dc_extract(SCM scm_resp)
{
    int i, o;
    struct dc_response *resp;
    SCM ret, l, w;
    
    SCM_ASSERT(SCM_SMOB_PREDICATE(resptype, scm_resp), scm_resp, SCM_ARG1, "dc-extract");
    resp = ((struct respsmob *)SCM_SMOB_DATA(scm_resp))->resp;
    ret = SCM_EOL;
    ret = scm_cons(scm_cons(scm_str2symbol("cmd"), scm_makfrom0str(icswcstombs(resp->cmdname, "UTF-8", NULL))), ret);
    ret = scm_cons(scm_cons(scm_str2symbol("code"), SCM_MAKINUM(resp->code)), ret);
    ret = scm_cons(scm_cons(scm_str2symbol("tag"), SCM_MAKINUM(resp->tag)), ret);
    l = SCM_EOL;
    for(i = resp->numlines - 1; i >= 0; i--)
    {
	w = SCM_EOL;
	for(o = resp->rlines[i].argc - 1; o >= 0; o--)
	    w = scm_cons(scm_makfrom0str(icswcstombs(resp->rlines[i].argv[o], "UTF-8", NULL)), w);
	l = scm_cons(w, l);
    }
    ret = scm_cons(scm_cons(scm_str2symbol("resp"), l), ret);
    return(ret);
}

static SCM scm_dc_intresp(SCM scm_resp)
{
    int i;
    struct dc_response *resp;
    struct dc_intresp *ires;
    SCM ret;
    
    SCM_ASSERT(SCM_SMOB_PREDICATE(resptype, scm_resp), scm_resp, SCM_ARG1, "dc-intresp");
    resp = ((struct respsmob *)SCM_SMOB_DATA(scm_resp))->resp;
    if((ires = dc_interpret(resp)) == NULL)
	return(SCM_BOOL_F);
    ret = SCM_EOL;
    for(i = ires->argc - 1; i >= 0; i--)
    {
	switch(ires->argv[i].type)
	{
	case 1:
	    ret = scm_cons(scm_makfrom0str(icswcstombs(ires->argv[i].val.str, "UTF-8", NULL)), ret);
	    break;
	case 2:
	    ret = scm_cons(scm_int2num(ires->argv[i].val.num), ret);
	    break;
	case 3:
	    ret = scm_cons(scm_double2num(ires->argv[i].val.flnum), ret);
	    break;
	}
    }
    dc_freeires(ires);
    return(ret);
}

static int qcmd_scmcb(struct dc_response *resp)
{
    struct scmcb *scmcb;
    
    scmcb = resp->data;
    scm_apply(scmcb->subr, scm_cons(makerespsmob(resp), SCM_EOL), SCM_EOL);
    scm_gc_unprotect_object(scmcb->subr);
    free(scmcb);
    return(2);
}

static SCM scm_dc_qcmd(SCM argv, SCM callback)
{
    int tag, enob;
    wchar_t **toks, *tok, *cmd;
    size_t tokssize, toksdata;
    SCM port;
    struct scmcb *scmcb;
    
    SCM_ASSERT(SCM_CONSP(argv), argv, SCM_ARG1, "dc-qcmd");
    if(callback != SCM_UNDEFINED)
	SCM_ASSERT(SCM_CLOSUREP(callback), callback, SCM_ARG2, "dc-qcmd");
    cmd = NULL;
    toks = NULL;
    tokssize = toksdata = 0;
    for(; argv != SCM_EOL; argv = SCM_CDR(argv))
    {
	port = scm_open_output_string();
	scm_display(SCM_CAR(argv), port);
	if((tok = icmbstowcs(SCM_STRING_CHARS(scm_get_output_string(port)), "UTF-8")) == NULL)
	{
	    enob = errno;
	    addtobuf(toks, NULL);
	    dc_freewcsarr(toks);
	    if(cmd != NULL)
		free(cmd);
	    errno = enob;
	    scm_syserror("dc-qcmd");
	}
	if(cmd == NULL)
	    cmd = tok;
	else
	    addtobuf(toks, tok);
    }
    addtobuf(toks, NULL);
    if(callback == SCM_UNDEFINED)
    {
	tag = dc_queuecmd(NULL, NULL, cmd, L"%%a", toks, NULL);
    } else {
	scmcb = scm_must_malloc(sizeof(*scmcb), "scmcb");
	scm_gc_protect_object(scmcb->subr = callback);
	tag = dc_queuecmd(qcmd_scmcb, scmcb, cmd, L"%%a", toks, NULL);
    }
    dc_freewcsarr(toks);
    if(cmd != NULL)
	free(cmd);
    return(SCM_MAKINUM(tag));
}

static void login_scmcb(int err, wchar_t *reason, struct scmcb *scmcb)
{
    SCM errsym;
    
    switch(err)
    {
    case DC_LOGIN_ERR_SUCCESS:
	errsym = scm_str2symbol("success");
	break;
    case DC_LOGIN_ERR_NOLOGIN:
	errsym = scm_str2symbol("nologin");
	break;
    case DC_LOGIN_ERR_SERVER:
	errsym = scm_str2symbol("server");
	break;
    case DC_LOGIN_ERR_USER:
	errsym = scm_str2symbol("user");
	break;
    case DC_LOGIN_ERR_CONV:
	errsym = scm_str2symbol("conv");
	break;
    case DC_LOGIN_ERR_AUTHFAIL:
	errsym = scm_str2symbol("authfail");
	break;
    }
    scm_apply(scmcb->subr, scm_cons(errsym, scm_cons((reason == NULL)?SCM_BOOL_F:scm_makfrom0str(icswcstombs(reason, "UTF-8", NULL)), SCM_EOL)), SCM_EOL);
    scm_gc_unprotect_object(scmcb->subr);
    free(scmcb);
}

static SCM scm_dc_loginasync(SCM callback, SCM useauthless, SCM username)
{
    struct scmcb *scmcb;
    
    SCM_ASSERT(SCM_CLOSUREP(callback), callback, SCM_ARG1, "dc-loginasync");
    scmcb = scm_must_malloc(sizeof(*scmcb), "scmcb");
    scm_gc_protect_object(scmcb->subr = callback);
    dc_loginasync(SCM_STRINGP(username)?SCM_STRING_CHARS(username):NULL, SCM_NFALSEP(useauthless), NULL, (void (*)(int, wchar_t *, void *))login_scmcb, scmcb);
    return(SCM_BOOL_T);
}

static SCM scm_dc_lexsexpr(SCM sexpr)
{
    SCM ret;
    wchar_t **arr, **ap, *buf;
    
    SCM_ASSERT(SCM_STRINGP(sexpr), sexpr, SCM_ARG1, "dc-lexsexpr");
    if((buf = icmbstowcs(SCM_STRING_CHARS(sexpr), NULL)) == NULL)
	scm_syserror("dc-lexsexpr");
    arr = dc_lexsexpr(buf);
    free(buf);
    ret = SCM_EOL;
    if(arr != NULL)
    {
	for(ap = arr; *ap != NULL; ap++)
	    ret = scm_cons(scm_makfrom0str(icswcstombs(*ap, "UTF-8", NULL)), ret);
	dc_freewcsarr(arr);
    }
    return(scm_reverse(ret));
}

static SCM scm_dc_checkproto(SCM resp, SCM version)
{
    int ver;
    
    SCM_ASSERT(SCM_SMOB_PREDICATE(resptype, resp), resp, SCM_ARG1, "dc-checkproto");
    if(version == SCM_UNDEFINED)
    {
	ver = DC_LATEST;
    } else {
	SCM_ASSERT(SCM_INUMP(version), version, SCM_ARG2, "dc-checkproto");
	ver = SCM_INUM(version);
    }
    if(dc_checkprotocol(((struct respsmob *)SCM_SMOB_DATA(resp))->resp, ver))
	return(SCM_BOOL_F);
    else
	return(SCM_BOOL_T);
}

static size_t resp_free(SCM respsmob)
{
    struct respsmob *data;
    
    data = (struct respsmob *)SCM_SMOB_DATA(respsmob);
    dc_freeresp(data->resp);
    free(data);
    return(sizeof(*data));
}

static int resp_print(SCM respsmob, SCM port, scm_print_state *pstate)
{
    struct respsmob *data;
    
    data = (struct respsmob *)SCM_SMOB_DATA(respsmob);
    scm_puts("#<dc-response ", port);
    scm_display(SCM_MAKINUM(data->resp->tag), port);
    scm_puts(" ", port);
    scm_puts(icswcstombs(data->resp->cmdname, "UTF-8", NULL), port);
    scm_puts(" ", port);
    scm_display(SCM_MAKINUM(data->resp->code), port);
    scm_puts(">", port);
    return(1);
}

void init_guiledc(void)
{
    scm_c_define_gsubr("dc-connect", 0, 1, 0, scm_dc_connect);
    scm_c_define_gsubr("dc-disconnect", 0, 0, 0, scm_dc_disconnect);
    scm_c_define_gsubr("dc-connected", 0, 0, 0, scm_dc_connected);
    scm_c_define_gsubr("dc-select", 0, 1, 0, scm_dc_select);
    scm_c_define_gsubr("dc-getresp", 0, 1, 0, scm_dc_getresp);
    scm_c_define_gsubr("dc-extract", 1, 0, 0, scm_dc_extract);
    scm_c_define_gsubr("dc-intresp", 1, 0, 0, scm_dc_intresp);
    scm_c_define_gsubr("dc-qcmd", 1, 1, 0, scm_dc_qcmd);
    scm_c_define_gsubr("dc-loginasync", 2, 1, 0, scm_dc_loginasync);
    scm_c_define_gsubr("dc-lexsexpr", 1, 0, 0, scm_dc_lexsexpr);
    scm_c_define_gsubr("dc-checkproto", 1, 1, 0, scm_dc_checkproto);
    scm_c_define("dc-latest", SCM_MAKINUM(DC_LATEST));
    resptype = scm_make_smob_type("dc-resp", sizeof(struct respsmob));
    scm_set_smob_free(resptype, resp_free);
    scm_set_smob_print(resptype, resp_print);
    dc_init();
}
