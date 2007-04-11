#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <panel-applet.h>
#include <doldaconnect/uilib.h>
#include <doldaconnect/uimisc.h>
#include <doldaconnect/utils.h>

#include "conduit.h"

struct data
{
    int fd;
    int gdkread, gdkwrite;
};

struct dtdata
{
    struct conduit *conduit;
    struct transfer *ct;
    char *tag;
    int realtag;
};

static struct conduit *inuse = NULL;

static void dcfdcb(struct conduit *conduit, int fd, GdkInputCondition condition);

static void updatewrite(struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(data->fd < 0)
	return;
    if(dc_wantwrite())
    {
	if(data->gdkwrite == -1)
	    data->gdkwrite = gdk_input_add(data->fd, GDK_INPUT_WRITE, (void (*)(gpointer, int, GdkInputCondition))dcfdcb, conduit);
    } else {
	if(data->gdkwrite != -1)
	{
	    gdk_input_remove(data->gdkwrite);
	    data->gdkwrite = -1;
	}
    }
}

static void disconnected(struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(inuse == conduit)
	inuse = NULL;
    if(data->gdkread != -1)
    {
	gdk_input_remove(data->gdkread);
	data->gdkread = -1;
    }
    if(data->gdkwrite != -1)
    {
	gdk_input_remove(data->gdkwrite);
	data->gdkwrite = -1;
    }
    data->fd = -1;
    conddisconn(conduit);
}

static int noconv(int type, wchar_t *text, char **resp, void *data)
{
    return(1);
}

static char *gettag(struct dc_transfer *dt)
{
    char *mbspath, *p, *buf;
    
    if(dt->path == NULL)
	return(NULL);
    if((mbspath = icwcstombs(dt->path, "UTF-8")) == NULL)
	return(NULL);
    /* XXX: Achtung! Too DC-specific! */
    if((p = strrchr(mbspath, '\\')) == NULL)
	p = mbspath;
    else
	p++;
    buf = sstrdup(p);
    free(mbspath);
    return(buf);
}

static void dtfreecb(struct dc_transfer *dt)
{
    struct dtdata *dtd;
    
    if((dtd = dt->udata) == NULL)
	return;
    if(dtd->ct != NULL)
	freetransfer(dtd->ct);
    if(dtd->tag != NULL)
	free(dtd->tag);
    free(dtd);
}

static int lstrargcb(struct dc_response *resp)
{
    struct dc_transfer *dt;
    struct dtdata *dtd;
    struct dc_intresp *ires;
    
    dt = resp->data;
    dtd = dt->udata;
    if(resp->code == 200)
    {
	while((dtd->tag == NULL) && ((ires = dc_interpret(resp)) != NULL))
	{
	    if(!wcscmp(ires->argv[0].val.str, L"tag"))
	    {
		dtd->realtag = 1;
		dtd->tag = icwcstombs(ires->argv[1].val.str, "UTF-8");
	    }
	    dc_freeires(ires);
	}
    }
    if(dtd->tag == NULL)
	dtd->tag = gettag(dt);
    dtd->ct = newtransfer(dtd->conduit, dtd->tag, dt->size, dt->curpos);
    return(1);
}

static void inittrans(struct conduit *conduit, struct dc_transfer *dt)
{
    struct dtdata *dtd;

    dtd = smalloc(sizeof(*dtd));
    memset(dtd, 0, sizeof(*dtd));
    dtd->conduit = conduit;
    dt->udata = dtd;
    dt->destroycb = dtfreecb;
    dc_queuecmd(lstrargcb, dt, L"lstrarg", L"%%i", dt->id, NULL);
}

static void trlistcb(int resp, struct conduit *conduit)
{
    struct data *data;
    struct dc_transfer *dt;
    
    data = conduit->cdata;
    if(resp != 200)
	return;
    for(dt = dc_transfers; dt != NULL; dt = dt->next)
    {
	if(dt->dir != DC_TRNSD_DOWN)
	    continue;
	inittrans(conduit, dt);
    }
}

static void logincb(int err, wchar_t *reason, struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(err != DC_LOGIN_ERR_SUCCESS)
    {
	dc_disconnect();
	disconnected(conduit);
	return;
    }
    condconnected(conduit);
    dc_gettrlistasync((void (*)(int, void *))trlistcb, conduit);
    dc_queuecmd(NULL, NULL, L"notify", L"trans:act", L"on", L"trans:prog", L"on", NULL);
}

static void dcfdcb(struct conduit *conduit, int fd, GdkInputCondition condition)
{
    struct data *data;
    struct dc_response *resp;
    struct dc_intresp *ires;
    struct dc_transfer *dt;
    struct dtdata *dtd;
    
    data = conduit->cdata;
    if(((condition & GDK_INPUT_READ) && dc_handleread()) || ((condition & GDK_INPUT_WRITE) && dc_handlewrite()))
    {
	disconnected(conduit);
	return;
    }
    while((resp = dc_getresp()) != NULL)
    {
	if(!wcscmp(resp->cmdname, L".connect"))
	{
	    if(resp->code == 200)
	    {
		dc_loginasync(NULL, 1, noconv, (void (*)(int, wchar_t *, void *))logincb, conduit);
	    } else {
		dc_disconnect();
		disconnected(conduit);
	    }
	} else if(!wcscmp(resp->cmdname, L".notify")) {
	    dc_uimisc_handlenotify(resp);
	    switch(resp->code)
	    {
	    case 610:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if((dt = dc_findtransfer(ires->argv[0].val.num)) != NULL)
		    {
			if(dt->dir == DC_TRNSD_DOWN)
			    inittrans(conduit, dt);
		    }
		    dc_freeires(ires);
		}
		break;
	    case 613:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if((dt = dc_findtransfer(ires->argv[0].val.num)) != NULL)
		    {
			if(((dtd = dt->udata) != NULL) && (dtd->ct != NULL))
			{
			    if(dtd->ct->size != dt->size)
				transfersetsize(dtd->ct, dt->size);
			}
		    }
		    dc_freeires(ires);
		}
		break;
	    case 615:
		if((ires = dc_interpret(resp)) != NULL)
		{
		    if((dt = dc_findtransfer(ires->argv[0].val.num)) != NULL)
		    {
			if(((dtd = dt->udata) != NULL) && (dtd->ct != NULL))
			{
			    if(dtd->ct->pos != dt->curpos)
				transfersetpos(dtd->ct, dt->curpos);
			}
		    }
		    dc_freeires(ires);
		}
		break;
	    }
	}
	dc_freeresp(resp);
    }
    updatewrite(conduit);
}

static int init(struct conduit *conduit)
{
    static int inited = 0;
    struct data *data;
    
    if(!inited)
    {
	dc_init();
	inited = 1;
    }
    data = smalloc(sizeof(*data));
    memset(data, 0, sizeof(*data));
    data->fd = -1;
    data->gdkread = data->gdkwrite = -1;
    conduit->cdata = data;
    return(0);
}

static int connect(struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(inuse != NULL)
	return(-1);
    if((data->fd = dc_connect(NULL)) < 0)
	return(-1);
    data->gdkread = gdk_input_add(data->fd, GDK_INPUT_READ, (void (*)(gpointer, int, GdkInputCondition))dcfdcb, conduit);
    updatewrite(conduit);
    inuse = conduit;
    return(0);
}

static void destroy(struct conduit *conduit)
{
    struct data *data;
    
    data = conduit->cdata;
    if(data->gdkread != -1)
	gdk_input_remove(data->gdkread);
    if(data->gdkwrite != -1)
	gdk_input_remove(data->gdkwrite);
    if(data->fd >= 0)
	dc_disconnect();
    if(inuse == conduit)
	inuse = NULL;
    free(data);
}

static int cancel(struct conduit *conduit, struct transfer *transfer)
{
    struct data *data;
    struct dtdata *dtd;
    struct dc_transfer *dt;
    
    data = conduit->cdata;
    for(dt = dc_transfers; dt != NULL; dt = dt->next)
    {
	if(((dtd = dt->udata) != NULL) && (dtd->ct == transfer))
	{
	    dc_queuecmd(NULL, NULL, L"cancel", L"%%i", dt->id, NULL);
	    return(0);
	}
    }
    errno = -ESRCH;
    return(-1);
}

static struct conduitiface st_conduit_dclib =
{
    .init = init,
    .connect = connect,
    .destroy = destroy,
    .cancel = cancel,
};

struct conduitiface *conduit_dclib = &st_conduit_dclib;
