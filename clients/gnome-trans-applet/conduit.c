#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <doldaconnect/utils.h>
#include <panel-applet.h>
#include <time.h>

#include "conduit.h"

void (*cb_condstate)(struct conduit *conduit, void *data) = NULL;
void (*cb_trsize)(struct transfer *transfer, void *data) = NULL;
void (*cb_trpos)(struct transfer *transfer, void *data) = NULL;
void (*cb_trnew)(struct transfer *transfer, void *data) = NULL;
void (*cb_trfree)(struct transfer *transfer, void *data) = NULL;

struct transfer *findtransferbytag(struct conduit *conduit, char *tag)
{
    struct transfer *transfer;
    
    for(transfer = conduit->transfers; transfer != NULL; transfer = transfer->next)
    {
	if((transfer->tag != NULL) && !strcmp(transfer->tag, tag))
	    break;
    }
    return(transfer);
}

void transfersetsize(struct transfer *transfer, int size)
{
    transfer->size = size;
    cb_trsize(transfer, transfer->conduit->udata);
}

void transfersetpos(struct transfer *transfer, int pos)
{
    transfer->pos = pos;
    cb_trpos(transfer, transfer->conduit->udata);
}

static gboolean trupdatetime(struct transfer *transfer)
{
    time_t now;
    
    if((transfer->size == -1) || (transfer->pos == -1))
	return(TRUE);
    now = time(NULL);
    if(now - transfer->ckptime >= 10)
    {
	transfer->cmptime = transfer->ckptime;
	transfer->cmpsize = transfer->ckpsize;
	transfer->ckptime = 0;
    }
    if(transfer->ckptime == 0)
    {
	transfer->ckptime = now;
	transfer->ckpsize = transfer->pos;
    }
    return(TRUE);
}

struct transfer *newtransfer(struct conduit *conduit, char *tag, int size, int pos)
{
    struct transfer *transfer;
    
    transfer = smalloc(sizeof(*transfer));
    memset(transfer, 0, sizeof(*transfer));
    if(tag != NULL)
	transfer->tag = sstrdup(tag);
    transfer->size = size;
    transfer->pos = pos;
    transfer->timeout = g_timeout_add(1000, (gboolean (*)(gpointer))trupdatetime, transfer);
    transfer->next = conduit->transfers;
    transfer->conduit = conduit;
    if(conduit->transfers != NULL)
	conduit->transfers->prev = transfer;
    conduit->transfers = transfer;
    cb_trnew(transfer, conduit->udata);
    return(transfer);
}

void freetransfer(struct transfer *transfer)
{
    if(transfer->next != NULL)
	transfer->next->prev = transfer->prev;
    if(transfer->prev != NULL)
	transfer->prev->next = transfer->next;
    if(transfer->conduit->transfers == transfer)
	transfer->conduit->transfers = transfer->next;
    cb_trfree(transfer, transfer->conduit->udata);
    g_source_remove(transfer->timeout);
    if(transfer->tag != NULL)
	free(transfer->tag);
    free(transfer);
}

struct conduit *newconduit(struct conduitiface *iface, void *udata)
{
    struct conduit *conduit;
    
    conduit = smalloc(sizeof(*conduit));
    memset(conduit, 0, sizeof(*conduit));
    conduit->iface = iface;
    conduit->udata = udata;
    if(iface->init(conduit))
    {
	free(conduit);
	return(NULL);
    }
    return(conduit);
}

void freeconduit(struct conduit *conduit)
{
    conduit->iface->destroy(conduit);
    while(conduit->transfers != NULL)
	freetransfer(conduit->transfers);
    free(conduit);
}

int condtryconn(struct conduit *conduit)
{
    if(conduit->state != CNDS_IDLE)
	return(-1);
    if(conduit->iface->connect(conduit))
	return(-1);
    conduit->state = CNDS_SYN;
    return(0);
}

void conddisconn(struct conduit *conduit)
{
    while(conduit->transfers != NULL)
	freetransfer(conduit->transfers);
    conduit->state = CNDS_IDLE;
    cb_condstate(conduit, conduit->udata);
}

void condconnected(struct conduit *conduit)
{
    conduit->state = CNDS_EST;
    cb_condstate(conduit, conduit->udata);
}
