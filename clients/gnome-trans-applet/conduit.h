#ifndef _CONDUIT_H
#define _CONDUIT_H

#include <sys/types.h>

#define CNDS_IDLE 0
#define CNDS_SYN 1
#define CNDS_EST 2

struct transfer
{
    struct transfer *next, *prev;
    struct conduit *conduit;
    char *tag; /* UTF8 */
    int pos, size;
    time_t cmptime, ckptime;
    size_t cmpsize, ckpsize;
    int timeout;
};

struct conduit
{
    struct transfer *transfers;
    struct conduitiface *iface;
    void *cdata, *udata;
    int state;
};

struct conduitiface
{
    int (*init)(struct conduit *conduit);
    int (*connect)(struct conduit *conduit);
    void (*destroy)(struct conduit *conduit);
};

struct transfer *findtransferbytag(struct conduit *conduit, char *tag);
void transfersetsize(struct transfer *transfer, int size);
void transfersetpos(struct transfer *transfer, int pos);
struct transfer *newtransfer(struct conduit *conduit, char *tag, int size, int pos);
void freetransfer(struct transfer *transfer);
struct conduit *newconduit(struct conduitiface *iface, void *udata);
void freeconduit(struct conduit *conduit);
int condtryconn(struct conduit *conduit);
void conddisconn(struct conduit *conduit);
void condconnected(struct conduit *conduit);

extern void (*cb_condstate)(struct conduit *conduit, void *data);
extern void (*cb_trsize)(struct transfer *transfer, void *data);
extern void (*cb_trpos)(struct transfer *transfer, void *data);
extern void (*cb_trnew)(struct transfer *transfer, void *data);
extern void (*cb_trfree)(struct transfer *transfer, void *data);
extern struct conduitiface *conduit_pipe;
extern struct conduitiface *conduit_dclib;

#endif
