#ifndef _UIMISC_H
#define _UIMISC_H

#define DC_LOGIN_ERR_SUCCESS 0
#define DC_LOGIN_ERR_NOLOGIN 1
#define DC_LOGIN_ERR_SERVER 2
#define DC_LOGIN_ERR_USER 3
#define DC_LOGIN_ERR_CONV 4
#define DC_LOGIN_ERR_AUTHFAIL 5

#define DC_LOGIN_CONV_NOECHO 0
#define DC_LOGIN_CONV_ECHO 1
#define DC_LOGIN_CONV_INFO 2
#define DC_LOGIN_CONV_ERROR 3

#define DC_FNN_STATE_SYN 0
#define DC_FNN_STATE_HS 1
#define DC_FNN_STATE_EST 2
#define DC_FNN_STATE_DEAD 3

#define DC_TRNS_WAITING 0
#define DC_TRNS_HS 1
#define DC_TRNS_MAIN 2
#define DC_TRNS_DONE 3

#define DC_TRNSD_UNKNOWN 0
#define DC_TRNSD_UP 1
#define DC_TRNSD_DOWN 2

#define DC_TRNSE_NOERROR 0
#define DC_TRNSE_NOTFOUND 1
#define DC_TRNSE_NOSLOTS 2

#include <time.h>

struct dc_fnetnode
{
    struct dc_fnetnode *next, *prev;
    int id;
    wchar_t *name;
    wchar_t *fnet;
    int state;
    int numusers;
    int found;
    void (*destroycb)(struct dc_fnetnode *fn);
    void *udata;
};

struct dc_transfer
{
    struct dc_transfer *next, *prev;
    int id;
    int dir, state;
    wchar_t *peerid, *peernick;
    wchar_t *path;
    int size, curpos;
    int found;
    int error;
    time_t errortime;
    wchar_t *hash;
    void (*destroycb)(struct dc_transfer *transfer);
    void *udata;
};

void dc_loginasync(char *username, int useauthless, int (*conv)(int, wchar_t *, char **, void *), void (*callback)(int, wchar_t *, void *), void *udata);
struct dc_fnetnode *dc_findfnetnode(int id);
void dc_getfnlistasync(void (*callback)(int, void *), void *udata);
void dc_uimisc_handlenotify(struct dc_response *resp);
struct dc_transfer *dc_findtransfer(int id);
void dc_gettrlistasync(void (*callback)(int, void *), void *udata);
wchar_t **dc_lexsexpr(wchar_t *sexpr);
void dc_freewcsarr(wchar_t **arr);

extern struct dc_fnetnode *dc_fnetnodes;
extern struct dc_transfer *dc_transfers;

#endif
