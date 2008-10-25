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

#define DC_FNPD_INT 0
#define DC_FNPD_LL 1
#define DC_FNPD_STR 2

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
    struct dc_fnetpeer *peers;
    struct dc_fnetpeerdatum *peerdata;
    void (*newpeercb)(struct dc_fnetpeer *peer);
    void (*delpeercb)(struct dc_fnetpeer *peer);
    void (*chpeercb)(struct dc_fnetpeer *peer);
    wchar_t *pubid;
};

struct dc_fnetpeerdatum
{
    struct dc_fnetpeerdatum *next, *prev;
    int refcount;
    int dt;
    wchar_t *id;
};

struct dc_fnetpeerdi
{
    struct dc_fnetpeerdatum *datum;
    union
    {
	int num;
	long long lnum;
	wchar_t *str;
    } d;
};

struct dc_fnetpeer
{
    struct dc_fnetpeer *next, *prev;
    struct dc_fnetnode *fn;
    wchar_t *id;
    wchar_t *nick;
    int dinum;
    int found;
    struct dc_fnetpeerdi *di;
};

struct dc_transfer
{
    struct dc_transfer *next, *prev;
    int id;
    int dir, state;
    wchar_t *peerid, *peernick;
    wchar_t *path;
    dc_lnum_t size, curpos;
    int found;
    int error;
    time_t errortime;
    wchar_t *hash;
    void (*destroycb)(struct dc_transfer *transfer);
    void *udata;
};

int dc_convtty(int type, wchar_t *text, char **resp, void *data);
int dc_convnone(int type, wchar_t *text, char **resp, void *data);
void dc_loginasync(char *username, int useauthless, int (*conv)(int, wchar_t *, char **, void *), void (*callback)(int, wchar_t *, void *), void *udata);
int dc_login(char *username, int useauthless, int (*conv)(int, wchar_t *, char **, void *), wchar_t **reason);
struct dc_fnetnode *dc_findfnetnode(int id);
void dc_getfnlistasync(void (*callback)(int, void *), void *udata);
void dc_uimisc_handlenotify(struct dc_response *resp);
struct dc_transfer *dc_findtransfer(int id);
void dc_gettrlistasync(void (*callback)(int, void *), void *udata);
wchar_t **dc_lexsexpr(wchar_t *sexpr);
void dc_freewcsarr(wchar_t **arr);
void dc_getpeerlistasync(struct dc_fnetnode *fn, void (*callback)(struct dc_fnetnode *, int, void *), void *udata);
struct dc_fnetpeer *dc_fnetfindpeer(struct dc_fnetnode *fn, wchar_t *id);

extern struct dc_fnetnode *dc_fnetnodes;
extern struct dc_transfer *dc_transfers;

#endif
