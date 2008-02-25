#ifndef _UILIB_H
#define _UILIB_H

#include <wchar.h>

#define DC_LATEST 3

typedef long long dc_lnum_t;

struct dc_response
{
    struct dc_response *next, *prev;
    int code, tag;
    wchar_t *cmdname;
    void *data;
    void *internal;
    struct dc_respline
    {
	int argc;
	wchar_t **argv;
    } *rlines;
    size_t linessize;
    int numlines;
    int curline;
};

struct dc_intresp
{
    int code;
    int argc;
    struct
    {
	int type;
	union
	{
	    int num;
	    dc_lnum_t lnum;
	    wchar_t *str;
	    double flnum;
	} val;
    } *argv;
};

char *dc_srv_local;

int dc_init(void);
void dc_cleanup(void);
void dc_disconnect(void);
void dc_freeresp(struct dc_response *resp);
struct dc_response *dc_getresp(void);
struct dc_response *dc_gettaggedresp(int tag);
struct dc_response *dc_gettaggedrespsync(int tag);
int dc_wantwrite(void);
int dc_getstate(void);
int dc_queuecmd(int (*callback)(struct dc_response *), void *data, ...);
int dc_handleread(void);
int dc_handlewrite(void);
int dc_connect(char *host);
int dc_connectsync(char *host, struct dc_response **respbuf);
int dc_connectsync2(char *host, int rev);
struct dc_intresp *dc_interpret(struct dc_response *resp);
void dc_freeires(struct dc_intresp *ires);
int dc_checkprotocol(struct dc_response *resp, int revision);
const char *dc_gethostname(void);
int dc_getfd(void);

#endif
