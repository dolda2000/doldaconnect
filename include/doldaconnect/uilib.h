#ifndef _UILIB_H
#define _UILIB_H

#include <wchar.h>

#define DC_LATEST 2

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
	    wchar_t *str;
	    double flnum;
	} val;
    } *argv;
};

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
struct dc_intresp *dc_interpret(struct dc_response *resp);
void dc_freeires(struct dc_intresp *ires);
int dc_checkprotocol(struct dc_response *resp, int revision);
const char *dc_gethostname(void);

#endif
