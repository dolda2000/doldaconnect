#ifndef _TIGER_H
#define _TIGER_H

struct tigerhash {
    unsigned long long a, b, c;
    unsigned char block[64];
    int offset;
    size_t len;
};

struct tigertreehash {
    int blocks;
    char block[1024];
    int offset;
    char stack[64][24];
    int depth;
};

void inittiger(struct tigerhash *th);
void dotiger(struct tigerhash *th, char *buf, size_t buflen);
void synctiger(struct tigerhash *th);
void restiger(struct tigerhash *th, char *rbuf);
void inittigertree(struct tigertreehash *tth);
void dotigertree(struct tigertreehash *tth, char *buf, size_t buflen);
void synctigertree(struct tigertreehash *tth);
void restigertree(struct tigertreehash *tth, char *rbuf);
void pushtigertree(struct tigertreehash *tth, char *buf);

#endif
