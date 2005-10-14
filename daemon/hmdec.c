#include <stdio.h>
#include <stdlib.h>

struct node
{
    int l, r, c;
};

struct node tree[512];
int newnode = 1;

int getbits(int numbits)
{
    static int cb = 8, c;
    int ret;
    
    if(numbits < 0)
    {
	cb = 8;
	return(0);
    }
    ret = 0;
    for(; numbits > 0; numbits--)
    {
	if(cb >= 8)
	{
	    c = getc(stdin);
	    cb = 0;
	}
	ret <<= 1;
	if(c & (1 << (cb++)))
	    ret |= 1;
    }
    return(ret);
}

int main(int argc, char **argv) {
    int i, o;
    int n;
    int size, ts, c;
    int chars[256], lens[256];
    
    for(i = 0; i < 512; i++)
	tree[i].l = tree[i].r = tree[i].c = -1;
    if((getc(stdin) != 'H') ||
       (getc(stdin) != 'E') ||
       (getc(stdin) != '3') ||
       (getc(stdin) != 13)) {
	fprintf(stderr, "not a HE3 file\n");
	exit(1);
    }
    getc(stdin);
    fread(&size, 4, 1, stdin);
    ts = 0;
    fread(&ts, 2, 1, stdin);
    for(i = 0; i < ts; i++) {
	chars[i] = getc(stdin);
	lens[i] = getc(stdin);
    }
    for(i = 0; i < ts; i++) {
	n = 0;
	for(o = 0; o < lens[i]; o++) {
	    if(getbits(1)) {
		if(tree[n].r < 0)
		    n = tree[n].r = newnode++;
		else
		    n = tree[n].r;
	    } else {
		if(tree[n].l < 0)
		    n = tree[n].l = newnode++;
		else
		    n = tree[n].l;
	    }
	}
	if(tree[n].c >= 0) {
	    fprintf(stderr, "double-used node: \"%c\"\n", chars[i]);
	    exit(1);
	}
	tree[n].c = chars[i];
    }
    getbits(-1);
    n = 0;
    for(i = 0; i < size;) {
	if(getbits(1))
	    n = tree[n].r;
	else
	    n = tree[n].l;
	if(n < 0) {
	    fprintf(stderr, "invalid path");
	    exit(1);
	}
	if(tree[n].c >= 0) {
	    putc(tree[n].c, stdout);
	    n = 0;
	    i++;
	}
    }
    return(0);
}
