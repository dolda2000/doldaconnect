bin_PROGRAMS=dolcon

dolcon_SOURCES=	main.c \
		progressbar.c \
		progressbar.h

EXTRA_DIST=mainwnd.desc inpdialog.desc pref.desc makegdesc

BUILT_SOURCES=mainwnd.gtk inpdialog.gtk pref.gtk

main.c: mainwnd.gtk inpdialog.gtk

localedir=$(datadir)/locale
dolcon_LDFLAGS=$(shell pkg-config --libs gtk+-2.0)
dolcon_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_CPPFLAGS=$(shell pkg-config --cflags gtk+-2.0) -DLOCALEDIR=\"$(localedir)\"

%.gtk: %.desc makegdesc
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< | ./makegdesc >$@

mainwnd.desc: ../../config.h
