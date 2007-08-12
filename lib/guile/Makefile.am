SUBDIRS=dolcon

EXTRA_DIST=autodl chatlog hubmgr dcruncmd

lib_LTLIBRARIES=libdolcon-guile.la

libdolcon_guile_la_SOURCES=dolcon-guile.c
libdolcon_guile_la_LDFLAGS=-module -avoid-version
libdolcon_guile_la_LIBADD=@GUILE_LDFLAGS@ $(top_srcdir)/lib/libdcui.la
libdolcon_guile_la_CPPFLAGS=@GUILE_CFLAGS@
