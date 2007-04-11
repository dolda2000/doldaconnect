EXTRA_DIST = makecmds uicmds

SUBDIRS=. @extlibs@
DIST_SUBDIRS=guile

lib_LTLIBRARIES = libdcui.la

libdcui_la_SOURCES = uilib.c uimisc.c
libdcui_la_LIBADD = @KRB5_LDADD@ $(top_srcdir)/common/libcommon.a
libdcui_la_LDFLAGS = -version-info 1:0:1

BUILT_SOURCES = initcmds.h

initcmds.h: uicmds makecmds
	./makecmds <uicmds >initcmds.h

AM_CPPFLAGS=-I$(top_srcdir)/include
