EXTRA_DIST = makecmds uicmds

SUBDIRS = ucs . guile

lib_LTLIBRARIES = libdcui.la

libdcui_la_SOURCES = uilib.c uimisc.c
libdcui_la_LIBADD = @KRB5_LIBS@ $(top_srcdir)/common/libcommon.a
libdcui_la_LDFLAGS = -version-info 3:0:0
libdcui_la_CPPFLAGS = @KRB5_CFLAGS@ -D_GNU_SOURCE

BUILT_SOURCES = initcmds.h

initcmds.h: uicmds makecmds
	./makecmds <uicmds >initcmds.h

AM_CPPFLAGS=-I$(top_srcdir)/include
