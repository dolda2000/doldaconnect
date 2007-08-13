SUBDIRS=gtk2 tty gnome-trans-applet gaim gui-shell
EXTRA_DIST=hellodolda.png dolda-icon.xpm

iconsdir = $(datadir)/pixmaps
icons_DATA = hellodolda.png

noinst_PROGRAMS=test

test_SOURCES=test.c

test_LDADD=$(top_srcdir)/lib/libdcui.la
AM_CPPFLAGS=-I$(top_srcdir)/include
