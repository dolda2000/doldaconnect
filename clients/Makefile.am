SUBDIRS=@clients@
DIST_SUBDIRS=gtk2 gnome-trans-applet
EXTRA_DIST=hellodolda.jpg

iconsdir = $(datadir)/pixmaps
icons_DATA = hellodolda.jpg

noinst_PROGRAMS=test

test_SOURCES=test.c

test_LDADD=$(top_srcdir)/lib/libdcui.la
AM_CPPFLAGS=-I$(top_srcdir)/include
