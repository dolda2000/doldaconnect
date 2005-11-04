plugindir = $(libdir)/gaim

plugin_LTLIBRARIES = libdolcon-gaim.la

libdolcon_gaim_la_SOURCES = dolcon-gaim.c

dolcon_trans_applet_LDFLAGS=-module -avoid-version $(shell pkg-config --libs gaim)
dolcon_trans_applet_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_trans_applet_CPPFLAGS=$(shell pkg-config --cflags gaim)
