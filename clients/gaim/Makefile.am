plugindir = $(libdir)/gaim

plugin_LTLIBRARIES = libdolcon-gaim.la

libdolcon_gaim_la_SOURCES = gaim-dolcon.c

libdolcon_gaim_la_LDFLAGS=-module -avoid-version $(shell pkg-config --libs gaim glib)
libdolcon_gaim_la_LIBADD=$(top_srcdir)/lib/libdcui.la
libdolcon_gaim_la_CPPFLAGS=$(shell pkg-config --cflags gaim glib) -DGAIM_PLUGINS
