gaimdir = $(libdir)/gaim
purpledir = $(libdir)/purple-2

if CLI_GAIM
gaim_LTLIBRARIES = libdolcon-gaim.la
endif
if CLI_PIDGIN
purple_LTLIBRARIES = libdolcon-purple.la
endif

libdolcon_gaim_la_SOURCES = gaim-dolcon.c
libdolcon_purple_la_SOURCES = purple-dolcon.c

BUILT_SOURCES = gaim-dolcon.c purple-dolcon.c

EXTRA_DIST = mksrc

gaim-dolcon.c purple-dolcon.c: gp-dolcon.c
	target="$@"; \
	basename="$${target%-dolcon.c}"; \
	./mksrc "$$basename" <$< >$@

libdolcon_gaim_la_LDFLAGS=-module -avoid-version $(shell pkg-config --libs gaim glib)
libdolcon_gaim_la_LIBADD=$(top_srcdir)/lib/libdcui.la
libdolcon_gaim_la_CPPFLAGS=$(shell pkg-config --cflags gaim glib) -DGAIM_PLUGINS
libdolcon_purple_la_LDFLAGS=-module -avoid-version $(shell pkg-config --libs purple glib)
libdolcon_purple_la_LIBADD=$(top_srcdir)/lib/libdcui.la
libdolcon_purple_la_CPPFLAGS=$(shell pkg-config --cflags purple glib) -DPURPLE_PLUGINS
