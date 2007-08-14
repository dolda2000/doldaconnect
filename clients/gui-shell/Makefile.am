bin_PROGRAMS=dolcon-launch doldacond-shell

dolcon_launch_SOURCES=launch.c

doldacond_shell_SOURCES=dsh.c

EXTRA_DIST=dsh-start.desc dsh-menu.desc
BUILT_SOURCES=dsh-start.gtk dsh-menu.gtk

dsh.c: dsh-start.gtk dsh-menu.gtk

AM_CPPFLAGS=-I$(top_srcdir)/include
localedir=$(datadir)/locale
doldacond_shell_LDADD=$(top_srcdir)/lib/libdcui.la
doldacond_shell_LDFLAGS= @GTK2_LIBS@ @LIBNOTIFY_LIBS@
doldacond_shell_CPPFLAGS=@GTK2_CFLAGS@ @LIBNOTIFY_CFLAGS@ \
			 -DLOCALEDIR=\"$(localedir)\"

.desc.gtk: $(top_srcdir)/common/makegdesc
	target="$@"; \
	basename="$${target%.gtk}"; \
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< \
	 | $(top_srcdir)/common/makegdesc -h "$${basename}.gtkh" >$@
