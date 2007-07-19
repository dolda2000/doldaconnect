if CLI_GTK2
bin_PROGRAMS=dolcon
app_DATA=dolcon.desktop
endif

dolcon_SOURCES=	dolcon.c \
		hublist.c \
		hublist.h \
		hublist-xml.c \
		hublist-old.c \
		progressbar.c \
		progressbar.h

EXTRA_DIST=mainwnd.desc inpdialog.desc pref.desc reslist.desc dolcon.desktop

BUILT_SOURCES=	mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

main.c: mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

localedir=$(datadir)/locale
dolcon_LDFLAGS=	@GTK2_LDADD@ @LIBXML_LDADD@
dolcon_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_CPPFLAGS=@GTK2_CFLAGS@ @LIBXML_CFLAGS@ \
		-DLOCALEDIR=\"$(localedir)\"

.desc.gtk: $(top_srcdir)/common/makegdesc
	target="$@"; \
	basename="$${target%.gtk}"; \
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< \
	 | $(top_srcdir)/common/makegdesc -h "$${basename}.gtkh" >$@

mainwnd.desc: ../../config.h

appdir=$(datadir)/applications
