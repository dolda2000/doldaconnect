if CLI_GTK2
bin_PROGRAMS=dolcon
if !GUISHELL
app_DATA=dolcon.desktop
endif
endif

dolcon_SOURCES=	dolcon.c \
		dolcon.h \
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
dolcon_LDFLAGS=	@GTK2_LIBS@ @LIBXML_LIBS@
dolcon_LDADD=$(top_srcdir)/lib/libdcui.la $(top_srcdir)/common/libhttp.a
dolcon_CPPFLAGS=@GTK2_CFLAGS@ @LIBXML_CFLAGS@ \
		-DLOCALEDIR=\"$(localedir)\"

.desc.gtk: $(top_srcdir)/common/makegdesc
	target="$@"; \
	basename="$${target%.gtk}"; \
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< \
	 | $(top_srcdir)/common/makegdesc -h "$${basename}.gtkh" >$@

mainwnd.desc: ../../config.h

appdir=$(datadir)/applications
