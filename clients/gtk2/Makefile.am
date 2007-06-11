bin_PROGRAMS=dolcon

dolcon_SOURCES=	main.c \
		progressbar.c \
		progressbar.h

EXTRA_DIST=mainwnd.desc inpdialog.desc pref.desc reslist.desc dolcon.desktop

BUILT_SOURCES=mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

main.c: mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

localedir=$(datadir)/locale
dolcon_LDFLAGS=	@GTK2_LDADD@ @LIBXML_LDADD@
dolcon_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_CPPFLAGS=@GTK2_CFLAGS@ @LIBXML_CFLAGS@ \
		-DLOCALEDIR=\"$(localedir)\"

.desc.gtk: $(top_srcdir)/common/makegdesc
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< | $(top_srcdir)/common/makegdesc >$@

mainwnd.desc: ../../config.h

appdir=$(datadir)/applications
app_DATA=dolcon.desktop
