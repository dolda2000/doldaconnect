if CLI_GTK2
bin_PROGRAMS=dolcon
endif

dolcon_SOURCES=	dolcon.c \
		dolcon.h \
		hublist.c \
		hublist.h \
		hublist-xml.c \
		hublist-old.c

EXTRA_DIST=mainwnd.desc inpdialog.desc pref.desc reslist.desc

BUILT_SOURCES=	mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

main.c: mainwnd.gtk inpdialog.gtk pref.gtk reslist.gtk

localedir=$(datadir)/locale
dolcon_LDFLAGS=	@GTK2_LIBS@ @LIBXML_LIBS@ -lbz2
dolcon_LDADD=$(top_srcdir)/lib/libdcui.la $(top_srcdir)/common/libhttp.a
dolcon_CPPFLAGS=@GTK2_CFLAGS@ @LIBXML_CFLAGS@ \
		-DLOCALEDIR=\"$(localedir)\" -D_GNU_SOURCE

.desc.gtk: $(top_srcdir)/common/makegdesc
	target="$@"; \
	basename="$${target%.gtk}"; \
	cpp $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $< \
	 | $(top_srcdir)/common/makegdesc -h "$${basename}.gtkh" >$@

mainwnd.desc: ../../config.h
