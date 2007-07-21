bin_PROGRAMS=
if DOLCONF
bin_PROGRAMS+=dolconf
endif
if BASECONV
bin_PROGRAMS+=baseconv
else
noinst_PROGRAMS=baseconv
endif

dolconf_SOURCES=dolconf.c
baseconv_SOURCES=baseconv.c
BUILT_SOURCES=dolconf-assistant.gtk dolconf-wnd.gtk

AM_CPPFLAGS=-I$(top_srcdir)/include
baseconv_LDADD=$(top_srcdir)/common/libcommon.a

dolconf.c: dolconf-assistant.desc dolconf-wnd.desc
localedir=$(datadir)/locale
dolconf_LDFLAGS=	@GTK2_LIBS@
dolconf_CPPFLAGS=	@GTK2_CFLAGS@ \
			-DLOCALEDIR=\"$(localedir)\"
dolconf_LDADD=		$(top_srcdir)/common/libcommon.a $(top_srcdir)/lib/libdcui.la

.desc.gtk: $(top_srcdir)/common/makegdesc
	$(top_srcdir)/common/makegdesc $< >$@

EXTRA_DIST=dolconf-assistant.desc dolconf-wnd.desc
