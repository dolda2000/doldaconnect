bin_PROGRAMS=locktouch tthsum dolconf
if baseconv
bin_PROGRAMS+=baseconv
else
noinst_PROGRAMS=baseconv
endif
libexec_PROGRAMS=speedrec

speedrec_SOURCES=speedrec.c
locktouch_SOURCES=locktouch.c
tthsum_SOURCES=tthsum.c
dolconf_SOURCES=dolconf.c
baseconv_SOURCES=baseconv.c
BUILT_SOURCES=dolconf-assistant.gtk dolconf-wnd.gtk

AM_CPPFLAGS=-I$(top_srcdir)/include
tthsum_LDADD=$(top_srcdir)/common/libcommon.a
baseconv_LDADD=$(top_srcdir)/common/libcommon.a

dolconf.c: dolconf-assistant.desc dolconf-wnd.desc
localedir=$(datadir)/locale
dolconf_LDFLAGS=	$(shell pkg-config --libs gtk+-2.0) \
			$(shell pkg-config --libs libxml-2.0)
dolconf_CPPFLAGS=	$(shell pkg-config --cflags gtk+-2.0) \
			$(shell pkg-config --cflags libxml-2.0) \
			-DLOCALEDIR=\"$(localedir)\"
dolconf_LDADD=		$(top_srcdir)/common/libcommon.a

%.gtk: %.desc $(top_srcdir)/common/makegdesc
	$(top_srcdir)/common/makegdesc $< >$@

EXTRA_DIST=doldacond.conf dc-filter dc-filtercmd dolconf-assistant.desc dolconf-wnd.desc

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(sysconfdir); \
	for file in doldacond.conf; do \
		if [ ! -e $(DESTDIR)$(sysconfdir)/$$file ]; then \
			$(INSTALL_DATA) $(srcdir)/$$file $(DESTDIR)$(sysconfdir)/$$file; \
		fi; \
	done; \
	for file in dc-filter dc-filtercmd; do \
		if [ ! -e $(DESTDIR)$(sysconfdir)/$$file ]; then \
			$(INSTALL) $(srcdir)/$$file $(DESTDIR)$(sysconfdir)/$$file; \
		fi; \
	done
