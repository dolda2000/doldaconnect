SUBDIRS=cmd util

EXTRA_DIST=doldacond.conf dc-filter dc-filtercmd

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
