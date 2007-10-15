appdir=$(datadir)/applications
iconsdir=$(datadir)/icons/hicolor/48x48/apps

icons_DATA=

if GUISHELL
realdesktop=dolcon-shell.desktop
icons_DATA+=dolcon.png
else
if CLI_GTK2
realdesktop=dolcon-gtk.desktop
icons_DATA+=dolcon.png
else
realdesktop=none
endif
endif

EXTRA_DIST=hellodolda.png dolcon.png dolcon-gtk.desktop dolcon-shell.desktop

install-data-local:
	if [ "$(realdesktop)" != none ]; then \
		$(mkinstalldirs) $(DESTDIR)$(appdir); \
		$(INSTALL_DATA) $(srcdir)/$(realdesktop) $(DESTDIR)$(appdir)/dolcon.desktop; \
	fi
