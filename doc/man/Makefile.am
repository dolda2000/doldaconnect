man_MANS =
if DAEMON
man_MANS += doldacond.conf.5 doldacond.8
endif
if CLI_GTK2
man_MANS += dolcon.1
endif
if ELIB_GUILE
man_MANS += hubmgr.1
endif
BUILT_SOURCES = doldacond.conf.5

doldacond.conf.5: doldacond.conf.5.in ../../daemon/*.c mkcvman
	./mkcvman ../../daemon/*.c <doldacond.conf.5.in >doldacond.conf.5

EXTRA_DIST =	doldacond.conf.5.in mkcvman \
		doldacond.8 dolcon.1
