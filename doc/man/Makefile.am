man_MANS = doldacond.conf.5 doldacond.8
BUILT_SOURCES = doldacond.conf.5

doldacond.conf.5: doldacond.conf.5.in ../../daemon/*.c mkcvman
	./mkcvman ../../daemon/*.c <doldacond.conf.5.in >doldacond.conf.5

EXTRA_DIST =	doldacond.conf.5.in mkcvman \
		doldacond.8
