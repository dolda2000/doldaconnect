if DAEMON
bin_PROGRAMS=doldacond
endif
doldacond_SOURCES=	main.c \
			search.c \
			search.h \
			transfer.c \
			transfer.h \
			sysevents.h \
			module.h \
			filenet.c \
			filenet.h \
			fnet-dc.c \
			fnet-adc.c \
			auth.c \
			auth.h \
			auth-pam.c \
			auth-krb5.c \
			auth-unix.c \
			client.c \
			client.h \
			net.c \
			net.h \
			log.c \
			ui.c \
			conf.c \
			conf.h

EXTRA_DIST=emacs-local
doldacond_LDADD=$(top_srcdir)/common/libcommon.a \
		@KRB5_LIBS@ -lbz2 -lz -lgdbm @PAM_LIBS@
doldacond_CPPFLAGS=-I$(top_srcdir)/include -DDAEMON @KRB5_CFLAGS@
