bin_PROGRAMS=doldacond
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
			auth.c \
			auth.h \
			auth-pam.c \
			auth-krb5.c \
			client.c \
			client.h \
			net.c \
			net.h \
			utils.c \
			utils.h \
			log.c \
			log.h \
			ui.c \
			conf.c \
			conf.h \
			tiger.c \
			tiger.h
EXTRA_DIST=emacs-local
doldacond_LDADD=@KRB5_LDADD@ -lbz2 -lz
