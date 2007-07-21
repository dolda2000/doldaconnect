if CLI_GNOMEAPPLET
libexec_PROGRAMS=dolcon-trans-applet
server_DATA=Dolcon_Transferapplet_Factory.server
endif

dolcon_trans_applet_SOURCES=	dolcon-trans-applet.c \
				conduit-pipe.c \
				conduit-dclib.c \
				conduit.c \
				conduit.h

localedir=$(datadir)/locale
dolcon_trans_applet_LDFLAGS=@PANELAPPLET_LIBS@
dolcon_trans_applet_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_trans_applet_CPPFLAGS=@PANELAPPLET_CFLAGS@ -DLOCALEDIR=\"$(localedir)\"

BUILT_SOURCES=Dolcon_Transferapplet_Factory.server

serverdir=$(libdir)/bonobo/servers

EXTRA_DIST=Dolcon_Transferapplet_Factory.server.in

SUFFIXES=.server .server.in
.server.in.server:
	sed -e "s|\@LIBEXECDIR\@|$(libexecdir)|" $< > $@
