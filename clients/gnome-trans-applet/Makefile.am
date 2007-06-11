libexec_PROGRAMS=dolcon-trans-applet

dolcon_trans_applet_SOURCES=	dolcon-trans-applet.c \
				conduit-pipe.c \
				conduit-dclib.c \
				conduit.c \
				conduit.h

localedir=$(datadir)/locale
dolcon_trans_applet_LDFLAGS=$(shell pkg-config --libs libpanelapplet-2.0)
dolcon_trans_applet_LDADD=$(top_srcdir)/lib/libdcui.la
dolcon_trans_applet_CPPFLAGS=$(shell pkg-config --cflags libpanelapplet-2.0) -DLOCALEDIR=\"$(localedir)\"

BUILT_SOURCES=Dolcon_Transferapplet_Factory.server

serverdir=$(libdir)/bonobo/servers
server_DATA=Dolcon_Transferapplet_Factory.server

EXTRA_DIST=Dolcon_Transferapplet_Factory.server.in

SUFFIXES=.server .server.in
.server.in.server:
	sed -e "s|\@LIBEXECDIR\@|$(libexecdir)|" $< > $@
