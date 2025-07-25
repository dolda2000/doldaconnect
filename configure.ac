AC_INIT([doldaconnect], [1.4])
AC_CONFIG_MACRO_DIRS(m4)
AC_CONFIG_SRCDIR(daemon/main.c)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS(config.h)
AC_USE_SYSTEM_EXTENSIONS

DOLDA_AC_GROUP([Checking build chain])

AC_PROG_CC
AC_PROG_CC_C_O
AM_PROG_CC_C_O
AC_PROG_INSTALL

DOLDA_AC_GROUP([Configuring gettext])

AM_GNU_GETTEXT_VERSION(0.12.1)
AM_GNU_GETTEXT([external])

DOLDA_AC_GROUP([Configuring libtool])

LT_INIT

experimental=no

DOLDA_AC_GROUP([Checking optional libraries])

# Standard libraries
AC_CHECK_LIB(z, deflate, [HAS_LIBZ=yes], [HAS_LIBZ=no])
AC_CHECK_LIB(bz2, BZ2_bzWriteOpen, [HAS_LIBBZ2=yes], [HAS_LIBBZ2=no])
AC_CHECK_LIB(gdbm, gdbm_open, [HAS_GDBM=yes], [HAS_GDBM=no])

# PAM check
pam_msg=no
AH_TEMPLATE(HAVE_PAM, [define to compile support for PAM authentication])
AC_ARG_WITH(pam, [  --with-pam              Enable PAM support])
DOLDA_PKG([HAS_PAM], [test "$with_pam" = no && HAS_PAM=no],
		     [AC_CHECK_LIB(pam, pam_start, [:], [HAS_PAM=no])],
		     [PAM_LIBS=-lpam])
if test "$with_pam" = yes -a "$HAS_PAM" = no; then
	AC_MSG_ERROR([*** cannot find PAM on this system])
fi
if test "$HAS_PAM" = yes; then
	AC_DEFINE(HAVE_PAM)
	pam_msg=yes
fi
AC_SUBST(PAM_LIBS)

# Gtk 2.0 check
AC_ARG_WITH(gtk2, [  --with-gtk2             Enable GTK2 support])
DOLDA_PKG([HAS_GTK2], [test "$with_gtk2" = no && HAS_GTK2=no],
		      [PKG_CHECK_MODULES(GTK2, gtk+-2.0, [], [HAS_GTK2=no])],
		      [AC_CHECK_LIB(gtk-x11-2.0, gtk_init, [:], [HAS_GTK2=no], $GTK2_LIBS)],
		      [DOLDA_CHECK_HEADER(gtk/gtk.h, [], [HAS_GTK2=no], $GTK2_CFLAGS)])
if test "$with_gtk2" = yes -a "$HAS_GTK2" = no; then
	AC_MSG_ERROR([*** cannot find GTK2 on this system])
fi
DOLDA_PKG([HAS_GTK2SI], [test "$HAS_GTK2" = no && HAS_GTK2SI=no],
			[DOLDA_CHECK_FUNC(gtk_status_icon_new_from_pixbuf, [], [HAS_GTK2SI=no], $GTK2_CFLAGS, $GTK2_LIBS)])
DOLDA_PKG([HAS_GTK2ASS], [test "$HAS_GTK2" = no && HAS_GTK2ASS=no],
			 [DOLDA_CHECK_FUNC(gtk_assistant_new, [], [HAS_GTK2ASS=no], $GTK2_CFLAGS, $GTK2_LIBS)])

# libxml2 check
AC_ARG_WITH(libxml2, [  --with-libxml2          Enable libxml2 support])
DOLDA_PKG([HAS_LIBXML], [test "$with_libxml2" = no && HAS_LIBXML=no],
		      	[PKG_CHECK_MODULES(LIBXML, libxml-2.0, [], [HAS_LIBXML=no])],
		       	[AC_CHECK_LIB(xml2, xmlReadFile, [:], [HAS_LIBXML=no], $LIBXML_LIBS)],
		       	[DOLDA_CHECK_HEADER(libxml/parser.h, [], [HAS_LIBXML=no], $LIBXML_CFLAGS)])
if test "$with_libxml" = yes -a "$HAS_LIBXML" = no; then
   	AC_MSG_ERROR([*** cannot find libxml2 on this system])
fi

# libnotify check
AH_TEMPLATE(HAVE_NOTIFY, [define to compile support for libnotify])
AC_ARG_WITH(libnotify, [  --with-libnotify        Enable libnotify support])
DOLDA_PKG([HAS_LIBNOTIFY], [test "$with_libnotify" = no && HAS_LIBNOTIFY=no],
			   [PKG_CHECK_MODULES(LIBNOTIFY, libnotify, [], [HAS_LIBNOTIFY=no])],
			   [AC_CHECK_LIB(notify, notify_init, [:], [HAS_LIBNOTIFY=no], $LIBNOTIFY_LIBS)])
if test "$with_libnotify" = yes -a "$HAS_LIBNOTIFY" = no; then
   	AC_MSG_ERROR([*** cannot find libnotify on this system])
fi
if test "$HAS_LIBNOTIFY" = yes; then
   	AC_DEFINE(HAVE_NOTIFY)
fi

# libattr check
AH_TEMPLATE(HAVE_XATTR, [define to compile support for extended attributes])
AC_ARG_WITH(xattr, [  --with-xattr            Enable XATTR support])
DOLDA_PKG([HAS_XATTR], [test "$with_xattr" = no && HAS_XATTR=no],
		       [AC_CHECK_LIB(attr, getxattr, [:], [HAS_XATTR=no])],
		       [DOLDA_CHECK_HEADER(attr/xattr.h, [], [HAS_XATTR=no])],
		       [XATTR_LIBS=-lattr])
if test "$with_xattr" = yes -a "$HAS_XATTR" = no; then
   	AC_MSG_ERROR([*** cannot find xattr support on this system])
fi
if test "$HAS_XATTR" = yes; then
   	AC_DEFINE(HAVE_XATTR)
fi
AC_SUBST(XATTR_LIBS)

# libpanelapplet check
DOLDA_PKG([HAS_LIBPANELAPPLET], [PKG_CHECK_MODULES(PANELAPPLET, libpanelapplet-2.0, [], [HAS_LIBPANELAPPLET=no])])

# Gaim/Pidgin check
DOLDA_PKG([HAS_LIBGAIM], [PKG_CHECK_MODULES(GAIM, gaim, [], [HAS_LIBGAIM=no])],
			 [DOLDA_CHECK_HEADER(plugin.h, [], [HAS_LIBGAIM=no], $GAIM_CFLAGS)])
DOLDA_PKG([HAS_LIBPURPLE], [PKG_CHECK_MODULES(PURPLE, purple, [], [HAS_LIBPURPLE=no])],
			   [DOLDA_CHECK_HEADER(plugin.h, [], [HAS_LIBPURPLE=no], $PURPLE_CFLAGS)])

# Daemon check
DOLDA_ENABLE(daemon, [  --enable-daemon         Enable the daemon], yes,
		     [HAS_LIBZ HAS_LIBBZ2 HAS_GDBM])
AM_CONDITIONAL(DAEMON, test "$enable_daemon" = yes)

# ADC check
DOLDA_ENABLE(fnetadc, [  --enable-fnetadc        Enable ADC code (experimental)], no,
		  [enable_daemon])
if test "$enable_fnetadc" = yes; then
   	experimental=yes
fi
AM_CONDITIONAL(ADC, test "$enable_fnetadc" = yes)

# Gtk GUI check
DOLDA_ENABLE(gtk2ui, [  --enable-gtk2ui         Enable the GTK2 user interface], yes,
		     [HAS_GTK2 HAS_LIBBZ2 HAS_LIBXML])
AM_CONDITIONAL(CLI_GTK2, test "$enable_gtk2ui" = yes)

# Dolconf check
DOLDA_ENABLE(dolconf, [  --enable-dolconf        Build the configuration helper], yes,
		      [HAS_GTK2], [HAS_GTK2ASS])
AM_CONDITIONAL(DOLCONF, test "$enable_dolconf" = yes)

# GUI shell check
DOLDA_ENABLE(guishell, [  --enable-guishell       Build the GUI shell programs], yes,
		       [HAS_GTK2], [HAS_GTK2SI])
AM_CONDITIONAL(GUISHELL, test "$enable_guishell" = yes)

# Gnome applet check
DOLDA_ENABLE(gnomeapplet, [  --enable-gnomeapplet    Enable GNOME transfer applet], no,
			  [HAS_LIBPANELAPPLET])
AM_CONDITIONAL(CLI_GNOMEAPPLET, test "$enable_gnomeapplet" = yes)

# Gaim plugin check
gaimplugin_msg=
DOLDA_ENABLE(gaimplugin, [  --enable-gaimplugin     Enable Gaim chat plugin], no,
			 [HAS_LIBGAIM])
if test "$enable_gaimplugin" = yes; then
	gaimplugin_msg="Gaim $gaimplugin_msg"
fi
AM_CONDITIONAL(CLI_GAIM, test "$enable_gaimplugin" = yes)
DOLDA_ENABLE(pidginplugin, [  --enable-pidginplugin   Enable Pidgin chat plugin], no,
			   [HAS_LIBPURPLE])
if test "$enable_pidginplugin" = yes; then
	gaimplugin_msg="Pidgin $gaimplugin_msg"
fi
AM_CONDITIONAL(CLI_PIDGIN, test "$enable_pidginplugin" = yes)
if test -z "$gaimplugin_msg"; then
   	gaimplugin_msg=none
fi

# Guile check (XXX: Shouldn't have to be enabled manually)
enable_guile=no
if test "$with_guile" = yes; then
	GUILE_FLAGS
	enable_guile=yes
fi
AM_CONDITIONAL(ELIB_GUILE, test "$enable_guile" = yes)

# Check whether to install baseconv
AC_ARG_ENABLE(baseconv, [  --enable-baseconv       Install the baseconv utility])
AM_CONDITIONAL(BASECONV, test "$enable_baseconv" = yes)

# Kerberos check
krb_msg=no
AH_TEMPLATE(HAVE_KRB5, [define to compile support for Kerberos 5 (not GSSAPI) authentication])
AC_ARG_WITH(krb5, [  --with-krb5[=PATH]        Enable Kerberos 5 (not GSSAPI) authentication])
AC_CHECK_PROG(KRBCONF, krb5-config, yes, no)
DOLDA_PKG([HAS_COMERR], [AC_CHECK_HEADER(com_err.h, [COMERR_CFLAGS=""; HAS_COMERR=yes], [])],
			[AC_CHECK_HEADER(et/com_err.h, [COMERR_CFLAGS="-I/usr/include/et"; HAS_COMERR=yes], [])],
			[HAS_COMERR=no])
DOLDA_PKG([HAS_KRB5], [test "$with_krb5" = no && HAS_KRB5=no],
		      [test "$KRBCONF" != yes && HAS_KRB5=no],
		      [test "$HAS_COMERR" = no && HAS_KRB5=no],
		      [KRB5_LIBS="`krb5-config --libs krb5`"
		       KRB5_CFLAGS="`krb5-config --cflags krb5` $COMERR_CFLAGS"]
		      [AC_CHECK_LIB(krb5, krb5_init_context, [:], [HAS_KRB5=no], $KRB5_LIBS)],
		      [DOLDA_CHECK_HEADER(krb5.h, [], [HAS_KRB5=no], $KRB5_CFLAGS)])
if test -n "$with_krb5" -a "$with_krb5" != no -a "$HAS_KRB5" = no; then
	AC_MSG_ERROR([*** cannot find Kerberos 5 on this system])
fi
if test "$with_krb5" != no -a "$HAS_KRB5" = yes; then
	AC_DEFINE(HAVE_KRB5)
	krb_msg=yes
fi
AC_SUBST(KRB5_LIBS)
AC_SUBST(KRB5_CFLAGS)

# Linux keyring
AH_TEMPLATE(HAVE_KEYUTILS, [define if your system supports the Linux keyring functions])
AC_ARG_WITH(keyutils, [  --with-keyutils           Enable support for the Linux keyring stash])
DOLDA_PKG([HAS_KEYUTILS], [test "$with_keyutils" = no && HAS_KEYUTILS=no],
			  [AC_CHECK_LIB(keyutils, keyctl_search, [:], [HAS_KEYUTILS=no])])
if test "$with_keyutils" = yes -a "$HAS_KEYUTILS" = no; then
   	AC_MSG_ERROR([*** cannot find keyutils on this system])
fi
if test "$HAS_KEYUTILS" = yes; then
   	AC_DEFINE(HAVE_KEYUTILS)
	KEYUTILS_LIBS=-lkeyutils
fi
AC_SUBST([KEYUTILS_LIBS])

DOLDA_AC_GROUP([Checking operating system features])

# Misc. functions checks
AC_CHECK_FUNC(vswprintf, , AC_MSG_ERROR([*** must have vswprintf]))
AH_TEMPLATE(HAVE_WCSCASECMP, [define if your system implements wcscasecmp])
AC_CHECK_FUNC(wcscasecmp, [ AC_DEFINE(HAVE_WCSCASECMP) ])

AH_TEMPLATE(HAVE_LINUX_SOCKIOS_H, [define if you have linux/sockios.h on your system])
AC_CHECK_HEADER([linux/sockios.h], [ AC_DEFINE(HAVE_LINUX_SOCKIOS_H) ])

AH_TEMPLATE(HAVE_IPV6, [define if your system supports IPv6 and you wish to compile with support for it])
AC_CHECK_MEMBER(struct sockaddr_in6.sin6_family, [ AC_DEFINE(HAVE_IPV6) ], , [#include <netinet/in.h>])

AH_TEMPLATE(HAVE_RESOLVER, [define if your system supports the res_* functions to fetch DNS RRs])
AC_CHECK_LIB(resolv, res_query, [ AC_DEFINE(HAVE_RESOLVER)
				  LDFLAGS="$LDFLAGS -lresolv" ])
AC_CHECK_LIB(resolv, __res_query, [ AC_DEFINE(HAVE_RESOLVER)
		     		    LDFLAGS="$LDFLAGS -lresolv" ])

# Unix credentials selector
AH_TEMPLATE(UNIX_AUTH_STYLE, [undefine for no Unix auth, 1 for Linux style, 2 for BSD style])
AC_CHECK_MEMBER(struct ucred.pid, [ linuxcreds=y ], [ linuxcreds=n ], [#include <sys/socket.h>])
AC_CHECK_FUNC(getpeereid, [ bsdcreds=y ], [ bsdcreds=n ])
AC_MSG_CHECKING([for Unix auth style])
if test $linuxcreds = y; then
	AC_DEFINE(UNIX_AUTH_STYLE, 1)
	AC_MSG_RESULT(linux)
elif test $bsdcreds = y; then
	AC_DEFINE(UNIX_AUTH_STYLE, 2)
	AC_MSG_RESULT(bsd)
else
	AC_MSG_RESULT(none)
fi

DOLDA_AC_GROUP([Checking standard headers])

AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT

AC_TYPE_PID_T
AC_TYPE_SIZE_T

CPPFLAGS="-I\$(top_srcdir)/include $CPPFLAGS"

DOLDA_AC_GROUP([Writing output])

AH_TEMPLATE(RELEASEINFO, [define release information reported by various programs (should be left to configure to define)])
relinfo="relver ${VERSION}\\n"
relinfo="${relinfo}builddate $(date)\\n"
if test -d .git && which git-rev-parse >/dev/null 2>&1; then
   	relinfo="${relinfo}source git $(git-rev-parse HEAD)\\n"
elif test -r source; then
	relinfo="${relinfo}source $(cat source)\\n"
else
	relinfo="${relinfo}source unknown\\n"
fi
AC_DEFINE_UNQUOTED([RELEASEINFO], ["$relinfo"])

AC_CONFIG_FILES([
Makefile
autopackage/Makefile
common/Makefile
daemon/Makefile
lib/Makefile
lib/guile/Makefile
lib/guile/dolcon/Makefile
clients/Makefile
clients/gtk2/Makefile
clients/tty/Makefile
clients/gnome-trans-applet/Makefile
clients/gaim/Makefile
clients/gui-shell/Makefile
include/Makefile
include/doldaconnect/Makefile
doc/Makefile
doc/man/Makefile
po/Makefile.in
config/Makefile
config/cmd/Makefile
config/util/Makefile
contrib/Makefile
contrib/doldaconnect.spec
share/Makefile
autopackage/doldacond.apspec
autopackage/dolcon.apspec
autopackage/dcuilib.apspec
autopackage/dcguile.apspec
autopackage/guishell.apspec
])
AC_OUTPUT

echo
echo "Dolda Connect has been configured with the following settings:"
echo
echo "    Daemon:                     $enable_daemon"
echo "    PAM support:                $pam_msg"
echo "    Kerberos 5 support:         $krb_msg"
echo "    ADC support (unfinished):   $enable_fnetadc"
echo "    GTK2 user interface:        $enable_gtk2ui"
echo "    Dolconf configurator:       $enable_dolconf"
echo "    GUI shell:                  $enable_guishell"
echo "    Guile extension library:    $enable_guile"
echo "    GNOME transfer applet:      $enable_gnomeapplet"
echo "    Gaim chat plugin:           $gaimplugin_msg"
echo

if tput bold >/dev/null 2>&1 && tty <&2 >/dev/null 2>&1; then
	hastput=y
fi
if test "$HAS_GTK2" = no -a "$with_gtk2" != no -a "$enable_gtk2ui" != no; then
	if test "$hastput" = y; then
		tput bold
		tput setf 4 2>/dev/null
	fi
	echo -n "    Warning: " >&2
	if test "$hastput" = y; then
		tput sgr0
	fi
	echo "Could not find a GTK2 development installation on this system." >&2
	echo "             That means you won't get a UI." >&2
	echo "             Make absolutely sure this is what you want!" >&2
	if test "$hastput" = y; then
		tput bel
	fi
	sleep 1
fi
if test "$experimental" = yes; then
	if test "$hastput" = y; then
		tput bold
		tput setf 4 2>/dev/null
	fi
	echo -n "    Warning: " >&2
	if test "$hastput" = y; then
		tput sgr0
	fi
	echo "You have enabled one or more experimental features!" >&2
	echo "             Please don't complain that it doesn't work, unless" >&2
	echo "             you have something constructive to add about the situation." >&2
	if test "$hastput" = y; then
		tput bel
	fi
	sleep 1
fi
