EXTRA_DIST = makegdesc

noinst_LIBRARIES = libcommon.a libhttp.a

libcommon_a_SOURCES =	tiger.c \
			utils.c

libhttp_a_SOURCES =	http.c

libcommon_a_CFLAGS = -fPIC
libhttp_a_CFLAGS = -fPIC

AM_CPPFLAGS=-I$(top_srcdir)/include
