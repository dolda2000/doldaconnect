EXTRA_DIST = makegdesc

noinst_LIBRARIES = libcommon.a libhttp.a
noinst_PROGRAMS = httest

libcommon_a_SOURCES =	tiger.c \
			utils.c

libhttp_a_SOURCES =	http.c

httest_SOURCES =	httest.c
httest_LDADD =		libhttp.a libcommon.a

libcommon_a_CPPFLAGS = -D_ISOC99_SOURCE
libcommon_a_CFLAGS = -fPIC
libhttp_a_CFLAGS = -fPIC

AM_CPPFLAGS=-I$(top_srcdir)/include
