AC_DEFUN([DOLDA_AC_GROUP],
[AC_MSG_NOTICE([ -- $1 -- ])])

m4_defun([_DOLDA_PKG_LOOP],[dnl
if test -z "$[]$1"; then
   	$2
fi
ifelse(m4_eval([$# > 2]), 1, [_DOLDA_PKG_LOOP($1, m4_shift(m4_shift($@)))])
])

# DOLDA_PKG(VARIABLE-NAME, COMMANDS...)
#
# Evaluate COMMANDS in sequence as long as ${VARIABLE-NAME} is empty

AC_DEFUN([DOLDA_PKG],[dnl
$1=""
_DOLDA_PKG_LOOP($1, m4_shift($@))
if test "$[]$1" != no; then $1=yes; fi
])

# DOLDA_CHECK_HEADER(FILE, ACTION-IF-FOUND, ACTION-IF-NOT-FOUND,
#       [EXTRA-CFLAGS])
#
# Augmented version of AC_CHECK_HEADER that overrides CPPFLAGS

AC_DEFUN([DOLDA_CHECK_HEADER],[dnl
cpp_bak="$CPPFLAGS"
ifelse([$4], , , [CPPFLAGS="$CPPFLAGS $4"])
AC_CHECK_HEADER($1, $2, $3)
CPPFLAGS="$cpp_bak"
])

# DOLDA_ENABLE(NAME, HELP, DEFAULT, DEPS)
#
# DEPS is a space-separated listing of required variables that must be
# `yes'

AC_DEFUN([DOLDA_ENABLE],[dnl
AC_ARG_ENABLE([$1], [$2])
if test "[$enable_][$1]" = yes; then
   	for var in [$4]; do
	    	if test "${!var}" != yes; then
		   	AC_MSG_ERROR([*** cannot enable $1 without $var])
		fi
	done
else
	ifelse([$3], yes, [dnl
	[enable_][$1]=yes
	for var in [$4]; do
	    	if test "${!var}" != yes; then
		   	[enable_][$1]=no
			break
		fi
	done
], [dnl
	[enable_][$1]=no
])
fi
])
