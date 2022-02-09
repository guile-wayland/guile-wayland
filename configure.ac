#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([guile-wayland], [0.0.1], [])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_HEADERS([config.h])

AC_USE_SYSTEM_EXTENSIONS
# Checks for programs.
AC_PROG_CC

# Checks for libraries.
m4_pattern_forbid([PKG_CHECK_MODULES])

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

PKG_CHECK_MODULES([GUILE], [guile-3.0])
PKG_CHECK_MODULES([WAYLAND_SERVER], [wayland-server])
#AC_CHECK_LIB()
AC_CONFIG_FILES([Makefile src/Makefile])
# Checks for header files.
AC_CHECK_HEADERS([wayland-server.h])
AC_CHECK_HEADERS([libguile.h])
# Checks for typedefs, structures, and compiler characteristics.
LT_INIT([dlopen])
dnl LT_CONFIG_LTDL_DIR([libltdl])
dnl LTDL_INIT
# Checks for library functions.
CFLAGS=`guile-config compile`
LIBS=`guile-config link`

#AC_CHECK_LIB([guile],[scm_to_pointer])

AC_OUTPUT
