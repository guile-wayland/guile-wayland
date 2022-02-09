#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADERS([config.h])

AC_USE_SYSTEM_EXTENSIONS
# Checks for programs.
AC_PROG_CC

# Checks for libraries.
m4_pattern_forbid([PKG_CHECK_MODULES])

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

PKG_CHECK_MODULES([GUILE], [guile-3.0 >= 3.0.3])
PKG_CHECK_MODULES([wayland-server],[wayland >= 1.18.0])
AC_CONFIG_FILES([Makefile])
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


AC_OUTPUT
