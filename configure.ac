#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([guile-wayland], [0.0.2],
                         [z572@z572.online],
                         [guile-wayland],
                         [https://github.com/guile-wayland/guile-wayland])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
                 modules/wayland/Makefile
                 modules/wayland/config.scm
                 libguile-wayland/Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([example/wl-client-1.scm], [chmod +x example/wl-client-1.scm])
AC_CONFIG_FILES([example/wl-client-2.scm], [chmod +x example/wl-client-2.scm])
AC_CONFIG_FILES([example/wl-client-3.scm], [chmod +x example/wl-client-3.scm])
AC_CONFIG_FILES([example/wl-server-1.scm], [chmod +x example/wl-server-1.scm])

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
PKG_CHECK_VAR([WAYLAND_LIBDIR],[wayland-server],[libdir])
PKG_CHECK_VAR([WAYLAND_DATAROOTDIR],[wayland-server],[datarootdir])
PKG_CHECK_VAR([WAYLAND_SCANNER],[wayland-scanner],[wayland_scanner])
PKG_CHECK_VAR([WAYLAND_PKGDATADIR],[wayland-scanner],[pkgdatadir])
PKG_CHECK_VAR([WAYLAND_PROTOCOLS_DATAROOTDIR],[wayland-protocols],[datarootdir])
AC_SUBST([WAYLAND_DATAROOTDIR])
AC_SUBST([WAYLAND_LIBDIR])
AC_SUBST([WAYLAND_PKGDATADIR])
AC_SUBST([WAYLAND_PROTOCOLS_DATAROOTDIR])
LT_INIT([dlopen])

AC_CHECK_HEADERS([wayland-server.h])
AC_CHECK_HEADERS([libguile.h])
GUILE_MODULE_REQUIRED([bytestructures guile])
GUILE_MODULE_REQUIRED([bytestructure-class])
AC_OUTPUT
