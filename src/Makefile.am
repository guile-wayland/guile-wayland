AUTOMAKE_OPTIONS = gnu
AM_CPPFLAGS = ${CFLAGS} -I/usr/include

lib_LTLIBRARIES = libguile-wayland.la
libguile_wayland_la_SOURCES = w.c
libguile_wayland_la_LIBADD = -lwayland-server ${LIBS}
libguile_wayland_la_LDFLAGS = -export-dynamic
libguile_wayland_la_DEPENDENCIES = ${LTLIBOBJS}
