AUTOMAKE_OPTIONS = gnu


snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(GUILE_CFLAGS)
BUILT_SOURCES = w.x
.SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
lib_LTLIBRARIES = libguile-wayland.la

libguile_wayland_la_SOURCES = w.c
libguile_wayland_la_LIBADD = ${WAYLAND_SERVER_LIBS} ${GUILE_LIBS}
libguile_wayland_la_LDFLAGS = -module $(GUILE_LDFLAGS)
libguile_wayland_la_DEPENDENCIES = ${LTLIBOBJS}
libguile_wayland_la_CPPFLAGS = $(CFLAGS) $(GUILE_CFLAGS)
