lib_LTLIBRARIES = libguile-wayland.la

libguile_wayland_la_SOURCES = server/listener.c interface.c
libguile_wayland_la_CFLAGS = $(WAYLAND_SERVER_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD = $(WAYLAND_SERVER_LIBS) $(GUILE_LIBS)
libguile_wayland_la_LDFLAGS = -module

C_FILES = $(libguile_wayland_la_SOURCES)
X_FILES = $(C_FILES:%.c=%.x)

SNARF_OPTS = $(libguile_wayland_la_CFLAGS)

$(X_FILES): %.x: %.c
	guile-snarf $(SNARF_OPTS) $< > $@

BUILT_SOURCES = $(X_FILES)
CLEANFILES = $(X_FILES)
