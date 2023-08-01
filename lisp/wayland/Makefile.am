GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES)

GUILE_WARNINGS = -W3

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) \
	$(GUILE_WARNINGS) -o "$@" "$<"

snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(libguile_wayland_la_CFLAGS)

BUILT_SOURCES= $(libguile_wayland_la_SOURCES:%.c=%.x)

.c.x:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env guile-snarf -o $@ $< $(snarfcppopts)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/wayland

godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/wayland

libguile_wayland_la_CFLAGS=$(WAYLAND_SERVER_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD=$(WAYLAND_SERVER_LIBS) $(GUILE_LIBS)
libguile_wayland_la_SOURCES=listener.c

lib_LTLIBRARIES = libguile-wayland.la

$(SOURCES):  $(lib_LTLIBRARIES)

SOURCES = ../wayland.scm \
 base.scm            \
 argument.scm        \
 client.scm          \
 config.scm          \
 cursor.scm          \
 display.scm         \
 server/display.scm  \
 server/client.scm   \
 server/global.scm   \
 egl.scm             \
 event-loop.scm      \
 global.scm          \
 interface.scm       \
 keyboard.scm        \
 protocol.scm        \
 list.scm            \
 listener.scm        \
 output.scm          \
 pointer.scm         \
 proxy.scm           \
 registry.scm        \
 resource.scm        \
 shm.scm             \
 signal.scm          \
 util.scm
protocol.scm:
	$(top_srcdir)/build-aux/wayland-scanner-guile > $@
