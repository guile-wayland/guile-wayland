GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(libguile_wayland_la_CFLAGS)
BUILT_SOURCES= $(libguile_wayland_la_SOURCES:%.c=%.x)
# $(libguile_wayland_la_SOURCES): $(libguile_wayland_la_SOURCES:%.c=%.x)
.c.x:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env guile-snarf -o $@ $< $(snarfcppopts)
moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/wayland
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/wayland
libguile_wayland_la_CFLAGS=$(WAYLAND_SERVER_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD=$(WAYLAND_SERVER_LIBS) $(GUILE_LIBS)
lib_LTLIBRARIES = libguile-wayland.la
$(SOURCES):  $(lib_LTLIBRARIES)
libguile_wayland_la_SOURCES=signal.c listener.c
SOURCES = ../wayland.scm \
 base.scm            \
 argument.scm        \
 callback.scm        \
 client.scm          \
 client-core.scm     \
 compositor.scm      \
 config.scm          \
 cursor.scm          \
 display.scm         \
 egl.scm             \
 event-loop.scm      \
 global.scm          \
 interface.scm       \
 keyboard.scm        \
 list.scm            \
 listener.scm        \
 output.scm          \
 pointer.scm         \
 proxy.scm           \
 registry.scm        \
 resource.scm        \
 server-core.scm     \
 shm.scm             \
 signal.scm          \
 touch.scm           \
 util.scm
