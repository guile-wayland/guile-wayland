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

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/wayland

godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/wayland

$(GOBJECTS): $(lib_LTLIBRARIES)

SOURCES =            \
 base.scm            \
 argument.scm        \
 config.scm          \
 cursor.scm          \
 client/display.scm  \
 client/proxy.scm    \
 server/display.scm  \
 server/client.scm   \
 server/global.scm   \
 server/resource.scm \
 server/listener.scm \
 server/event-loop.scm      \
 scanner.scm         \
 server/protocol/wayland.scm \
 client/protocol/wayland.scm \
 client/protocol/viewporter.scm \
 client/protocol/xdg-shell.scm \
 egl.scm             \
 interface.scm       \
 list.scm            \
 shm.scm             \
 signal.scm          \
 util.scm
