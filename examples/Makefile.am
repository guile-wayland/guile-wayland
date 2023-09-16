EXAMPLES = \
	wl-client-1.scm.in \
	wl-client-2.scm.in \
    wl-client-3.scm.in \
	wl-server-1-scm.in

EXTRA_DIST= \
	wl-client-1.scm.in \
	wl-client-2.scm.in \
    wl-client-3.scm.in \
	wl-server-1-scm.in

SOURCES = \
	wl-client-1.scm.in \
	wl-client-2.scm.in \
    wl-client-3.scm.in \
	wl-server-1-scm.in

examplesdir = $(pkgdatadir)/examples
dist_examples_DATA = wl-client-1.scm wl-client-2.scm wl-client-3.scm wl-server-1.scm

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

SUFFIXES = .in

.in:
	@echo "  GEN      $@"
	@$(MKDIR_P) "`dirname "$@"`"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"
