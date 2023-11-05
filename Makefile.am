CLEANFILES = $(GOBJECTS) $(TESTS:tests/%.scm=%.log)

EXTRA_DIST =                                    \
  $(SOURCES)                                    \
  $(NOCOMP_SOURCES)                             \
  $(top_srcdir)/build-aux/test-driver.scm       \
  $(TESTS)

AM_SCM_LOG_FLAGS = --no-auto-compile

TESTS = tests/display.scm tests/list.scm tests/scanner.scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

TEST_EXTENSIONS = .scm

SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
  $(top_srcdir)/build-aux/test-driver.scm

SUBDIRS = libguile-wayland modules/wayland
