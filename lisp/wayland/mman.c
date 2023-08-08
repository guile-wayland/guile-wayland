#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>
#include <libguile.h>
#define define_public(x,xx) scm_c_define (x , xx); scm_c_export(x);

void
scm_init_mman(void) {

#ifdef PROT_NONE
  define_public ("PROT_NONE", scm_from_int (PROT_NONE));
#endif
#ifdef PROT_READ
  define_public ("PROT_READ", scm_from_int (PROT_READ));
#endif
#ifdef PROT_WRITE
  define_public ("PROT_WRITE", scm_from_int (PROT_WRITE));
#endif
#ifdef PROT_EXEC
  define_public ("PROT_EXEC", scm_from_int (PROT_EXEC));
#endif

#ifdef MAP_ANONYMOUS
  define_public ("MAP_ANONYMOUS", scm_from_int (MAP_ANONYMOUS));
#endif
#ifdef MAP_ANON
  define_public ("MAP_ANON", scm_from_int (MAP_ANON));
#endif
#ifdef MAP_FILE
  define_public ("MAP_FILE", scm_from_int (MAP_FILE));
#endif
#ifdef MAP_FIXED
  define_public ("MAP_FIXED", scm_from_int (MAP_FIXED));
#endif
#ifdef MAP_HASSEMAPHORE
  define_public ("MAP_HASSEMAPHORE", scm_from_int (MAP_HASSEMAPHORE));
#endif
#ifdef MAP_PRIVATE
  define_public ("MAP_PRIVATE", scm_from_int (MAP_PRIVATE));
#endif
#ifdef MAP_SHARED
  define_public ("MAP_SHARED", scm_from_int (MAP_SHARED));
#endif
#ifdef MAP_NOCACHE
  define_public ("MAP_NOCACHE", scm_from_int (MAP_NOCACHE));
#endif
  define_public ("PAGE_SIZE", scm_from_int (sysconf (_SC_PAGESIZE)));
#ifdef MS_ASYNC
  define_public ("MS_ASYNC", scm_from_int (MS_ASYNC));
#endif
#ifdef MS_SYNC
  define_public ("MS_SYNC", scm_from_int (MS_SYNC));
#endif
#ifdef MS_INVALIDATE
  define_public ("MS_INVALIDATE", scm_from_int (MS_INVALIDATE));
#endif
#ifndef SCM_MAGIC_SNARFER
#include "mman.x"
#endif
}
