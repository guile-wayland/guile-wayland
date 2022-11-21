#include "libguile/eval.h"
#include "libguile/foreign.h"
#include "libguile/gc.h"
#include "libguile/list.h"
#include "libguile/modules.h"
#include "libguile/symbols.h"
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <libguile.h>

SCM_DEFINE(scm_make_wl_listener,"%make-wl-listener",1,0,0,(SCM proc),""){
  struct wl_listener *new_listener=scm_gc_malloc(sizeof(struct wl_listener), "wl-listener");
  new_listener->notify=scm_to_pointer(scm_procedure_to_pointer(scm_c_public_ref("system foreign", "void"),
                                                proc,
                                                scm_list_2(scm_from_utf8_symbol("*"),
                                                           scm_from_utf8_symbol("*"))));
  return scm_call_1(scm_c_public_ref("wayland listener", "wrap-wl-listener"),
                    scm_from_pointer(new_listener,NULL));
}

void scm_init_wl_listener(void){
#ifndef SCM_MAGIC_SNARFER
#include "listener.x"
#endif
}
