#include <wayland-server-core.h>
#include <wayland-util.h>
#include <libguile.h>

SCM_DEFINE (scm_wl_signal_add ,"wl-signal-add",2,0,0,(SCM signal ,SCM listener),"")
{
  (wl_signal_add((struct wl_signal*)(scm_to_pointer(scm_call_1(scm_c_public_ref("wayland signal","unwrap-wl-signal"),signal))),
                 (struct wl_listener*)(scm_to_pointer(scm_call_1(scm_c_public_ref("wayland listener","unwrap-wl-listener"),listener)))));
  return SCM_UNSPECIFIED;
}

void scm_init_wl_signal(void){
#ifndef SCM_MAGIC_SNARFER
#include "signal.x"
#endif
}
