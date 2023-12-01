#include <libguile.h>
#include <stdlib.h>
#include <wayland-server-core.h>
#include <wayland-util.h>

typedef struct {
  struct wl_listener listener;
  SCM callback;
}  Listener;

static void listener_call (struct wl_listener *listener, void *data){
  Listener *l;
  l=wl_container_of(listener,l,listener);
  SCM slistener=scm_call_1(scm_c_public_ref
                           ("wayland server listener", "wrap-wl-listener"),
                           scm_from_pointer(listener,NULL));
  SCM signal = scm_slot_ref (slistener, scm_from_utf8_symbol ("signal"));
  SCM sdata=scm_from_pointer(data,NULL);
  if (scm_is_true(signal))
    {
      SCM data_wrap= scm_slot_ref (signal, scm_from_utf8_symbol ("data-wrapper"));
      if (scm_is_true(data_wrap))
        sdata = scm_call_1 (data_wrap, sdata);
    }
  scm_call_2(l->callback,
             slistener,
             sdata);
}

SCM_DEFINE (scm_make_wl_listener, "make-wl-listener", 1, 0, 0, (SCM proc), "")
{
  Listener *listener = scm_gc_calloc (sizeof (Listener),"wl-listener");
  listener->callback=proc;
  listener->listener.notify = &listener_call;
  SCM s_listener = scm_call_1 (
      scm_c_public_ref ("wayland server listener", "wrap-wl-listener"),
      scm_from_pointer (&listener->listener, NULL));
  scm_gc_protect_object(s_listener);
  scm_slot_set_x(s_listener, scm_from_utf8_symbol("scm-created?"), SCM_BOOL_T);
  return s_listener;
}

SCM_DEFINE (scm_wl_listener_remove, "wl-listener-remove", 1, 0, 0,
            (SCM listener), "")
{
  SCM listner_p = (scm_call_1 (scm_c_public_ref ("wayland server listener", "unwrap-wl-listener"),
                               listener));
  struct wl_listener *wl_listener = scm_to_pointer (listner_p);
  if (scm_to_bool(scm_slot_ref(listener, scm_from_utf8_symbol("scm-created?"))))
    scm_gc_unprotect_object(listener);
  wl_list_remove (&wl_listener->link);
  return SCM_UNSPECIFIED;
}

void
scm_init_wl_listener (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "listener.x"
#endif
}
