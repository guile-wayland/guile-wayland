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
  scm_call_2(l->callback,
             scm_call_1(scm_c_public_ref
                        ("wayland listener", "wrap-wl-listener"),
                        scm_from_pointer(listener,NULL)),
             scm_from_pointer(data,NULL));
}

SCM_DEFINE (scm_make_wl_listener, "make-wl-listener", 1, 0, 0, (SCM proc), "")
{
  Listener *listener = scm_gc_calloc (sizeof (Listener),"wl-listener");
  listener->callback=proc;
  listener->listener.notify = &listener_call;
  SCM s_listener = scm_call_1 (
      scm_c_public_ref ("wayland listener", "wrap-wl-listener"),
      scm_from_pointer (&listener->listener, NULL));
  return s_listener;
}

SCM_DEFINE (scm_wl_listener_remove, "wl-listener-remove", 1, 0, 0,
            (SCM listener), "")
{
  SCM listner_p = (scm_call_1 (scm_c_public_ref ("wayland listener", "unwrap-wl-listener"),
                               listener));
  struct wl_listener *wl_listener = scm_to_pointer (listner_p);
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
