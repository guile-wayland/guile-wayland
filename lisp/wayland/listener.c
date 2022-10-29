#include <libguile.h>
#include <wayland-server-core.h>
#include <wayland-util.h>

SCM_DEFINE_PUBLIC (scm_wl_listener_link, "wl-listener-link", 1, 0, 0,
                   (SCM listener), "")
{
  struct wl_list *list
      = &((struct wl_listener *)(scm_call_1 (
              scm_c_private_ref ("wayland listener", "unwrap-wl-listener"),
              listener)))
             ->link;
  return scm_call_1 (scm_c_private_ref ("wayland list", "wrap-wl-list"),
                     (scm_from_pointer ((list), NULL)));
}

void
scm_init_wl_listener (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "listener.x"
#endif
}
