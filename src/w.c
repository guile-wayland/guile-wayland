#include <libguile.h>
#include <wayland-server-core.h>



void
wl_event_loop_destroy_wrapper (SCM loop)
{
  return wl_event_loop_destroy((struct wl_event_loop*)scm_to_pointer(loop));
}

SCM
wl_event_loop_create_wrapper (void)
{
  return scm_from_pointer(wl_event_loop_create() , NULL);
}

void
init_w (void)
{
  scm_c_define_gsubr ("wl-event-loop-create" ,0,0,0 , wl_event_loop_create_wrapper);
  scm_c_define_gsubr ("wl-event-loop-destroy" ,1,0,0 , wl_event_loop_destroy_wrapper);
}
