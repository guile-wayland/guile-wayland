#include <libguile.h>
#include <wayland-server-core.h>

SCM
a_wrapper (SCM x)
{
  return scm_fr;
}

void
init_wayland (void)
{
  scm_c_define_gsubr ("wl-event-loop-create" ,0,0,0 , a_wrapper)
}
