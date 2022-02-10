#include <libguile.h>

#include <wayland-server-core.h>

static void finalize_event_loop(SCM loop) {
  scm_foreign_object_signed_ref(loop, 0);
}

static SCM w_event_loop_type;

void init_w_event_loop_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("wl-event-loop");
  slots = scm_list_1(scm_from_utf8_symbol("pointer"));
  finalizer = NULL;
  w_event_loop_type = scm_make_foreign_object_type(name, slots, finalizer);
}

SCM make_event_loop(SCM s_pointer) {
  /* struct wl_event_loop *w_event_loop; */
  struct wl_event_loop *p;
  p = (struct wl_event_loop *)scm_to_pointer(s_pointer);

  /* w_event_loop = (struct wl_event_loop *) scm_gc_malloc (sizeof (struct
   * wl_event_loop), "wl-event-loop"); */
  return scm_make_foreign_object_1(w_event_loop_type, p);
}

SCM_DEFINE(scm_wl_event_loop_destroy, "wl-event-loop-destroy", 1, 0, 0,
           (SCM loop), "destroy @var{loop}.")
#define FUNC_NAME s_scm_wl_event_loop_destroy
{
  scm_assert_foreign_object_type(w_event_loop_type, loop);

  wl_event_loop_destroy((struct wl_event_loop *)scm_to_pointer(loop));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM wl_event_loop_create_wrapper(void) {
  return make_event_loop(scm_from_pointer(wl_event_loop_create(), NULL));
}

SCM wl_event_loop_add_fd_wrapper(SCM loop, SCM w_fd, SCM w_mask,
                                 wl_event_loop_fd_func_t func, void *data) {
  int fd = scm_to_int(w_fd);
  int mask = scm_to_uint32(w_mask);
  return scm_from_pointer(
      wl_event_loop_add_fd((struct wl_event_loop *)scm_to_pointer(loop), fd,
                           mask, func, data),
      NULL);
}

SCM wl_display_create_wrapper(void) {
  return scm_from_pointer(wl_display_create(), NULL);
}
SCM wl_display_destroy_wrapper(SCM display) {
  wl_display_destroy(scm_to_pointer(display));
  return SCM_UNSPECIFIED;
}

void init_w(void) {
#ifndef SCM_MAGIC_SNARFER
#include "w.x"
#endif
  scm_c_define_gsubr("wl-event-loop-create", 0, 0, 0,
                     wl_event_loop_create_wrapper);
  scm_c_define_gsubr("wl-event-loop-add-fd-wrapper", 5, 0, 0,
                     wl_event_loop_add_fd_wrapper);
  scm_c_define_gsubr("wl-display-destroy", 1, 0, 0, wl_display_destroy_wrapper);
}
