#include <libguile.h>
#include <wayland-server-core.h>

static SCM w_event_loop_type;

static void init_w_event_loop_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("wl-event-loop");
  slots = scm_list_1(scm_from_utf8_symbol("item"));
  finalizer = NULL;
  w_event_loop_type = scm_make_foreign_object_type(name, slots, finalizer);
}

static SCM make_event_loop(void *a) {
  return scm_make_foreign_object_1(w_event_loop_type, a);
}

static struct wl_event_loop *event_loop_ref(SCM loop) {
  return (struct wl_event_loop *)scm_foreign_object_signed_ref(loop, 0);
}
// event-loop

SCM_DEFINE(scm_wl_event_loop_destroy, "wl-event-loop-destroy", 1, 0, 0,
           (SCM loop), "destroy @var{loop}.")
#define FUNC_NAME s_scm_wl_event_loop_destroy
{
  scm_assert_foreign_object_type(w_event_loop_type, loop);
  struct wl_event_loop *s = event_loop_ref(loop);
  wl_event_loop_destroy(s);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(scm_wl_event_loop_create, "wl-event-loop-create", 0, 0, 0, (void),
           "")
#define FUNC_NAME s_scm_wl_event_loop_create
{
  return make_event_loop(wl_event_loop_create());
}
#undef FUNC_NAME

SCM_DEFINE(scm_wl_event_loop_add_fd, "wl-event-loop-add-fd", 5, 0, 0,
           (SCM loop, SCM w_fd, SCM w_mask, wl_event_loop_fd_func_t func,
            void *data),
           "")
#define FUNC_NAME s_scm_wl_event_loop_add_fd
{
  int fd = scm_to_int(w_fd);
  int mask = scm_to_uint32(w_mask);
  return scm_from_pointer(
      wl_event_loop_add_fd((struct wl_event_loop *)scm_to_pointer(loop), fd,
                           mask, func, data),
      NULL);
}
#undef FUNC_NAME

// display

static SCM w_display_type;

static void init_w_display_type(void) {
  scm_t_struct_finalize finalizer = NULL;
  w_display_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol("wl-display"),
      scm_list_1(scm_from_utf8_symbol("item")), finalizer);
}
static SCM make_display(void *p) {
  return scm_make_foreign_object_1(w_display_type, p);
}
static struct wl_display *display_ref(SCM i) {
  return (struct wl_display *)scm_foreign_object_signed_ref(i, 0);
}

SCM_DEFINE(scm_wl_display_create, "wl-display-create", 0, 0, 0, (void),
           "create wl-display object")
#define FUNC_NAME s_scm_wl_display_create
{
  return make_display(wl_display_create());
}

#undef FUNC_NAME

SCM_DEFINE(scm_wl_display_destroy, "wl-display-destroy", 1, 0, 0, (SCM display),
           "destroy wl-display")
#define FUNC_NAME s_scm_wl_display_destroy
{
  scm_assert_foreign_object_type(w_display_type, display);
  wl_display_destroy(display_ref(display));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void init_w(void) {
  init_w_event_loop_type();
  init_w_display_type();
#ifndef SCM_MAGIC_SNARFER
#include "w.x"
#endif
}
