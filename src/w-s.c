#include <libguile.h>
#include <wayland-server-core.h>

static SCM w_event_loop_type;

static void
init_w_event_loop_type (void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol ("wl-event-loop");
  slots = scm_list_1 (scm_from_utf8_symbol ("item"));
  finalizer = NULL;
  w_event_loop_type = scm_make_foreign_object_type (name, slots, finalizer);
}

static SCM
make_event_loop (void *a)
{
  return scm_make_foreign_object_1 (w_event_loop_type, a);
}

static struct wl_event_loop *
scm_to_event_loop (SCM loop)
{
  return (struct wl_event_loop *)scm_foreign_object_signed_ref (loop, 0);
}
static SCM w_display_type;

static void
init_w_display_type (void)
{
  scm_t_struct_finalize finalizer = NULL;
  w_display_type = scm_make_foreign_object_type (
      scm_from_utf8_symbol ("wl-display"),
      scm_list_1 (scm_from_utf8_symbol ("item")), finalizer);
}

static SCM
make_display (void *p)
{
  return scm_make_foreign_object_1 (w_display_type, p);
}
static struct wl_display *
scm_to_display (SCM i)
{
  return (struct wl_display *)scm_foreign_object_signed_ref (i, 0);
}
// event-loop

SCM_DEFINE (scm_wl_event_loop_destroy, "wl-event-loop-destroy", 1, 0, 0,
            (SCM loop), "destroy @var{loop}.")
#define FUNC_NAME s_scm_wl_event_loop_destroy
{
  scm_assert_foreign_object_type (w_event_loop_type, loop);
  struct wl_event_loop *s = scm_to_event_loop (loop);
  wl_event_loop_destroy (s);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_event_loop_create, "wl-event-loop-create", 0, 0, 0, (void),
            "")
#define FUNC_NAME s_scm_wl_event_loop_create
{
  return make_event_loop (wl_event_loop_create ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_event_loop_add_fd, "wl-event-loop-add-fd", 5, 0, 0,
            (SCM loop, SCM w_fd, SCM w_mask, wl_event_loop_fd_func_t func,
             void *data),
            "")
#define FUNC_NAME s_scm_wl_event_loop_add_fd
{
  int fd = scm_to_int (w_fd);
  int mask = scm_to_uint32 (w_mask);
  return scm_from_pointer (
      wl_event_loop_add_fd ((struct wl_event_loop *)scm_to_pointer (loop), fd,
                            mask, func, data),
      NULL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_event_loop_add_signal, "wl-event-loop-add-signal", 4, 0, 0,
            (SCM loop, SCM w_signal_number, wl_event_loop_signal_func_t func,
             SCM w_data),
            "")
#define FUNC_NAME s_scm_wl_event_loop_add_fd
{
  //  wl_event_loop_signal_func_t a
  //  =scm_pointer_to_procedure(wl_event_loop_signal_func_t, s_func, SCM
  //  arg_types);
  int signal_number = scm_to_int (w_signal_number);
  void *data = scm_to_pointer (w_data);

  return scm_from_pointer (
      wl_event_loop_add_signal ((struct wl_event_loop *)scm_to_pointer (loop),
                                signal_number, func, data),
      NULL);
}
#undef FUNC_NAME

// display

SCM_DEFINE (scm_wl_display_create, "wl-display-create", 0, 0, 0, (void),
            "create wl-display object")
#define FUNC_NAME s_scm_wl_display_create
{
  return make_display (wl_display_create ());
}

#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_destroy, "wl-display-destroy", 1, 0, 0,
            (SCM display), "destroy wl-display")
#define FUNC_NAME s_scm_wl_display_destroy
{
  scm_assert_foreign_object_type (w_display_type, display);
  wl_display_destroy (scm_to_display (display));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_get_event_loop, "wl-display-get-event-loop", 1, 0,
            0, (SCM display), "")
#define FUNC_NAME s_scm_wl_display_get_event_loop
{
  scm_assert_foreign_object_type (w_display_type, display);
  return make_event_loop (
      wl_display_get_event_loop (scm_to_display (display)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_add_socket_auto, "wl_display_add_socket_auto", 1, 0,
            0, (SCM display), "")
#define FUNC_NAME s_scm_wl_display_add_socket_auto
{
  scm_assert_foreign_object_type (w_display_type, display);
  return scm_from_utf8_string (
      wl_display_add_socket_auto (scm_to_display (display)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_terminate, "wl-display-terminate", 1, 0, 0,
            (SCM display), "")
#define FUNC_NAME s_scm_wl_display_terminate
{
  scm_assert_foreign_object_type (w_display_type, display);
  wl_display_terminate (scm_to_display (display));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_run, "wl-display-run", 1, 0, 0, (SCM display), "")
#define FUNC_NAME s_scm_wl_display_run
{
  scm_assert_foreign_object_type (w_display_type, display);
  wl_display_run (scm_to_display (display));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_wl_display_get_serial, "wl-display-get-serial", 1, 0, 0,
            (SCM display), "")
#define FUNC_NAME s_scm_wl_get_serial
{
  scm_assert_foreign_object_type (w_display_type, display);
  return scm_from_uint32 (wl_display_get_serial (scm_to_display (display)));
}
#undef FUNC_NAME

void
init_w (void)
{
  init_w_event_loop_type ();
  init_w_display_type ();
#ifndef SCM_MAGIC_SNARFER
#include "w.x"
#endif
}
