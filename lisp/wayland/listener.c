#include "libguile/boolean.h"
#include "libguile/eval.h"
#include "libguile/foreign.h"
#include "libguile/gc.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/list.h"
#include "libguile/modules.h"
#include "libguile/scm.h"
#include "libguile/symbols.h"
#include <libguile.h>
#include <stdlib.h>
#include <wayland-server-core.h>
#include <wayland-util.h>

SCM_DEFINE (scm_make_wl_listener, "%make-wl-listener", 1, 0, 0, (SCM proc), "")
{
  struct wl_listener *new_listener = scm_calloc (sizeof (&new_listener));
  SCM p = scm_procedure_to_pointer (
      scm_c_public_ref ("system foreign", "void"), proc,
      scm_list_2 (scm_from_utf8_symbol ("*"), scm_from_utf8_symbol ("*")));
  scm_gc_protect_object (p);
  new_listener->notify = scm_to_pointer (p);
  SCM listener_p = scm_from_pointer (new_listener, NULL);
  SCM listener = scm_call_1 (
      scm_c_public_ref ("wayland listener", "wrap-wl-listener"), listener_p);
  scm_slot_set_x (listener, scm_from_utf8_symbol ("%notify-pointer"), p);

  return listener;
}
SCM_DEFINE (scm_wl_listener_remove, "wl-listener-remove", 1, 0, 0,
            (SCM listener), "")
{

  SCM notify_pointer
    = scm_slot_ref (listener, scm_from_utf8_symbol ("%notify-pointer"));
  SCM listner_p = (scm_call_1 (
                               scm_c_public_ref ("wayland listener", "unwrap-wl-listener"), listener));
  struct wl_listener *wl_listener = scm_to_pointer (listner_p);
  wl_list_remove (&wl_listener->link);

  if (scm_is_true (notify_pointer))
    {
      scm_gc_unprotect_object (notify_pointer);
      scm_slot_set_x (listener, scm_from_utf8_symbol ("%notify-pointer"), SCM_BOOL_F);
      free (wl_listener);
    }

  return SCM_UNSPECIFIED;
}

void
scm_init_wl_listener (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "listener.x"
#endif
}
