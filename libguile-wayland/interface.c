#include <libguile.h>
#include <wayland-util.h>

struct my_wl_message
{
  const char *name;
  const char *signature;
  struct wl_interface **types;
};

struct my_wl_interface
{
  char *name;
  int version;
  int method_count;
  struct wl_message *methods;
  int event_count;
  struct wl_message *events;
};

struct wl_interface;
SCM_DEFINE (scm_make_interface, "%make-wl-interface", 4, 0, 0,
            (SCM name, SCM version, SCM smethods, SCM sevents), "")
{
  struct wl_interface *interface = scm_calloc (sizeof (struct wl_interface));
  interface->name = scm_to_utf8_string (name);
  interface->version = scm_to_int (version);
  interface->method_count = scm_to_int (scm_length (smethods));
  interface->event_count = scm_to_int (scm_length (sevents));
  struct wl_message *methods
      = scm_calloc (interface->method_count * (sizeof (struct wl_message)));
  for (unsigned int i = 0; i < interface->method_count; i++)
    {
      SCM sm = scm_list_ref (smethods, scm_from_unsigned_integer (i));
      char *name = scm_to_utf8_string (
          scm_list_ref (sm, scm_from_unsigned_integer (0)));
      char *signature = scm_to_utf8_string (
          scm_list_ref (sm, scm_from_unsigned_integer (1)));
      methods[i].name = name;
      methods[i].signature = signature;
    }
  interface->methods = methods;

  struct wl_message *events
      = scm_calloc (interface->event_count * (sizeof (struct wl_message)));
  for (unsigned int i = 0; i < interface->event_count; i++)
    {
      SCM sm = scm_list_ref (sevents, scm_from_unsigned_integer (i));
      char *name = scm_to_utf8_string (
          scm_list_ref (sm, scm_from_unsigned_integer (0)));
      char *signature = scm_to_utf8_string (
          scm_list_ref (sm, scm_from_unsigned_integer (1)));
      events[i].name = name;
      events[i].signature = signature;
    }
  interface->events = events;
  return scm_call_1 (
      scm_c_public_ref ("wayland interface", "wrap-wl-interface"),
      scm_from_pointer (interface, NULL));
}

SCM_DEFINE (scm_interface_update_message_types,
            "%wl-interface-update-message-types", 3, 0, 0,
            (SCM sinterface, SCM smethods, SCM sevents), "")
{
  struct my_wl_interface *interface = scm_to_pointer (scm_call_1 (
      scm_c_public_ref ("wayland interface", "unwrap-wl-interface"),
      sinterface));
  struct my_wl_message *methods = &interface->methods;
  for (unsigned int i = 0; i < interface->method_count; i++)
    {
      SCM sm = scm_list_ref (smethods, scm_from_unsigned_integer (i));
      int arg_length = scm_to_int (scm_length (sm));
      struct my_wl_interface *method_types
          = scm_calloc (arg_length * (sizeof (struct wl_interface)));
      for (unsigned int j = 0; j < arg_length; j++)
        {
          SCM in = scm_list_ref (sm, scm_from_int (j));
          if (scm_is_true (in))
            {
              struct my_wl_interface *w = ((scm_to_pointer (
                  scm_call_1 (scm_c_public_ref ("wayland interface",
                                                "unwrap-wl-interface"),
                              in))));
              method_types[j] = *w;
            }
        }
      interface->methods[i].types = method_types;
    }

  for (unsigned int i = 0; i < interface->event_count; i++)
    {
      SCM sm = scm_list_ref (sevents, scm_from_unsigned_integer (i));
      int arg_length = scm_to_int (scm_length (sm));
      struct my_wl_interface *event_types
          = scm_calloc (arg_length * (sizeof (struct wl_interface)));
      for (unsigned int j = 0; j < arg_length; j++)
        {
          SCM in = scm_list_ref (sm, scm_from_int (j));
          if (scm_is_true (in))
            {
              struct my_wl_interface *w = ((scm_to_pointer (
                  scm_call_1 (scm_c_public_ref ("wayland interface",
                                                "unwrap-wl-interface"),
                              in))));
              event_types[j] = *w;
            }
        }
      interface->events[i].types = event_types;
    }

  return SCM_UNSPECIFIED;
}

void
scm_init_wl_interface (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "interface.x"
#endif
}
