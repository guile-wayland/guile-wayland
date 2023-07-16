(define-module (wayland callback)
  #:use-module (bytestructure-class)
  #:use-module (wayland interface)
  #:use-module (wayland util))

(define-public wl-callback-interface
  (pointer->bytestructure
   (wayland-server->pointer "wl_callback_interface")
   %wl-interface-struct))
