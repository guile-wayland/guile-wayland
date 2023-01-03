(define-module (wayland pointer)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:use-module (wayland proxy)
  #:use-module (wayland util)
  #:export (%wl-pointer-struct))

(define %wl-pointer-struct (bs:unknow))
(define-bytestructure-class <wl-pointer> ()
  %wl-pointer-struct
  wrap-wl-pointer unwrap-wl-pointer wl-pointer?)
(define %wl-pointer-interface-struct
  (bs:struct
   `((set-cursor ,(bs:pointer '*))
     (release ,(bs:pointer '*)))))
(define-public %wl-pointer-interface
  (wayland-server->pointer "wl_pointer_interface"))

(define-method (wl-pointer-get-user-data (pointer <wl-pointer>))
  (wl-proxy-get-user-data pointer))
