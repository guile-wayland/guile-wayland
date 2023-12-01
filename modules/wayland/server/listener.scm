(define-module (wayland server listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module ((system foreign)
                #:select
                (%null-pointer
                 procedure->pointer
                 void
                 pointer?
                 pointer->bytevector))
  #:use-module (bytestructures guile)
  #:duplicates (merge-generics)
  #:export (%wl-listener-struct
            wrap-wl-listener
            unwrap-wl-listener
            make-wl-listener
            wl-listener-remove
            .link
            .notify))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wl_listener"))

(define wl-notify-func
  (bs:pointer '*))

(define %wl-listener-struct
  (bs:struct
   `((link ,%wl-list-struct)
     (notify ,wl-notify-func))))

(define-bytestructure-class <wl-listener> ()
  %wl-listener-struct
  wrap-wl-listener unwrap-wl-listener wl-listener?
  (signal #:allocation #:instance #:init-value #f)
  (scm-created? #:allocation #:instance #:init-value #f)
  (link #:accessor .link)
  (notify #:accessor .notify))
