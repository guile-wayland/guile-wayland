(define-module (wayland compositor)
  #:use-module (wayland util)
  #:use-module (wayland interface)
  #:export (%wl-compositor-interface))

(define-public %wl-compositor-interface
  (wrap-wl-interface
   (wayland-server->pointer "wl_compositor_interface")))

                                        ;(define)

;; (define-foreign-object-type
;;   <wl-compositor> make-wl-compositor ())
(define WL_COMPOSITOR_CREATE_SURFACE 0)
;; (define (wl-compositor-create-surface compositior)
;;   (wl-proxy-marshal-constructor
;;    compositior
;;    WL_COMPOSITOR_CREATE_SURFACE
;;    (wl-interface->pointer wl-surface-in)))
