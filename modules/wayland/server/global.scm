(define-module (wayland server global)
  #:use-module (wayland interface)
  #:use-module (wayland server client)
  #:use-module (wayland server display)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:export (%wl-global-struct
            wl-global?
            wrap-wl-global
            unwrap-wl-global))

(define %wl-global-struct (bs:unknow))

(define-bytestructure-class <wl-global> ()
  %wl-global-struct
  wrap-wl-global unwrap-wl-global wl-global?)

(define-wl-server-procedure (wl-global-create display
                                              interface version data bind)
  ('* "wl_global_create" (list '* '* ffi:int '* '*))
  (wrap-wl-global (% (unwrap-wl-display display)
                     (unwrap-wl-interface interface)
                     version
                     data
                     (ffi:procedure->pointer
                      ffi:void
                      (lambda (client data version id)
                        (bind (wrap-wl-client client) data version id))
                      (list '* ffi:void ffi:uint32 ffi:uint32)))))

(define-wl-server-procedure (wl-global-remove global)
  (ffi:void "wl_global_remove" (list '*))
  (% (unwrap-wl-global global)))

(define-wl-server-procedure (wl-global-destroy global)
  (ffi:void "wl_global_destroy" (list '*))
  (% (unwrap-wl-global global)))

(define-wl-server-procedure (wl-global-get-interface global)
  ('* "wl_global_get_interface" '(*))
  (wrap-wl-interface (% (unwrap-wl-global global))))

;; wl_global_get_name

;; wl_global_get_version

;; wl_global_get_display

;; wl_global_get_user_data

;; wl_global_set_user_data
