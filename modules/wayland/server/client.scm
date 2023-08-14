(define-module (wayland server client)
  #:use-module (bytestructures guile)
  #:autoload (wayland server protocol wayland) (wrap-wl-display unwrap-wl-display)
  #:use-module (wayland server listener)
  #:use-module (wayland server resource)
  #:use-module (wayland base)
  #:use-module (wayland list)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (%wl-client-struct
            wl-client-create
            wrap-wl-client
            unwrap-wl-client))

(define %wl-client-struct (bs:unknow))

(define-bytestructure-class <wl-client> ()
  %wl-client-struct
  wrap-wl-client unwrap-wl-client wl-client?)

(define-wl-server-procedure (wl-client-create w-display fd)
  ('* "wl_client_create" (list '* ffi:int))
  (wrap-wl-client (% (unwrap-wl-display w-display) fd)))

(define-wl-server-procedure (wl-client-get-link client)
  ('* "wl_client_get_link" '(*))
  (wrap-wl-list (% (unwrap-wl-client client))))

(define-wl-server-procedure (wl-client-from-link wlist)
  ('* "wl_client_from_link" '(*))
  (wrap-wl-client (% (unwrap-wl-list wlist))))

(define-wl-server-procedure (wl-client-destroy client)
  (ffi:void "wl_client_destroy" (list '*))
  (% (unwrap-wl-client client)))

(define-wl-server-procedure (wl-client-flush client)
  (ffi:void "wl_client_flush" '(*))
  (% (unwrap-wl-client client)))

;; wl_client_get_credentials

;; wl_client_get_fd

(define-wl-server-procedure (wl-client-add-destroy-listener client listener)
  (ffi:void "wl_client_add_destroy_listener" '(* *))
  (% (unwrap-wl-client client) (unwrap-wl-listener listener)))

;; wl_client_get_destroy_listener

(define-wl-server-procedure (wl-client-get-object client id)
  ('* "wl_client_get_object" (list '* ffi:uint32))
  (wrap-wl-resource (% (unwrap-wl-client client) id)))

(define-wl-server-procedure (wl-client-post-no-memory client)
  ('* "wl_client_post_no_memory" (list '*))
  (wrap-wl-resource (% (unwrap-wl-client client))))

(define-wl-server-procedure (wl-client-post-implementation-error client msg)
  ('* "wl_client_post_implementation_error" (list '* '*))
  (wrap-wl-resource (% (unwrap-wl-client client) (ffi:string->pointer msg))))

(define wl-client-add-resource-created-listener
  (let ((proc (wayland-server->procedure ffi:void "wl_client_add_resource_created_listener"
                                         '(* *))))
    (lambda (client listener)
      (proc (unwrap-wl-client client) (unwrap-wl-listener listener)))))

(define wl-client-for-each-resource
  (let ((proc (wayland-server->procedure ffi:void "wl_client_for_each_resource"
                                         (list '* '*))))
    (lambda (client iterator)
      (proc (unwrap-wl-client client) iterator))))

(define-wl-server-procedure (wl-client-get-display client)
  ('* "wl_client_get_display" '(*))
  (wrap-wl-display (% (unwrap-wl-client client))))
