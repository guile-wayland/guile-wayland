(define-module (wayland resource)
  #:use-module (wayland base)
  #:use-module (ice-9 format)
  #:use-module ((system foreign)
                #:prefix ffi:)
  #:use-module (wayland util)
  #:use-module (wayland client)
  #:use-module (wayland list)
  #:use-module (wayland interface)
  #:export (%wl-resource-struct
            wl-resource?
            wrap-wl-resource
            unwrap-wl-resource

            wl-resource-create
            wl-resource-get-version
            wl-resource-get-class))

(define %wl-resource-struct (bs:unknow))

(define-bytestructure-class <wl-resource> ()
  %wl-resource-struct
  wrap-wl-resource unwrap-wl-resource wl-resource?)

(define-wl-server-procedure (wl-resource-create client interface version id)
  ('* "wl_resource_create" (list '* '* ffi:int ffi:uint32))
  (wrap-wl-resource (% (unwrap-wl-client client)
                       (unwrap-wl-interface interface)
                       version
                       id)))

(define-wl-server-procedure (wl-resource-destroy resource)
  (ffi:void "wl_resource_destroy" '(*))
  (% (unwrap-wl-resource resource)))

(define-wl-server-procedure (wl-resource-instance-of resource
                                                     interface
                                                     implementation)
  (ffi:int "wl_resource_instance_of" '(* * *))
  (% (unwrap-wl-resource resource)
     (unwrap-wl-interface interface)
     implementation))

(define-wl-server-procedure (wl-resource-get-id resource)
  (ffi:uint32 "wl_resource_get_id" '(*))
  (% (unwrap-wl-resource resource)))

(define-wl-server-procedure (wl-resource-get-link resource)
  ('* "wl_resource_get_link" '(*))
  (wrap-wl-list (% (unwrap-wl-resource resource))))

(define-wl-server-procedure (wl-resource-from-link wlist)
  ('* "wl_resource_from_link" '(*))
  (wrap-wl-resource (% (unwrap-wl-list wlist))))

(define-wl-server-procedure (wl-resource-find-for-client wlist client)
  ('* "wl_resource_find_for_client" '(* *))
  (wrap-wl-resource (% (unwrap-wl-list wlist) (unwrap-wl-client client))))

(define-wl-server-procedure (wl-resource-get-client resource)
  ('* "wl_resource_get_client" '(*))
  (wrap-wl-client (% (unwrap-wl-resource resource))))

(define-wl-server-procedure (wl-resource-get-version resource)
  (ffi:int "wl_resource_get_version" '(*))
  (% (unwrap-wl-resource resource)))

(define-wl-server-procedure (wl-resource-get-class resource)
  ('* "wl_resource_get_class" '(*))
  (ffi:pointer->string (% (unwrap-wl-resource resource))))
