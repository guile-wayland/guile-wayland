(define-module (wayland resource)
  #:use-module (wayland base)
  #:use-module (ice-9 format)
  #:use-module ((system foreign)
                #:select
                (define-wrapped-pointer-type
                  pointer->string
                  pointer-address
                  (int . ffi:int)
                  (uint32 . ffi:uint32)))
  #:use-module (wayland util)
  #:use-module (wayland client)
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

(define %wl-resource-create
  (wayland-server->procedure '* "wl_resource_create" (list '* '* ffi:int ffi:uint32)))

(define (wl-resource-create
         client interface version id)
  (wrap-wl-resource (%wl-resource-create (unwrap-wl-client client)
                                         (unwrap-wl-interface interface)
                                         version
                                         id)))

(define %wl-resource-instance-of
  (wayland-server->procedure ffi:int "wl_resource_instance_of" '(* * *)))

(define (wl-resource-instance-of resource interface implementation)
  (%wl-resource-instance-of resource interface implementation))

(define %wl-resource-get-version
  (wayland-server->procedure ffi:int "wl_resource_get_version" '(*)))

(define (wl-resource-get-version resource)
  (%wl-resource-get-version (unwrap-wl-resource resource)))

(define %wl-resource-get-class
  (wayland-server->procedure '* "wl_resource_get_class" '(*)))

(define (wl-resource-get-class resource)
  (pointer->string(%wl-resource-get-class (unwrap-wl-resource resource))))

                                        ;(wayland-server->procedure  "wl_resource_post_event")
