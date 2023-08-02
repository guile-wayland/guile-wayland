(define-module (wayland shm)
  #:use-module ((system foreign)
                #:select
                (null-pointer?
                 define-wrapped-pointer-type
                 pointer->procedure
                 %null-pointer
                 void
                 pointer-address
                 (int32 . ffi:int32)
                 (uint32 . ffi:uint32)
                 (int . ffi:int)))
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module (wayland proxy)
  #:use-module (wayland server client)
  #:use-module (wayland server resource)
  #:use-module (ice-9 format)
  #:use-module (bytestructures guile))

(define %wl-shm-buffer-struct (bs:unknow))

(define-bytestructure-class <wl-shm-buffer> ()
  %wl-shm-buffer-struct
  wrap-wl-shm-buffer unwrap-wl-shm-buffer wl-shm-buffer?)

(define %wl-shm-buffer-get
  (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))

(define (wl-shm-buffer-get resource)
  (wrap-wl-shm-buffer (%wl-shm-buffer-get (unwrap-wl-resource resource))))

(define %wl-shm-buffer-begin-access
  (wayland-server->procedure void "wl_shm_buffer_begin_access" '(*)))

(define (wl-shm-buffer-begin-access buffer)
  (%wl-shm-buffer-begin-access (unwrap-wl-shm-buffer buffer)))

(define %wl-shm-buffer-end-access
  (wayland-server->procedure '* "wl_shm_buffer_end_access" '(*)))

(define (wl-shm-buffer-end-access resource)
  (%wl-shm-buffer-end-access (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-data
  (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))

(define (wl-shm-buffer-get-data resource)
  (%wl-shm-buffer-get (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-stride
  (wayland-server->procedure '* "wl_shm_buffer_get" '(*)))

(define (wl-shm-buffer-get-stride resource)
  (%wl-shm-buffer-get (unwrap-wl-resource resource)))

(define %wl-shm-buffer-get-format
  (wayland-server->procedure '* "wl_shm_buffer_get_format" '(*)))
