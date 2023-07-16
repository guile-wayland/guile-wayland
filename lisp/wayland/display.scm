(define-module (wayland display )
  #:use-module (ice-9 format)
  ;; #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland config)
  #:use-module (wayland proxy)
  #:use-module (wayland registry)
  #:use-module (wayland event-loop)
  #:use-module (wayland interface)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland listener)
  #:use-module ((system foreign)
                #:select
                (null-pointer?
                 bytevector->pointer
                 make-pointer
                 procedure->pointer
                 pointer->procedure
                 pointer->bytevector
                 pointer->string
                 string->pointer
                 sizeof
                 %null-pointer
                 dereference-pointer
                 define-wrapped-pointer-type
                 pointer-address
                 void
                 (int . ffi:int)
                 (uint32 . ffi:uint32)
                 (double . ffi:double)
                 (size_t . ffi:size_t)
                 (uintptr_t . ffi:uintptr_t)))
  #:use-module (system foreign-object)
  #:use-module (system foreign-library)
  #:use-module (bytestructures guile)
  #:export (%wl-client-display-struct
            %wl-display-interface
            wl-client-display?
            wrap-wl-client-display
            unwrap-wl-client-display

            ;; client
            wl-display-connect
            wl-display-disconnect
            wl-display-get-registry
            wl-display-get-fd
            wl-display-dispatch
            wl-display-roundtrip


            wl-display-read-events
            wl-display-flush))

(eval-when (eval load compile)
  (begin
    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module)))
           (submodule-interface (resolve-interface '(wayland server display))))
      (module-use! current-module submodule-interface)
      (module-use! current-module-interface submodule-interface))))

;; (define wl-display-interface-struct
;;   (bs:struct `((sync ,(bs:pointer 'void))
;;                (get-registry ,(bs:pointer 'void)))))
(define %wl-display-interface
  (wrap-wl-interface
   (wayland-server->pointer "wl_display_interface")))

(define WL_DISPLAY_GET_REGISTRY 1)

(define %wl-client-display-struct (bs:unknow))

(define-bytestructure-class <wl-client-display> (<wl-proxy>)
  %wl-client-display-struct
  wrap-wl-client-display unwrap-wl-client-display wl-client-display?)


;; client

(define-wl-client-procedure (wl-display-connect #:optional (name #f))
  ('* "wl_display_connect" '(*))
  (let ((out (% (if name (string->pointer name) %null-pointer))))
    (if (null-pointer? out)
        #f
        (wrap-wl-client-display out))))

(define-wl-client-procedure (wl-display-connect-to-fd fd)
  ('* "wl_display_connect_to_fd" (list ffi:int))
  "if success, return wl-display else #f."
  (let ((out (% fd)))
    (if (null-pointer? out)
        #f
        (wrap-wl-client-display out))))

(define-wl-client-procedure (wl-display-disconnect w-display)
  (void "wl_display_disconnect" '(*))
  (% (unwrap-wl-client-display w-display)))

(define-wl-client-procedure (wl-display-get-fd display)
  (ffi:int "wl_display_get_fd" '(*))
  (% (unwrap-wl-client-display display)))

(define (wl-display-get-registry display)
  (wrap-wl-registry
   (wl-proxy-marshal-flags
    display
    WL_DISPLAY_GET_REGISTRY
    %wl-registry-interface
    (wl-proxy-get-version display))))

(define-wl-client-procedure (wl-display-dispatch display)
  (ffi:int "wl_display_dispatch" '(*))
  (% (unwrap-wl-client-display display)))

(define-wl-client-procedure (wl-display-roundtrip display)
  (ffi:int "wl_display_roundtrip" '(*))
  (% (unwrap-wl-client-display display)))

(define-wl-client-procedure (wl-display-read-events display)
  (ffi:int "wl_display_read_events" '(*))
  (% (unwrap-wl-client-display display)))

(define-wl-client-procedure (wl-display-flush w-display)
  (ffi:int "wl_display_flush" '(*))
  (% (unwrap-wl-client-display w-display)))
