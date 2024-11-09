(define-module (wayland server display)
  #:use-module (wayland server protocol wayland)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland server event-loop)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland server listener)
  #:use-module ((system foreign) #:select (null-pointer?
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
  #:export (wl-display-add-socket*)
  #:re-export (wrap-wl-display
               unwrap-wl-display
               wl-display?))


(define-wl-server-procedure (wl-display-create)
  ('* "wl_display_create" '())
  (let ((out (%)))
    (if (null-pointer? out)
        #f
        (wrap-wl-display out))))

(define-wl-server-procedure (wl-display-destroy w-display)
  (void "wl_display_destroy" '(*))
  (% (unwrap-wl-display w-display)))

(define-wl-server-procedure (wl-display-get-event-loop w-display)
  ('* "wl_display_get_event_loop" '(*))
  (wrap-wl-event-loop
   (% (unwrap-wl-display w-display))))

(define* (wl-display-add-socket* display #:optional fd-or-name)
  (if fd-or-name
      (cond ((string? fd-or-name)
             (wl-display-add-socket display fd-or-name))
            (else
             (wl-display-add-socket-fd display fd-or-name)))

      (wl-display-add-socket-auto display)))

(define-wl-server-procedure (wl-display-add-socket a b)
  (ffi:int "wl_display_add_socket" '(* *))
  (% (unwrap-wl-display a)
     (string->pointer b)))

(define-wl-server-procedure (wl-display-add-socket-auto display)
  ('* "wl_display_add_socket_auto" '(*))
  (let ((out (% (unwrap-wl-display display))))
    (if (null-pointer? out)
        #f
        (pointer->string out))))

(define-wl-server-procedure (wl-display-add-socket-fd a b)
  (ffi:int "wl_display_add_socket_fd" (list '* ffi:int))
  (% (unwrap-wl-display a) b))

(define-wl-server-procedure (wl-display-terminate a)
  (void "wl_display_terminate" '(*))
  (% (unwrap-wl-display a)))

(define-wl-server-procedure (wl-display-run w-display)
  (void "wl_display_run" '(*))
  (% (unwrap-wl-display w-display)))

(define-wl-server-procedure (wl-display-flush-clients d)
  (void "wl_display_flush_clients" '(*))
  (% (unwrap-wl-display d)))

(define-wl-server-procedure (wl-display-destroy-clients w-display)
  (void "wl_display_destroy_clients" '(*))
  (% (unwrap-wl-display w-display)))


;; wl_display_get_serial

;; wl_display_next_serial

(define-wl-server-procedure (wl-display-add-destroy-listener w-display w-listener)
  (void "wl_display_add_destroy_listener" '(* *))
  (% (unwrap-wl-display w-display)
     (unwrap-wl-listener w-listener)))

(define-wl-server-procedure (wl-display-add-client-created-listener w-display w-listener)
  (void "wl_display_add_client_created_listener"
        '(* *))
  (% (unwrap-wl-display w-display)
     (unwrap-wl-listener w-listener)))

;; wl_display_get_destroy_listener

;; wl_display_set_global_filter
(define-wl-server-procedure (wl-display-get-client-list w-display)
  ('* "wl_display_get_client_list" '(*))
  (wrap-wl-list
   (%
    (unwrap-wl-display w-display))))

(define-wl-server-procedure (wl-display-init-shm w-display)
  (ffi:int "wl_display_init_shm" '(*))
  (% (unwrap-wl-display w-display)))

;; wl_display_add_shm_format

;; wl_display_add_protocol_logger
