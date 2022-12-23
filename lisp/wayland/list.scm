(define-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer %null-pointer void pointer?(int . ffi:int)))
  #:export (;wl-list-init
            %wl-list-struct
            wrap-wl-list
            unwrap-wl-list
            wl-list-init
            wl-list-insert
            wl-list-remove
            wl-list-length
            wl-list-empty
            make-wl-list))

;; (define-class <wl-list> ()
;;   (pointer #:ass))
(define %wl-list-struct
  (bs:struct
   `((prev ,(bs:pointer (delay %wl-list-struct)))
     (next ,(bs:pointer (delay %wl-list-struct))))))

(define-wl-type <wl-list>
  %wl-list %make-wl-list
  ---
  wl-list?
  wrap-wl-list unwrap-wl-list)


(define (make-wl-list )
  (wrap-wl-list (bytestructure->pointer (bytestructure %wl-list-struct))))

(define-public (wl-list-next wl-l)
  (wrap-wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (unwrap-wl-list wl-l) %wl-list-struct) 'next))))

(define-public (wl-list-prev wl-l)
  (wrap-wl-list
   (make-pointer
    (bytestructure-ref
     (pointer->bytestructure (unwrap-wl-list wl-l) %wl-list-struct) 'prev))))

(define %wl-list-init
  (wayland-server->procedure
   void "wl_list_init"
   (list '*)))

(define (wl-list-init wl-l)
  (let ((p (unwrap-wl-list wl-l)))
    (%wl-list-init p)
    wl-l))

(define wl-list-insert
  (let ((proc (wayland-server->procedure
               void "wl_list_insert"
               (list '* '*))))
    (lambda (lst lst2)
      (proc (unwrap-wl-list lst)
            (unwrap-wl-list lst2)))))

(define-wl-server-procedure (wl-list-remove l)
  (void "wl_list_remove" (list '*))
  (% (unwrap-wl-list l)))

(define %wl-list-length
  (wayland-server->procedure ffi:int "wl_list_length" '(*)))

(define (wl-list-length w-list)
  (%wl-list-length (unwrap-wl-list w-list)))

(define %wl-list-empty (wayland-server->procedure ffi:int "wl_list_empty" '(*)))

(define (wl-list-empty l) (case (%wl-list-empty (unwrap-wl-list l))
                            ((0) #f)
                            ((1) #t)
                            (else (error ))) )
