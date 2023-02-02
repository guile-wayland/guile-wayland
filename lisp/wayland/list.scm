(define-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module (ice-9 format)
  #:use-module (bytestructure-class)
  #:use-module ((system foreign) #:select (make-pointer
                                           pointer-address
                                           %null-pointer void pointer?
                                           (int . ffi:int)))
  #:export (;wl-list-init
            %wl-list-struct
            wrap-wl-list
            unwrap-wl-list
            wl-list-init
            wl-list-insert
            wl-list-remove
            wl-list-length
            wl-list-empty
            make-wl-list
            wl-list-for-each
            wl-list-for-each-reverse
            wl-list->list
            .prev
            .next))

;; (define-class <wl-list> ()
;;   (pointer #:ass))
(define %wl-list-struct
  (bs:struct
   `((prev ,(bs:pointer (delay %wl-list-struct)))
     (next ,(bs:pointer (delay %wl-list-struct))))))

(define-bytestructure-class <wl-list> ()
  %wl-list-struct
  wrap-wl-list unwrap-wl-list wl-list?
  (prev #:accessor .prev)
  (next #:accessor .next))

(define (make-wl-list)
  (let ((o(wrap-wl-list (bytestructure->pointer (bytestructure %wl-list-struct)))))
    (wl-list-init o)
    o))

(define-public wl-list-next .next)

(define-public wl-list-prev .prev)

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

(define* (make-wl-list-for-each #:optional (to-next .next))
  (lambda (proc wl-list bs member)
    (let ((bs (if (bytestructure-descriptor? bs)
                  bs
                  (.descriptor bs))))
      (let loop ((wl-list* (to-next wl-list))
                 (obj (wl-container-of (to-next wl-list) bs member)))
        (unless (eq? wl-list wl-list*)
          (begin
            (proc obj wl-list*)
            (loop (to-next wl-list*)
                  (wl-container-of (to-next wl-list*) bs member))))))))
(define-method (wl-list-for-each (proc <procedure>) (wl-list <wl-list>) bs (member <symbol>))
  ((make-wl-list-for-each) proc wl-list bs member))
(define-method (wl-list-for-each-reverse (proc <procedure>) (wl-list <wl-list>) bs (member <symbol>))
  ((make-wl-list-for-each .prev) proc wl-list bs member))
(define (wl-list->list wlist bs member)
  (let ((n '()))
    (wl-list-for-each
     (lambda (obj wl) (set! n (cons obj n)))
     wlist bs member)
    n))
