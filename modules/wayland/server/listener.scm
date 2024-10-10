(define-module (wayland server listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module ((system foreign)
                #:select
                (%null-pointer
                 procedure->pointer
                 void
                 pointer?
                 pointer->bytevector))
  #:use-module (bytestructures guile)
  #:duplicates (merge-generics)
  #:export (%wl-listener-struct
            wrap-wl-listener
            unwrap-wl-listener
            make-wl-listener
            wl-listener-remove
            .link
            .notify))

(define wl-notify-func
  (bs:pointer '*))

(define %wl-listener-struct
  (bs:struct
   `((link ,%wl-list-struct)
     (notify ,wl-notify-func))))

(define-bytestructure-class <wl-listener> ()
  %wl-listener-struct
  wrap-wl-listener unwrap-wl-listener wl-listener?
  (signal #:allocation #:instance #:init-value #f)
  (scm-proc #:allocation #:instance)
  (link #:accessor .link)
  (notify #:init-keyword #:notify #:accessor .notify))

(define %listeners (make-weak-key-hash-table))

(define (make-wl-listener proc)
  (letrec ((l (make <wl-listener>))
           (pre
            (lambda (_ raw-data)
              (let* ((signal (slot-ref l 'signal))
                     (data-wrapper
                      (and signal (slot-ref signal 'data-wrapper))))
                (proc l (if data-wrapper
                            (data-wrapper raw-data)
                            raw-data)))))
           (pre-p
            (procedure->pointer
             void
             pre
             '(* *))))
    (slot-set! l 'scm-proc proc)
    (slot-set! l 'notify pre-p)
    (hashq-set! %listeners l (list proc pre-p))
    l))

(define (wl-listener-remove listener)
  (wl-list-remove (.link listener))
  (hashq-remove! %listeners listener))

;; (eval-when (expand load eval)
;;   (load-extension "libguile-wayland" "scm_init_wl_listener"))
