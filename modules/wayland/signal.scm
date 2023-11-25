(define-module (wayland signal)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module (wayland list)
  #:use-module (wayland server listener)
  #:use-module (rnrs bytevectors)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign)
                #:select
                (pointer?
                 pointer->bytevector
                 bytevector->pointer
                 make-pointer
                 void
                 procedure->pointer))
  #:use-module (oop goops)
  #:duplicates (merge-generics)
  #:export (%wl-signal-struct
            wl-signal-add
            wrap-wl-signal
            unwrap-wl-signal
            wl-signal-init
            .listener-list
            <wl-signal>))

(eval-when (expand load eval)
  (define %wl-signal-struct
    (bs:struct `((listener-list ,%wl-list-struct)))))

(define-bytestructure-class <wl-signal> ()
  %wl-signal-struct wrap-wl-signal unwrap-wl-signal wl-signal?
  (data-wrapper #:allocation #:instance #:init-value #f)
  (listener-list #:accessor .listener-list))

(define (make-wl-signal)
  (wrap-wl-signal
   (make-bytevector
    (bytestructure-descriptor-size %wl-signal-struct))))

(define (wl-signal-add signal listener)
  (slot-set! listener 'signal signal)
  (wl-list-insert (.prev (.listener-list signal)) (.link listener)))

(define* (wl-signal-init #:optional (signal (make-wl-signal)))
  (wl-list-init (.listener-list signal))
  signal)

;; (define wl-signal-emit)
