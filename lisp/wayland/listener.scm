(define-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
  #:use-module (wayland base)
  #:use-module (wayland util)
  #:use-module ((system foreign) #:select (%null-pointer
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
            .link
            .notify))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wl_listener"))

(define wl-notify-func
  (bs:pointer
   (delay (bs:struct `((listener ,%wl-listener-struct)
                       (data ,(bs:pointer 'void)))))))
(define %wl-listener-struct
  (bs:struct
   `((link ,%wl-list-struct)
     (notify ,wl-notify-func))))

(define-bytestructure-class <wl-listener> ()
  %wl-listener-struct
  wrap-wl-listener unwrap-wl-listener wl-listener?
  (link #:accessor .link)
  (notify #:accessor .notify))

(define (make-wl-listener
         notify)
  (%make-wl-listener (lambda (l data)
                       (notify (wrap-wl-listener l) data))))
