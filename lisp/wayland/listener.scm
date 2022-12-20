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
  #:export (%wl-listener
            wrap-wl-listener
            unwrap-wl-listener
            make-wl-listener
            .link
            .notify))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wl_listener"))

(define wl-notify-func
  (bs:pointer
   (delay (bs:struct `((listener ,%wl-listener)
                       (data ,(bs:pointer 'void)))))))
(define %wl-listener
  (bs:struct
   `((link ,%wl-list)
     (notify ,wl-notify-func))))

(define-wl-type <wl-listener>
  %%wl-listener %%make-wl-listener
  ---
  wl-listener? wrap-wl-listener unwrap-wl-listener)

(define-method (.link (a <wl-listener>))
  (wrap-wl-list
   (bytestructure->pointer
    (bytestructure-ref
     (pointer->bytestructure (unwrap-wl-listener a)
                             %wl-listener)
     'link))))
(define-method (.notify (a <wl-listener>))
  (bytestructure-ref
   (pointer->bytestructure (unwrap-wl-listener a)
                           %wl-listener)
   'notify))

(define (make-wl-listener
         notify)
  (%make-wl-listener (lambda (l data)
                       (notify (wrap-wl-listener l) data))))
