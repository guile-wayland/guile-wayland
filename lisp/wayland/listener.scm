(define-module (wayland listener)
  #:use-module (wayland list)
  #:use-module (oop goops)
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


;; (define-class <wl-listener> ()
;;   (pointer))

(define-class <wl-listener> ()
  (data #:accessor .data
        #:init-keyword #:data)
  (link #:allocation #:virtual
        #:accessor .link
        #:slot-ref (lambda (a)
                     (wrap-wl-list
                      (bytestructure-ref
                       (pointer->bytestructure (.data a)
                                               %wl-listener)
                       'link)))
        #:slot-set! (lambda (instance new-val)
                      (bytestructure-set!
                       (pointer->bytestructure (.data instance)
                                               %wl-listener)
                       'link new-val)))
  (notify #:allocation #:virtual
          #:accessor .notify
          #:slot-ref (lambda (a) (bytestructure-ref
                                  (pointer->bytestructure (.data a)
                                                          %wl-listener) 'notify))
          #:slot-set! (lambda (instance new-val)
                        (bytestructure-set!
                         (pointer->bytestructure (.data instance)
                                                 %wl-listener)
                         'notify new-val))))

(define (make-wl-listener
         notify)
  (%make-wl-listener (lambda (l data)
                       (notify (wrap-wl-listener l) data))))

(define (wrap-wl-listener p)
  (make <wl-listener> #:data p ;; (cond ((pointer? p ) (pointer->bytestructure p %wl-listener))
        ;;       ((bytestructure? p) p))
        ))

(define (unwrap-wl-listener listener)
  (.data listener))
