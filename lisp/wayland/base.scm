(define-module (wayland base)
  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:use-module (system foreign)
  #:export (define-wl-type))
(define-syntax define-wl-type
  (lambda (x)
    (syntax-case x (---)
      ((_ class-name ref-name make-name --- check-name wrap-name unwrap-name)
       #'(begin
           (define-foreign-object-type class-name
             make-name
             (ref-name))
           (define (check-name obj) (is-a? obj class-name))
           (define wrap-name
             (let ((address->instance (make-weak-value-hash-table 3000)))
               (lambda (ptr)
                 (let ((address (pointer-address ptr)))
                   (or (hash-ref address->instance address)
                       (let ((o (make-name address)))
                         (hash-set! address->instance address o)
                         o))))))

           (define (unwrap-name i)
             (make-pointer (ref-name i))))))))
