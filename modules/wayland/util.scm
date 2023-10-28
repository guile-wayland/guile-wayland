(define-module (wayland util)
  #:use-module (wayland config)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign)
                #:select (null-pointer?
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
                          void))
  #:re-export (bytestructure->pointer
               pointer->bytestructure)
  #:export (wayland-server->pointer
            wayland-client->pointer
            wayland-client->procedure
            wayland-server->procedure

            make-pointer->string
            string->pointer-address
            %wl-array-struct
            wl-container-of

            wrap-wl-array
            unwrap-wl-array
            wl-array?

            wl-log-set-handler-server
            WL_MARSHAL_FLAG_DESTROY

            xml)
  #:export-syntax (define-wl-server-procedure
                    define-wl-client-procedure))

(define WL_MARSHAL_FLAG_DESTROY 1)

(define (wayland-server->pointer name)
  (dynamic-func name (dynamic-link %libwayland-server)))

(define (wayland-client->pointer name)
  (dynamic-func name (dynamic-link %libwayland-client)))

(define (wayland-server->procedure return name params)
  (let ((ptr (wayland-server->pointer name)))
    (pointer->procedure return ptr params)))

(define (wayland-client->procedure return name params)
  (let ((ptr (wayland-client->pointer name)))
    (pointer->procedure return ptr params)))

(define-syntax define-wl-server-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (name args ...) (return-type cname arg-types) body ...)
       (with-syntax ((% (datum->syntax x '%)))
         #'(begin
             (define-public name
               (let ((% (wayland-server->procedure return-type cname arg-types)))
                 (lambda* (args ...)
                   body ...))))))
      ((o-name (name args ...) (return-type cname arg-types))
       #'(o-name (name args ...) (return-type cname arg-types) (% args ...))))))

(define-syntax define-wl-client-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (name args ...) (return-type cname arg-types) body ...)
       (with-syntax ((% (datum->syntax x '%)))
         #'(begin
             (define-public name
               (let ((% (wayland-client->procedure return-type cname arg-types)))
                 (lambda* (args ...)
                   body ...))))))
      ((o-name (name args ...) (return-type cname arg-types))
       #'(o-name (name args ...) (return-type cname arg-types) (% args ...))))))

(define make-pointer->string
  (compose (lambda (a) (if (null-pointer? a)
                      ""
                      (pointer->string a))) make-pointer))

(define string->pointer-address
  (compose pointer-address string->pointer))

(define %wl-array-struct
  (bs:struct `((size ,size_t)
               (alloc ,size_t)
               (data ,(bs:pointer 'void)))))

(define-bytestructure-class <wl-array> ()
  %wl-array-struct
  wrap-wl-array unwrap-wl-array wl-array?
  (size #:getter .size)
  (alloc #:getter .alloc)
  (data #:getter .data))

(define (wl-log-set-handler-server proc)
  (wayland-server->procedure void "wl_log_set_handler_server" '(*))
  (procedure->pointer 'void (lambda (a b) (proc (pointer->string a) b)) (list '* '*)))

(define (wl-container-of ptr sample member)
  (let ((bs (if (bytestructure-descriptor? sample)
                sample
                (.descriptor sample))))

    (bytestructure->bs-instance
     (pointer->bytestructure
      (make-pointer
       (- (pointer-address (get-pointer ptr))
          (bytestructure-offset
           (bytestructure-ref
            (bytestructure bs)
            member))))
      bs))))

(define (xml name)
  (string-append %wayland-protocols-dir "/" name ".xml"))
