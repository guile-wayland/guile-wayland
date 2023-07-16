(define-module (wayland output)
  #:use-module (wayland interface)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:use-module (wayland util)
  #:export (%wl-output-interface
            %wl-output-struct))

(define %wl-output-struct (bs:unknow))

(define-bytestructure-class <wl-output> ()
  %wl-output-struct
  wrap-wl-output unwrap-wl-output wl-output?)

(define %wl-output-interface-struct
  (bs:struct `((release ,(bs:pointer 'void)))))

(define (make-wl-output-interface-implementation release)
  (bytestructure %wl-output-interface-struct
                 `((release ,(procedure->pointer
                              void (lambda (client resource)
                                     (release (wrap-wl-client client)
                                              (wrap-wl-resource resource))) '(* *))))))

(define %wl-output-interface
  (pointer->bytestructure
   (wayland-server->pointer "wl_output_interface")
   %wl-interface-struct))

(define-method (wl-output-get-version (output <wl-output>))
  (wl-proxy-get-version output))
