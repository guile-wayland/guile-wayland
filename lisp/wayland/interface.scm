(define-module (wayland interface)
  #:use-module (srfi srfi-9)
  #:use-module (wayland util)
  #:use-module (bytestructures guile)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:use-module ((system foreign) #:select (make-pointer))
  #:export (%wl-message-struct
            %wl-interface-struct
            wrap-wl-message unwrap-wl-message wl-message?
            .name
            .signature
            .version
            .method-count
            .methods
            .event-count
            .events
            wrap-wl-interface unwrap-wl-interface wl-interface?))

(define %wl-message-struct
  (bs:struct
   `((name ,cstring-pointer)
     (signature ,cstring-pointer)
     (types ,(bs:pointer '*)))))

(define-bytestructure-class <wl-message> ()
  %wl-message-struct
  wrap-wl-message unwrap-wl-message wl-message?
  (name #:accessor .name)
  (signature #:accessor .signature)
  (types #:accessor .types))

(define %wl-interface-struct
  (bs:struct
   `((name ,cstring-pointer)
     (version ,int)
     (method-count ,int)
     (methods ,(bs:pointer %wl-message-struct))
     (event-count ,int)
     (events ,(bs:pointer %wl-message-struct)))))

(define-bytestructure-class <wl-interface> ()
  %wl-interface-struct
  wrap-wl-interface unwrap-wl-interface wl-interface?
  (name #:accessor .name)
  (version #:accessor .version)
  (method-count #:accessor .method-count)
  (methods #:accessor .methods )
  (event-count #:accessor .event-count)
  (events #:accessor .events))
