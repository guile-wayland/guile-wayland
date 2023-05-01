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
     (methods ,(bs:pointer '*))
     (event-count ,int)
     (events ,(bs:pointer '*)))))

(define-bytestructure-class <wl-interface> ()
  %wl-interface-struct
  wrap-wl-interface unwrap-wl-interface wl-interface?
  (name #:accessor .name)
  (version #:accessor .version)
  (method-count #:accessor .method-count)
  (methods #:accessor .methods
           #:allocation #:virtual
           #:slot-ref
           (lambda (o)
             (let* ((bs (get-bytestructure o))
                    (method-count (bytestructure-ref bs 'method-count))
                    (methods (pointer->bytestructure
                              (make-pointer (bytestructure-ref bs 'methods))
                              (bs:vector method-count %wl-message-struct))))
               (map (lambda (n) (wrap-wl-message
                                 (bytestructure-ref methods n)))
                    (iota method-count))))
           #:slot-set! (const #f))
  (event-count #:accessor .event-count)
  (events #:allocation #:virtual
          #:slot-ref
          (lambda (o)
            (let* ((bs (get-bytestructure o))
                   (event-count (bytestructure-ref bs 'event-count))
                   (events (pointer->bytestructure
                            (make-pointer (bytestructure-ref bs 'events))
                            (bs:vector event-count %wl-message-struct))))
              (map (lambda (n)
                     (wrap-wl-message
                      (bytestructure-ref events n)))
                   (iota event-count))))
          #:slot-set! (const #f)
          #:accessor .events))
