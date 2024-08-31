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
            wrap-wl-interface unwrap-wl-interface wl-interface?
            %make-wl-interface
            %wl-interface-update-message-types))

(define %wl-interface-struct
  (bs:struct
   `((name ,cstring-pointer*)
     (version ,int)
     (method-count ,int)
     (methods ,(bs:pointer '*))
     (event-count ,int)
     (events ,(bs:pointer '*)))))

(define %wl-message-struct
  (bs:struct
   `((name ,cstring-pointer*)
     (signature ,cstring-pointer*)
     (types ,(bs:pointer %wl-interface-struct)))))

(define-bytestructure-class <wl-message> ()
  %wl-message-struct
  wrap-wl-message unwrap-wl-message wl-message?
  (name #:accessor .name)
  (signature #:accessor .signature)
  (types #:accessor .types))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write (o <wl-message>) file)
  (let ((class (class-of o)))
    (begin
      (display "#<" file)
      (display (class-name class) file)
      (display #\space file)
      (display (.name o) file)
      (display #\space file)
      (write (.signature o)  file)
      (display #\space file)
      (display-address o file)
      (display #\> file))))

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
                    (method-count (bytestructure-ref bs 'method-count)))
               (if (zero? method-count)
                   '()
                   (let* ((methods (pointer->bytestructure
                                    (make-pointer (bytestructure-ref bs 'methods))
                                    (bs:vector method-count %wl-message-struct))))
                     (map (lambda (n) (wrap-wl-message
                                       (bytestructure-ref methods n)))
                          (iota method-count))))))
           #:slot-set! (const #f))
  (event-count #:accessor .event-count)
  (events #:allocation #:virtual
          #:slot-ref
          (lambda (o)
            (let* ((bs (get-bytestructure o))
                   (event-count (bytestructure-ref bs 'event-count)))
              (if (zero? event-count)
                  '()
                  (let* ((events (pointer->bytestructure
                                  (make-pointer (bytestructure-ref bs 'events))
                                  (bs:vector event-count %wl-message-struct))))
                    (map (lambda (n)
                           (wrap-wl-message
                            (bytestructure-ref events n)))
                         (iota event-count))))))
          #:slot-set! (const #f)
          #:accessor .events))

(define-method (write (o <wl-interface>) file)
  (let ((class (class-of o)))
    (begin
      (display "#<" file)
      (display (class-name class) file)
      (display #\space file)
      (display (.name o) file)
      (display #\space file)
      (display #\v file)
      (display (.version o) file)
      (display #\space file)
      (display-address o file)
      (display #\> file))))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wl_interface"))
