(define-module (wayland proxy)
  #:use-module (ice-9 format)
  #:use-module (wayland util)
  #:use-module (bytestructure-class)
  #:use-module (oop goops)
  #:use-module (wayland interface)
  #:use-module ((system foreign) #:select (void
                                           make-pointer
                                           %null-pointer
                                           pointer-address
                                           pointer->string
                                           procedure->pointer
                                           define-wrapped-pointer-type
                                           (uint32 . ffi:uint32)
                                           (int . ffi:int)))
  #:export (%wl-proxy-struct
            <wl-proxy>
            wrap-wl-proxy
            wl-proxy-get-class
            wl-proxy-add-listener
            wl-proxy-marshal-constructor
            wl-proxy-marshal-constructor-versioned
            wl-proxy-get-user-data
            wl-proxy-get-version))

;; (define-wrapped-pointer-type wl-proxy
;;   wl-proxy?
;;   wrap-wl-proxy unwrap-wl-proxy
;;   (lambda (b p)
;;     (format p "#<wl-proxy ~x>" (pointer-address (unwrap-wl-proxy b)))))

(define %wl-proxy-struct (bs:unknow))
(define-bytestructure-class <wl-proxy> ()
  %wl-proxy-struct
  wrap-wl-proxy unwrap-wl-proxy wl-proxy?)

(define %wl-proxy-create (wayland-client->procedure '* "wl_proxy_create" '(* *)))

(define-method (wl-proxy-create (factory <wl-proxy>) interface)
  (wrap-wl-proxy (%wl-proxy-create (unwrap-wl-proxy factory)
                                   interface)))

(define %wl-proxy-get-class (wayland-client->procedure '* "wl_proxy_get_class" '(*)))
(define-method (wl-proxy-get-class (proxy <wl-proxy>))
  (pointer->string (%wl-proxy-get-class (unwrap-wl-proxy proxy))))
(define %wl-proxy-marshal-constructor
  (wayland-client->procedure
   '*
   "wl_proxy_marshal_constructor"
   (list '* ffi:uint32 '*)))

(define-method (wl-proxy-marshal-constructor (proxy <wl-proxy>) opcode interface)
  (%wl-proxy-marshal-constructor
   (unwrap-wl-proxy proxy)
   opcode
   interface))

(define %wl-proxy-marshal-constructor-versioned
  (wayland-client->procedure
   '*
   "wl_proxy_marshal_constructor_versioned"
   (list '* ffi:uint32 '* ffi:uint32 ffi:uint32 '* ffi:uint32 '*)))
(define-method (wl-proxy-marshal-constructor-versioned (proxy <wl-proxy>) opcode interface version . other)
  (apply %wl-proxy-marshal-constructor-versioned
         (unwrap-wl-proxy proxy)
         opcode
         interface
         version
         other))

(define %wl-proxy-add-listener
  (wayland-client->procedure ffi:int "wl_proxy_add_listener" (list '* '* '*)))
(define-method (wl-proxy-add-listener (proxy <wl-proxy>) implementation data)
  (%wl-proxy-add-listener
   (unwrap-wl-proxy proxy)
   implementation
   data))

(define-public (wl-proxy-marshal-flags proxy opcode interface version . flags)
  (let ((% (wayland-client->procedure
            '* "wl_proxy_marshal_flags"
            `(* ,ffi:uint32 * ,ffi:uint32
                ,@(make-list (length flags) ffi:uint32)))))
    (wrap-wl-proxy
     (apply % (unwrap-wl-proxy proxy)
            opcode (unwrap-wl-interface interface)
            version flags))))

(define %wl-proxy-get-version
  (wayland-client->procedure
   ffi:uint32 "wl_proxy_get_version" '(*)))

(define-method (wl-proxy-get-version (proxy <wl-proxy>))
  (%wl-proxy-get-version (unwrap-wl-proxy proxy)))

(define %wl-proxy-get-user-data
  (wayland-client->procedure
   '* "wl_proxy_get_user_data" '(*)))

(define-method (wl-proxy-get-user-data (proxy <wl-proxy>))
  (%wl-proxy-get-user-data (unwrap-wl-proxy proxy)))
