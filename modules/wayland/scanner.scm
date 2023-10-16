(define-module (wayland scanner)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign-library) #:select (foreign-library-pointer))
  #:use-module (wayland config)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (wayland util)
  #:use-module (wayland client proxy)
  #:use-module (wayland interface)
  #:autoload (wayland server client) (wrap-wl-client)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wayland server resource)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module (bytestructures guile)
  #:use-module (sxml simple)
  #:use-module (sxml match)
  #:use-module (bytestructure-class)
  #:export (use-wayland-protocol
            guile-wayland-protocol-path))

(define-syntax define-type
  (lambda (x)
    (define (change-syntax obj proc)
      (datum->syntax
       x (proc (syntax->datum obj))
       #:source obj))
    (syntax-case x ()
      ((_ name (childs ...))
       (with-syntax ((cname (change-syntax #'name (cut symbol-append '< <> '>)))
                     (make (change-syntax #'name (cut symbol-append '%make- <>)))
                     (check (change-syntax #'name (cut symbol-append  <> '?)))
                     ((get-childs ...)
                      (map (lambda (o)
                             (change-syntax
                              o
                              (cut symbol-append (syntax->datum #'name)
                                   '- <>))) #'(childs ...))))
         #`(begin (define-record-type cname
                    (make childs ...) check (childs get-childs) ...)))))))
(define-type protocol (name copyright interfaces))
(define-type interface (name version requests events enums))
(define-type message (name destructor since type args))
(define-type enum (name values bitfield?))
(define-type arg (name type interface allow-null?))

(define (file->sxml file)
  (call-with-input-file file (cut xml->sxml <> #:trim-whitespace? #t)))
(define -->_ (cut string-map (lambda (a) (if (char=? a #\-) #\_ a)) <>))
(define _->- (cut string-map (lambda (a) (if (char=? a #\_) #\- a)) <>))
(define string->keyword (compose symbol->keyword string->symbol))
(define* (make-%interface-name name #:key (string? #f))
  ((compose (if string? identity string->symbol))
   (string-append "%" (_->- name) "-interface")))
(define* (make-%wrap-name name #:key (string? #f))
  ((compose (if string? identity string->symbol))
   (string-append "wrap-" (_->- name))))
(define* (make-%unwrap-name name #:key (string? #f))
  ((compose (if string? identity string->symbol))
   (string-append "unwrap-" (_->- name))))
(define (sxml->protocol sxml)
  (sxml-match sxml
    ((*TOP* ,_ (protocol (@ (name ,(_->- -> name)))
                         (copyright ,copyright) ,interfaces ...))
     (%make-protocol name copyright (map sxml->interface (assoc-remove! interfaces 'description))))))

(define (sxml->interface sxml)
  (sxml-match (assoc-remove! sxml 'description)
    ((interface (@ (name ,name) (version ,version*)) . ,rest)
     (let* ((childs (list-transduce
                     (compose (tpartition first)
                              (tmap (lambda (x)
                                      (cons (caar x)
                                            (map sxml->message/enum x)))))
                     rcons
                     (sort rest
                           (lambda (x x2)
                             (string>
                              (symbol->string (first x))
                              (symbol->string (first x2))))))))
       (%make-interface (_->- name) (string->number version*)
                        (or (assoc-ref childs 'request) '())
                        (or (assoc-ref childs 'event) '())
                        (or (assoc-ref childs 'enum) '()))))
    (,otherwise
     (error "Can't match in (sxml->interface)~%" sxml))))

(define (c-num->scm-num s)
  (or (if (string-prefix? "0x" s )
          (string->number (substring s 2) 16)
          (string->number s 10))
      (throw 'convert-c-num-fail s)))

(define (sxml->message/enum sxml)
  (sxml-match (assoc-remove! sxml 'description)
    ((event (@ (name ,name) (since (,since #f)) (type (,type #f)))
            (arg (@ (type ,arg-type)
                    (name ,(_->- -> arg-name))
                    (interface (,arg-interface-type #f))
                    (allow-null (,allow-null #f)))) ...)
     (%make-message (_->- name)
                    (and type (string=? type "destructor"))
                    since
                    'event
                    (list (%make-arg arg-name arg-type
                                     (and arg-interface-type
                                          (_->- arg-interface-type))
                                     (->bool allow-null)) ...)))
    ((event (@ (name ,name) (since (,since #f)) (type (,type #f))))
     (%make-message (_->- name)
                    (and type (string=? type "destructor"))
                    since
                    'event
                    (list)))
    ((request (@ (name ,name) (since (,since #f)) (type (,type #f)))
              (arg (@ (type ,arg-type)
                      (name ,(_->- -> arg-name))
                      (interface (,arg-interface-type #f))
                      (allow-null (,allow-null #f)))) ...)
     (%make-message (_->- name)
                    (and type (string=? type "destructor"))
                    since 'request
                    (list (%make-arg arg-name arg-type
                                     (and arg-interface-type
                                          (_->- arg-interface-type))
                                     (->bool allow-null)) ...)))
    ((request (@ (name ,name) (type (,type #f))))
     (%make-message (_->- name)
                    (and type (string=? type "destructor"))
                    #f 'request
                    (list)))
    ((enum (@ (name ,name) (bitfield (,bitfield #f)))
           (entry (@ (name ,entry-name) (value ,entry-value)) . ,rest) ...)
     (%make-enum (_->- name)
                 (list (cons entry-name (c-num->scm-num entry-value)) ...)
                 (->bool bitfield)))
    (,rest (throw 'no-found sxml))))

(define (new-id-handle arg is no-i no)
  (if (string= (arg-type arg) "new_id") (if (arg-interface arg) is no-i) no))
(define (is-nullable-type? type)
  (member type '("string" "object")))

(define (arg-type->ffi type)
  (cond ((equal? type "uint") (list #`ffi:uint32))
        ((member type '("int" "fixed")) (list #`ffi:int32))
        ((equal? type "fd") (list #`ffi:int))
        ((member type '("new_id" "string" "array" "object")) (list #`'*))))
(define (message-singature m)
  (apply string-append
         (or (message-since m) "")
         (map arg->signature (message-args m))))
(define (arg->signature arg)
  (define itype (arg-interface arg))
  (define allow-null? (arg-allow-null? arg))
  (string-append
   (if (or (is-nullable-type? (arg-type arg)) allow-null?) "?" "")
   (case (string->symbol (arg-type arg))
     ((int) "i")
     ((new_id) (if itype "n" "sun"))
     ((uint) "u")
     ((fixed) "f")
     ((string) "s")
     ((object) "o")
     ((array) "a")
     ((fd) "h"))))


(define guile-wayland-protocol-path
  (make-parameter
   (cons %wayland-protocols-dir
         (or (parse-path (getenv "GUILE_WAYLAND_PROTOCOL_PATH"))
             (list)))))
(define (find-protocol path)
  (or (search-path
       (guile-wayland-protocol-path)
       path)
      path))

(define-syntax use-wayland-protocol
  (lambda (x)
    (define ->syntax (cut datum->syntax x <>))
    (define* (protocol->code protocol #:key (type 'server))
      (assert (protocol? protocol))
      (append (append-map (lambda (interface)
                            (let* ((name (interface-name interface))
                                   (requests (interface-requests interface))
                                   (events (interface-events interface)))
                              (append (interface->interface-struct interface #:type type)
                                      (interface->code interface)
                                      (append-map
                                       (cut enum->code <> name)
                                       (interface-enums interface))
                                      (apply append-map
                                             (cut message->procedure-code <> name <> type)
                                             (case type
                                               ((server) (list events (iota (length events))))
                                               ((client)(list requests (iota (length requests))))))
                                      (interface->struct-code interface type) '())))
                          (protocol-interfaces protocol))

              (append-map
               (lambda (interface)
                 (let ((m->types
                        (lambda (m)
                          (map
                           (lambda (arg)
                             (->syntax
                              (and (and (member (arg-type arg)
                                                '("new_id" "object"))
                                        (arg-interface arg))
                                   (make-%interface-name (arg-interface arg)))))
                           (message-args m)))))
                   (with-syntax ((iname (->syntax (make-%interface-name (interface-name interface))))
                                 (((requests-types ...) ...) (map m->types (interface-requests interface)))
                                 (((events-types ...) ...) (map m->types (interface-events interface))))
                     #`((%wl-interface-update-message-types
                         iname
                         (list (list requests-types ...) ...)
                         (list (list events-types ...) ...))))))
               (protocol-interfaces protocol))))

    (define* (arg->unwrap-sexp i wraped-obj #:optional (server? #t))
      (define type (arg-type i))
      (cond
       ((or (member type '("fd" "int" "uint" "fixed")))
        wraped-obj)
       ((equal? type "string")
        #`(ffi:string->pointer #,wraped-obj))
       ((member type '("new_id" "object"))
        (if server?
            #`(unwrap-wl-resource #,wraped-obj)
            (if (arg-interface i)
                #`(#,(->syntax (make-%unwrap-name (arg-interface i)))
                   #,(->syntax (string->symbol (arg-name i)))))))
       ((equal? type "array") #`(unwrap-wl-array #,wraped-obj))))

    (define* (interface->code interface)
      (let* ((name (interface-name interface))

             (m->bs (lambda (x)
                      (list (-->_ (message-name x))
                            (message-singature x)))))
        (with-syntax ((%name (->syntax (make-%interface-name name)))
                      (iname (->syntax (-->_ name)))
                      (iversion (interface-version interface))
                      (((requests ...) ...) (map m->bs (interface-requests interface)))
                      (((events ...) ...) (map m->bs (interface-events interface))))
          #`((define-once %name
               (%make-wl-interface
                iname iversion
                (list (list requests ...) ...)
                (list (list events ...) ...)))
             (export %name)))))
    (define* (interface->interface-struct interface #:key (type 'server))
      (let* ((%name (string->symbol (interface-name interface))))
        (with-syntax ((bname (->syntax (symbol-append '% %name '-struct)))
                      (wrap (->syntax (symbol-append 'wrap- %name)))
                      (unwrap (->syntax (symbol-append 'unwrap- %name)))
                      (cname (->syntax (symbol-append '< %name '>)))
                      (check (->syntax (symbol-append %name '?)))
                      (supclasss (case type
                                   ((server) #'())
                                   ((client) #'(<wl-proxy>)))))
          #`((define bname (bs:unknow))
             (define-bytestructure-class cname supclasss
               bname wrap unwrap check)
             (export bname wrap unwrap check cname)
             #,@(case type
                  ((server) #'())
                  ((client)
                   (with-syntax ((add-listener-name (->syntax (symbol-append %name '-add-listener)))
                                 (unwrap-listener (->syntax (symbol-append 'unwrap- %name '-listener)))
                                 (get-version (->syntax (symbol-append %name '-get-version)))
                                 (get-user-data (->syntax (symbol-append %name '-get-user-data))))
                     #`(#,@(if (null-list? (interface-events interface))
                               #'()
                               #'((define* (add-listener-name obj listener #:optional (data ffi:%null-pointer))
                                    (assert (check obj))
                                    (wl-proxy-add-listener obj (unwrap-listener listener) data))
                                  (export add-listener-name)))
                        (define (get-version obj)
                          (assert (check obj)) (wl-proxy-get-version obj))
                        (define (get-user-data obj)
                          (assert (check obj)) (wl-proxy-get-user-data obj))
                        (export get-version get-user-data)))))))))
    (define (interface->struct-code interface type)
      (assert (interface? interface))
      (define iname (interface-name interface))
      (define need-handle-list ((case type
                                  ((server) interface-requests)
                                  ((client) interface-events)) interface))
      (define %name (string-append iname
                                   (case type
                                     ((server)  "-interface")
                                     ((client) "-listener"))))
      (if (null-list? need-handle-list)
          #'()
          (with-syntax ((bs (->syntax (string->symbol (string-append "%" %name "-struct"))))
                        (cname (->syntax (string->symbol (string-append "<" %name  ">"))))
                        (wrap (->syntax (make-%wrap-name %name)))
                        (unwrap (->syntax (make-%unwrap-name %name)))
                        (check (->syntax (string->symbol (string-append %name "?"))))
                        (((keyword slot-name lambda-args (call-args ...) (ffi-descs ...)) ...)
                         (map
                          (lambda (m)
                            (list (string->keyword (message-name m))
                                  (->syntax (string->symbol (message-name m)))
                                  (append (case type
                                            ((server) #`(wl-client resource))
                                            ((client) #`(data #,(->syntax (string->symbol iname)))) )
                                          (append-map
                                           (lambda (arg)
                                             (with-syntax ((arg-name (->syntax (string->symbol (arg-name arg)))))
                                               (cond ((and (eq? type 'server) (equal? (arg-type arg) "new_id") (not (arg-interface arg)))
                                                      #`(interface version* arg-name))
                                                     (else #`(arg-name)))))
                                           (message-args m)))
                                  (append (case type
                                            ((server)#`((wrap-wl-client wl-client)
                                                        (wrap-wl-resource resource)))
                                            ((client)#`(data (#,(->syntax
                                                                 (make-%wrap-name iname))
                                                              #,(->syntax (string->symbol iname))))))
                                          (append-map
                                           (lambda (arg)
                                             (let ((name (arg-name arg))
                                                   (atype (arg-type arg))
                                                   (itype (arg-interface arg)))
                                               (with-syntax ((name* (->syntax (string->symbol name))))
                                                 (cond ((and (eq? type 'server) (equal? atype "object"))
                                                        #`((wrap-wl-resource name*)))
                                                       ((and (eq? type 'server) (equal? atype "new_id") (not itype))
                                                        #`((ffi:pointer->string interface) version* name*))
                                                       ((and (eq? type 'client) (equal? atype "new_id"))
                                                        #`(name*))
                                                       ((or (member atype '("int" "fd" "new_id" "uint" "fixed")))
                                                        #`(name*))
                                                       ((equal? atype "string")
                                                        #`((ffi:pointer->string name*)))
                                                       ((equal? atype "object")
                                                        #`((#,(->syntax (make-%wrap-name iname))
                                                            name*)))
                                                       ((equal? atype "array")
                                                        #`((wrap-wl-array name*)))))))
                                           (message-args m)))
                                  (append-map
                                   (lambda (arg)
                                     (let ((name (arg-name arg))
                                           (atype (arg-type arg))
                                           (itype (arg-interface arg)))
                                       (cond ((and (eq? type 'server) (equal? atype "new_id") (not itype))
                                              #`('* ffi:uint32 ))
                                             (else (arg-type->ffi atype)))))
                                   (message-args m))))
                          need-handle-list)))
            #`((define-public bs (bs:struct `((slot-name ,(bs:pointer '*)) ...)))
               (define-bytestructure-class cname ()
                 bs wrap unwrap check (slot-name #:init-keyword keyword) ...)
               (export bs wrap unwrap check cname)
               (define-method (initialize (obj cname) initargs)
                 (next-method
                  obj (append (list keyword
                                    (let ((proc (get-keyword keyword initargs #f)))
                                      (cond ((ffi:pointer? proc) proc)
                                            ((procedure? proc)
                                             (ffi:procedure->pointer
                                              ffi:void
                                              (lambda lambda-args
                                                (proc call-args ...))
                                              (list '* '* ffi-descs ...)))
                                            ((->bool proc) (throw 'wayland-init-fail keyword proc))
                                            (else ffi:%null-pointer)))) ...
                                            initargs)))))))
    (define (message->procedure-code request iname index type)
      (assert (and (message? request)
                   (eq? (message-type request)
                        (case type
                          ((server) 'event)
                          ((client) 'request)))))
      (case type
        ((server)
         (let* ((args (message-args request))
                (name (message-name request)))
           (with-syntax ((const-name
                          (->syntax
                           (string->symbol
                            (string-upcase
                             (string-append (-->_ iname) "_" name)))))
                         (func-name
                          (->syntax (string->symbol
                                     (string-append
                                      iname "-send-" name))))
                         ((ffi-rest-args ...)
                          (append-map arg-type->ffi (map arg-type args)))
                         ((arglist ...) (map (compose ->syntax
                                                      string->symbol
                                                      arg-name)
                                             args))
                         ((call-arg-list ...)
                          (map
                           (lambda (i)
                             (arg->unwrap-sexp
                              i
                              (->syntax (string->symbol (arg-name i)))))
                           args)))
             #`((define-public const-name #,(->syntax index))
                (define-public func-name
                  (let ((wl-resource-post-event
                         (wayland-server->procedure
                          ffi:void
                          "wl_resource_post_event"
                          (list '* ffi:uint32 ffi-rest-args ...))))
                    (lambda (resource arglist ...)
                      (wl-resource-post-event
                       resource const-name
                       call-arg-list ...))))))))

        ((client)
         (let* ((pname (string-append iname "-" (message-name request)))
                (args (message-args request))
                (ret (find (lambda (i) (string= (arg-type i) "new_id")) args)))
           (with-syntax ((pname (->syntax (string->symbol pname)))
                         (opcode (->syntax index))
                         (opcode-name (->syntax
                                       (string->symbol
                                        (string-upcase (-->_ pname)))))
                         (obj (->syntax (string->symbol iname)))
                         (destructor-flag (if (message-destructor request)
                                              #'WL_MARSHAL_FLAG_DESTROY
                                              #'0))
                         ((ffi-rest-args ...)
                          (append-map
                           (lambda (i)
                             (new-id-handle
                              i
                              (list #`'*)
                              (list #`'* #'ffi:uint32 #`'*)
                              (arg-type->ffi (arg-type i))))
                           args))
                         ((lambda-args ...)
                          (append-map
                           (lambda (i)
                             (new-id-handle
                              i
                              (list)
                              #'(%arg-interface %arg-version)
                              (list (->syntax (string->symbol (arg-name i))))))
                           args))
                         ((rest-args ...)
                          (append-map
                           (lambda (i)
                             (new-id-handle
                              i
                              (list #`ffi:%null-pointer)
                              (list #`(ffi:string->pointer (.name %arg-interface)) #'%arg-version
                                    #`ffi:%null-pointer)
                              (list (arg->unwrap-sexp i (->syntax (string->symbol (arg-name i))) #f))))
                           args) ))
             #`((define-public opcode-name opcode)
                (define-public pname
                  (let ((%wl-proxy-marshal-flags
                         (wayland-client->procedure
                          '*
                          "wl_proxy_marshal_flags"
                          (list
                           '* ffi:uint32 '* ffi:uint32 ffi:uint32
                           ffi-rest-args ...))))
                    (lambda (obj lambda-args ...)
                      (let ((%arg-out
                             (wrap-wl-proxy
                              (%wl-proxy-marshal-flags
                               (#,(->syntax (make-%unwrap-name iname)) obj)
                               opcode-name
                               #,@(if ret
                                      (or (and=> (arg-interface ret)
                                                 (lambda (x)
                                                   (with-syntax
                                                       ((iname (->syntax
                                                                (make-%interface-name x))))
                                                     #`((unwrap-wl-interface iname)
                                                        (wl-proxy-get-version obj)))))
                                          #'((unwrap-wl-interface %arg-interface) %arg-version))
                                      (list #`ffi:%null-pointer #`(wl-proxy-get-version obj)))
                               destructor-flag rest-args ...))))
                        %arg-out
                        #,(if ret
                              (or (and=> (arg-interface ret)
                                         (lambda (x)
                                           #`(#,(->syntax
                                                 (make-%wrap-name x))
                                              %arg-out)))
                                  #'%arg-out)
                              (->syntax *unspecified*))))))))))))

    (define (enum->code enum in-name)
      (assert (enum? enum))
      (let ((name (string-append in-name "-" (enum-name enum))))
        (with-syntax ((bs-name (->syntax (string->symbol
                                          (string-append "%" name "-enum"))))
                      (((enames evalues) ...)
                       (map (lambda (x)
                              #`(#,(->syntax
                                    (string->symbol
                                     (string-upcase
                                      (-->_ (string-append
                                             name "_" (car x))))))
                                 #,(cdr x)))
                            (enum-values enum))))
          #`((define-public bs-name (bs:enum '((enames evalues) ...)))
             (define-public enames evalues) ...))))
    (syntax-case x ()
      ((_ (xml-path a ...))
       #`(begin #,@(apply protocol->code
                          (let ((path (syntax->datum #'xml-path)))
                            (sxml->protocol
                             (file->sxml
                              (cond ((string? path)
                                     (find-protocol path))
                                    ((symbol? path)
                                     (module-ref (current-module) path))))))
                          (syntax->datum #'(a ...))))))))
