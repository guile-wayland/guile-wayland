(define-module (wayland base)
  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:use-module (system foreign)
  #:use-module (bytestructure-class)
  #:re-export (bs:unknow
               stdbool
               bs:enum
               define-bytestructure-class))
