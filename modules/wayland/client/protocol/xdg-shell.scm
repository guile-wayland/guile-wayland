(define-module (wayland client protocol xdg-shell)
  #:use-module (wayland client protocol wayland)
  #:use-module (bytestructure-class)
  #:use-module (wayland scanner)
  #:use-module (wayland config)
  #:use-module ((wayland util)
                #:select (xml)))

(eval-when (compile)
  (define xdg-shell.xml
    (xml "xdg-shell")))

(use-wayland-protocol (xdg-shell.xml #:type client))
