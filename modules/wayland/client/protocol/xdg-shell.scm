(define-module (wayland client protocol xdg-shell)
  #:use-module (wayland client protocol wayland)
  #:use-module (bytestructure-class)
  #:use-module (wayland scanner)
  #:use-module (wayland config))
(use-wayland-protocol
 ("wayland-protocols/stable/xdg-shell/xdg-shell.xml"
  #:type client))
