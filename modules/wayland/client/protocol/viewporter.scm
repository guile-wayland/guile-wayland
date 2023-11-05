(define-module (wayland client protocol viewporter)
  #:use-module (bytestructure-class)
  #:use-module (wayland client protocol wayland)
  ;; #:use-module (wayland client protocol viewporter)
  #:use-module (wayland scanner)
  #:use-module (wayland config))
(use-wayland-protocol
 ("wayland-protocols/stable/viewporter/viewporter.xml"
  #:type client))
