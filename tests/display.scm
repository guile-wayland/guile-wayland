(define-module (tests display)
  #:use-module ((wayland client display) #:prefix c:)
  #:use-module ((wayland client protocol wayland) #:prefix cp:)
  #:use-module (wayland server display)
  #:use-module (wayland server event-loop)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(setenv "XDG_RUNTIME_DIR"
        (mkdtemp "/tmp/test-guile-wayland-XXXXXX"))

(test-group "server"
  (let ((d (wl-display-create)))
    (test-assert "server create"
      (begin d))
    (test-assert "wl-display-add-socket-auto"
      (string? (wl-display-add-socket-auto d)))
    (test-assert "wl-display-get-event-loop"
      (wl-event-loop? (wl-display-get-event-loop d)))
    (test-assert "wl-display-terminate"
      (begin (wl-display-terminate d)))
    (test-assert "server destroy"
      (begin (wl-display-destroy d)))))

(test-group "client"
  (let ((cd (c:wl-display-connect
             (wl-display-add-socket-auto
              (wl-display-create)))))
    (test-assert "wl-display-connect" cd)
    (test-assert "wl-display-get-fd"
      (let ((fd (c:wl-display-get-fd cd)))
        (and (number? fd)
             (file-exists?
              (string-append
               "/proc/self/fd/"
               (number->string fd))))))
    (test-assert "get-registry"
      (cp:wl-registry? (cp:wl-display-get-registry cd)))
    (test-assert "wl-display-disconnect"
      (c:wl-display-disconnect cd))))
