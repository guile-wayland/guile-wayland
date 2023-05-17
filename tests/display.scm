(define-module (tests display)
  #:use-module (wayland display)
  #:use-module (wayland event-loop)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(setenv "XDG_RUNTIME_DIR" (mkdtemp "/tmp/test-guile-wayland-XXXXXX"))
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
  (let ((cd (wl-display-connect
             (wl-display-add-socket-auto
              (wl-display-create)))))
    (test-assert "wl-display-connect" cd)
    (test-assert "wl-display-get-fd"
      (let ((fd (wl-display-get-fd cd)))
        (and (number? fd)
             (file-exists?
              (string-append
               "/proc/self/fd/"
               (number->string fd))))))
    (test-assert "wl-display-disconnect"
      (wl-display-disconnect cd))))
