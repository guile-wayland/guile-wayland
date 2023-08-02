#!/usr/bin/env -S guile -e main
!#
(use-modules
 (wayland server listener)
 (wayland server display)
 (wayland server client))

(define (main . _)
  (let ((w-display (wl-display-create)))
    (unless w-display
      (display "Unable to create Wayland display.\n")
      (exit 1))
    (let ((socket (wl-display-add-socket-auto w-display)))
      (unless socket
        (display "Unable to add socket to Wayland display.\n")
        (exit 1))
      (format #t "Running Wayland display on ~S\n" socket))

    (let* ((client-destroy-listener
            (make-wl-listener
             (lambda (listener pointer)
               (let ((wc (wrap-wl-client pointer)))
                 (display "Client destroy: ") (display wc) (newline)))))
           (client-created-listener
            (make-wl-listener
             (lambda (listener pointer)
               (let ((wc (wrap-wl-client pointer)))
                 (wl-client-add-destroy-listener wc client-destroy-listener)
                 (display "New client: ") (display wc) (newline))))))
      (wl-display-add-client-created-listener
       w-display client-created-listener))
    (wl-display-run w-display)
    (wl-display-destroy w-display)
    (exit 0)))
