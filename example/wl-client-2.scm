#!/usr/bin/env -S guile -e main
!#
(use-modules (wayland display)
             (wayland proxy)
             (wayland registry))


(define (main . _)
  (let* ((w-display (wl-display-connect)))
    (unless w-display
      (display "Unable to connect to wayland compositor")
      (newline)
      (exit -1))
    (display "connect to wayland compositor: ")
    (display w-display)
    (newline)
    (let ((registry (wl-display-get-registry w-display))
          (listener (make-wl-registry-listener
                     (lambda (data registry name interface version)
                       (pk 'add data registry name interface version))
                     (lambda (data registry name)
                       (pk 'remove data registry name)))))
      (wl-registry-add-listener registry listener))
    (wl-display-roundtrip w-display)
    (wl-display-disconnect w-display)))
(main)
