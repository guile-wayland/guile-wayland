#!/usr/bin/env -S guile -e main
!#
(use-modules (wayland display))


(define (main . _)
  (let* ((w-display (wl-display-connect (getenv "WAYLAND_DISPLAY"))))
    (unless w-display
      (display "Unable to connect to wayland compositor")
      (exit -1))
    (display "connect to wayland compositor: ")
    (display w-display)
    (newline)
    (wl-display-disconnect w-display)))