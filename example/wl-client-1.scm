#!@GUILE@
!#
(use-modules (wayland client display))


(define (main . _)
  (let* ((w-display (wl-display-connect)))
    (unless w-display
      (display "Unable to connect to wayland compositor")
      (newline)
      (exit -1))
    (display "connect to wayland compositor: ")
    (display w-display)
    (newline)
    (wl-display-disconnect w-display)))
