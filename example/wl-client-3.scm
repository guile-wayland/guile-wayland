#!/usr/bin/env -S guile -e main
!#
(use-modules (wayland client display)
             (oop goops)
             (ice-9 format)
             (wayland interface)
             (wayland proxy)
             (wayland client protocol wayland)
             (wayland client protocol xdg-shell))

(define compositor (make-parameter #f))
(define shm (make-parameter #f))
(define xdg-wm-base (make-parameter #f))
(define xdg-toplevel (make-parameter #f))
(define wsurface (make-parameter #f))







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
          (listener (make <wl-registry-listener>
                      #:global
                      (lambda (data registry name interface version)
                        (cond ((string=? "wl_compositor" interface)
                               (compositor (wrap-wl-compositor
                                            (wl-registry-bind
                                             registry name
                                             %wl-compositor-interface 3))))
                              ((string=? "wl_shm" interface)
                               (shm (wrap-wl-shm
                                     (wl-registry-bind registry name %wl-shm-interface 1))))
                              ((string=? "xdg_wm_base" interface)
                               (xdg-wm-base
                                (wrap-xdg-wm-base
                                 (wl-registry-bind registry name %xdg-wm-base-interface 1)))
                               (xdg-wm-base-add-listener
                                (xdg-wm-base)
                                (make <xdg-wm-base-listener>
                                  #:ping (lambda (data base  serial)
                                           (pk 'ping)
                                           (xdg-wm-base-pong base serial))))
                               )))
                      #:global-remove
                      (lambda (data registry name)
                        (pk 'remove data registry name)))))
      (wl-registry-add-listener registry listener))
    (wl-display-roundtrip w-display)
    (if (and (compositor) (shm) (xdg-wm-base))
        (format #t "Got them all!~%" )
        (begin (format #t "Some required globals unavailable~%")
               (exit 1)))
    (let* ((surface (wl-compositor-create-surface (compositor)))
           (xdg-surface (xdg-wm-base-get-xdg-surface (xdg-wm-base) surface)))
      (xdg-surface-add-listener
       xdg-surface
       (make <xdg-surface-listener>
         #:configure
         (lambda (data xdg-surface serial)
           (xdg-surface-ack-configure xdg-surface serial)
           (wl-surface-commit (wsurface)))))
      (wsurface surface)
      (xdg-toplevel (xdg-surface-get-toplevel xdg-surface))

      (xdg-toplevel-add-listener
       (xdg-toplevel)
       (make <xdg-toplevel-listener>
         #:configure
         (lambda (data xdg width height states)
           (pk 'config xdg width height states)
           )
         #:close
         (lambda (data xdg-toplevel)
           (pk 'close xdg-toplevel)
           (format #t "exit!~%")
           (exit 0))))

      (xdg-toplevel-set-title (xdg-toplevel) "Example client")
      (xdg-toplevel-set-app-id (xdg-toplevel) "guile-wayland-example")
      (wl-surface-commit surface))
    (while #t
      (wl-display-dispatch w-display))))
