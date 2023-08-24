(define-module (tests display)
  #:use-module ((wayland client display) #:prefix c:)
  #:use-module ((wayland client protocol wayland) #:prefix cp:)
  #:use-module (wayland list)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(test-group "list"
  (test-assert "make-wl-list"
    (wl-list? (make-wl-list)))
  (test-assert "wl-list-empty: 0"
    (wl-list-empty (make-wl-list)))
  (test-equal "wl-list-length: 0"
    0 (wl-list-length (make-wl-list)))
  (let* ((l (make-wl-list))
         (o (wl-list-insert l (make-wl-list))))
    (test-equal "wl-list-length: 1"
      1 (wl-list-length l)))
  (let* ((l (make-wl-list))
         (o (wl-list-insert l (make-wl-list))))
    (test-equal "wl-list-length: 1"
      1 (wl-list-length l))))
