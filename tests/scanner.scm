(define-module (tests scanner)
  ;; #:use-module ((wayland client display) #:prefix c:)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland scanner)
  #:use-module (wayland list)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(test-group "scanner"
  (test-error "guile-wayland-protocol-path: fail"
              'system-error
              (let ((m (make-fresh-user-module)))
                (module-use! m (resolve-interface
                                '(wayland client protocol wayland)))
                (module-use! m (resolve-interface '(wayland scanner)))
                (eval '(use-wayland-protocol ("idle.xml" #:type client)) m)))
  (test-assert "guile-wayland-protocol-path"
    (let ((m (make-fresh-user-module)))
      (module-use! m (resolve-interface '(wayland client protocol wayland)))
      (module-use! m (resolve-interface '(wayland scanner)))
      (parameterize ((guile-wayland-protocol-path
                      (list (dirname (current-filename)))))
        (eval '(use-wayland-protocol ("idle.xml" #:type client)) m)
        (module-defined?
         m
         '%org-kde-kwin-idle-struct)))))
