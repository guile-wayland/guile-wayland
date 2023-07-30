(define-module (wayland client))

(eval-when (eval load compile)
  (begin
    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module)))
           (submodule-interface (resolve-interface '(wayland server client))))
      (module-use! current-module submodule-interface)
      (module-use! current-module-interface submodule-interface))))



;; "wl_client_get_credentials"
