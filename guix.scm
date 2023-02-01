(use-modules
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (gnu packages freedesktop)
 (guix git-download)
 (guix gexp)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages file)
 (gnu packages build-tools))
(define wayland-next
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fwad6w5jm32c04wh4gca7d1ixdj4b9dnsiy1h6qd9nxs0w47wwy"))))))
(define guile-wayland
  (package
    (name "guile-wayland")
    (version "0.1")
    (source (local-file "." "guile-wayland-checkout"
                        #:recursive? #t
                        #:select? (git-predicate (dirname (current-filename)))))
    (build-system gnu-build-system)
    (arguments (list #:make-flags '(list "GUILE_AUTO_COMPILE=0")
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-after 'build 'load-extension
                           (lambda* (#:key outputs #:allow-other-keys)
                             (substitute*
                                 (find-files "." ".*\\.scm")
                               (("\\(load-extension \"libguile-wayland\" *\"(.*)\"\\)" _ o)
                                (string-append
                                 (object->string
                                  `(or (false-if-exception (load-extension "libguile-wayland" ,o))
                                       (load-extension
                                        ,(string-append
                                          (assoc-ref outputs "out")
                                          "/lib/libguile-wayland.so")
                                        ,o)))))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           texinfo
           guile-3.0-latest))
    (inputs (list guile-3.0-latest wayland-next))
    (propagated-inputs
     (list
      (primitive-load
       (string-append (dirname (dirname (current-filename)))
                      "/guile-bytestructure-class/guix.scm"))
      guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-wayland
;; (list
;;  info-reader
;;  bear)
