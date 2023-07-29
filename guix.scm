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

(define-public guile-bytestructure-class
  (package
    (name "guile-bytestructure-class")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Z572/guile-bytestructure-class")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lmpckqisz3viakbzwa659l86l64iyf98qskbi8ggf0m1p46rmmw"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list guile-bytestructures))
    (synopsis "bytestructure and goops")
    (description "This package combines bytestructure with goops,
and provide 4 new bytestructure-descriptor:
bs:unknow, cstring-pointer*, bs:enum, stdbool.")
    (home-page "https://github.com/Z572/guile-bytestructure-class")
    (license license:gpl3+)))

(define guile-wayland
  (package
    (name "guile-wayland")
    (version "0.1")
    (source (local-file "." "guile-wayland-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnu-build-system)
    (arguments (list
                #:configure-flags #~(list "--disable-dependency-tracking")
                #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
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
    (inputs (list guile-3.0-latest wayland))
    (propagated-inputs
     (list
      guile-bytestructure-class
      guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))

guile-wayland
