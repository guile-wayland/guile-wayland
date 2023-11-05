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

(define %source-dir (dirname (current-filename)))

(define-public guile-bytestructure-class
  (package
   (name "guile-bytestructure-class")
   (version "0.2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Z572/guile-bytestructure-class")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0y3sryy79arp3f5smyxn8w7zra3j4bb0qdpl1p0bld3jicc4s86a"))))
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
   (version "0.0.2")
   (source (local-file %source-dir "guile-wayland-checkout"
                       #:recursive? #t
                       #:select? (git-predicate %source-dir)))
   (build-system gnu-build-system)
   (arguments
    (list
     ;; #:configure-flags '(list "--disable-static")
     #:make-flags '(list "GUILE_AUTO_COMPILE=0")
     #:phases
     #~(modify-phases
        %standard-phases
        (add-after 'build 'load-extension
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* (find-files "." "\\.scm")
                                   (("\\(load-extension \"libguile-wayland\" *\"(.*)\"\\)" _ o)
                                    (string-append
                                     (object->string
                                      `(or (false-if-exception
                                            (load-extension "libguile-wayland" ,o))
                                           (load-extension
                                            ,(string-append
                                              #$output
                                              "/lib/libguile-wayland.so")
                                            ,o)))))))))))
   (native-inputs
    (list autoconf
          automake
          libtool
          pkg-config
          texinfo
          guile-3.0-latest))
   (inputs (list guile-3.0-latest wayland wayland-protocols))
   (propagated-inputs
    (list
     guile-bytestructure-class
     guile-bytestructures))
   (synopsis "")
   (description "")
   (home-page "https://github.com/guile-wayland/guile-wayland")
   (license license:gpl3+)))

guile-wayland
