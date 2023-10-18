(use-modules
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (gnu packages freedesktop)
 (guix git-download)
 (guix gexp)
 (guix build-system gnu)
 (guix build-system meson)
 (gnu packages pciutils)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages linux)
 (gnu packages xdisorg)
 (gnu packages gl)
 (gnu packages admin)
 (gnu packages xorg)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages file)
 (gnu packages build-tools))

(define-public wlroots
  (package
    (name "wlroots")
    (version "0.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m12nv6avgnz626h3giqp6gcx44w1wq6z0jy780mx8z255ic7q15"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t))
         (add-before 'configure 'fix-meson-file
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (substitute* "backend/drm/meson.build"
               (("/usr/share/hwdata/pnp.ids")
                (string-append (assoc-ref (or native-inputs inputs) "hwdata")
                               "/share/hwdata/pnp.ids")))))
         (add-after 'install 'copy-protocols
           ;; NOTE: exposed protocols files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "protocols")
             (copy-recursively
              "protocol" ;; why singular?!
              (string-append (assoc-ref outputs "out")
                             "/protocols"))
             #t)))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
      eudev
      libinput-minimal
      libxkbcommon
      mesa
      pixman
      libseat
      wayland
      wayland-protocols
      xcb-util-errors
      xcb-util-wm
      xorg-server-xwayland))
    (native-inputs
     (cons*
      `(,hwdata "pnp")
      pkg-config
      wayland
      (if (%current-target-system)
          (list pkg-config-for-build)
          '())))
    (home-page "https://gitlab.freedesktop.org/wlroots/wlroots/")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))

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
      #:configure-flags '(list "--disable-static")
      #:make-flags '(list "GUILE_AUTO_COMPILE=0")
      #:phases
      #~(modify-phases
            %standard-phases
          (add-before 'build 'load-extension
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib")))
                (invoke "make" "install"
                        "-C" "libguile-wayland"
                        "-j" (number->string
                              (parallel-job-count)))
                (substitute* (find-files "." "\\.scm$")
                  (("\"libguile-wayland\"")
                   (string-append "\"" lib "/libguile-wayland\"")))))))))
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
