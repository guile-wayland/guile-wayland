(define-module (wayland  mman)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (memfd-create
            munmap
            mmap))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_mman"))

(define memfd-create
  (let ((% (foreign-library-function
            #f  "memfd_create"
            #:return-type ffi:int
            #:arg-types `(* ,ffi:unsigned-int))))
    (lambda (name flags)
      (% (ffi:string->pointer name) flags))))

(define mmap
  (let ((% (foreign-library-function
            #f  "mmap"
            #:return-type '*
            #:arg-types `(* ,ffi:size_t ,ffi:int ,ffi:int ,ffi:int ,ffi:long))))
    (lambda (addr length prot flags fd offset)
      (ffi:pointer->bytevector
       (% (or addr ffi:%null-pointer) length prot flags fd offset)
       length))))

(define munmap
  (let ((% (foreign-library-function
            #f  "munmap"
            #:return-type ffi:int
            #:arg-types `(* ,ffi:size_t))))
    (lambda* (addr)
      (%  (ffi:bytevector->pointer addr)
          (bytevector-length addr)))))

(define shm-open
  (let ((% (foreign-library-function
            #f  "shm_open"
            #:return-type ffi:int
            #:arg-types `(* ,ffi:int ,ffi:int))))
    (lambda (name oflags mode)
      (% (ffi:string->pointer name) oflags mode))))

(define shm-unlink
  (let ((% (foreign-library-function
            #f  "shm_unlink"
            #:return-type ffi:int
            #:arg-types `(*))))
    (lambda (name)
      (% (ffi:string->pointer name)))))
