(define-module (myguix packages maths)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license-gnu:)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module (myguix packages nvidia)
  #:use-module ((gnu packages bootstrap)
                #:select (glibc-dynamic-linker))
  #:use-module (srfi srfi-26))


;;;
;;; MAGMA.
;;;

(define-public magma-cuda
  (package
    (name "magma")
    (version "2.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/icl-utk-edu/magma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ad657bhlmqynk17hqwwz9qmw41ydgc48s214l88zr1k8vmq35fi"))))
    (supported-systems '("x86_64-linux"))
    (build-system cmake-build-system)
    (native-inputs (list patchelf-0.16 which python perl pkg-config))
    (inputs (list gcc cuda-toolkit-12.4 openblas))
    (propagated-inputs (list nvidia-driver))
    (arguments
     (list
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
                   (add-before 'configure 'fix-shebang
                     (lambda* (#:key inputs
                               (source-directory ".") #:allow-other-keys)
                       (substitute* (string-append source-directory
                                                   "/tools/codegen.py")
                         (("/usr/bin/env")
                          (which "env"))
                         (("python")
                          (which "python3"))
                         (("perl")
                          (which "perl")))))
                   (add-before 'configure 'copy-source
                     (lambda* (#:key (source ".") outputs #:allow-other-keys)
                       (copy-recursively source
                                         (string-append (assoc-ref outputs
                                                                   "out")
                                                        "/source"))))
                   (replace 'configure
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let* ((openblas-dir (assoc-ref inputs "openblas"))
                              (cuda-dir (assoc-ref inputs "cuda-toolkit"))
                              (gcc (string-append (assoc-ref inputs "gcc")
                                                  "/bin/gcc"))
                              (g++ (string-append (assoc-ref inputs "gcc")
                                                  "/bin/g++"))
                              (nvcc (string-append cuda-dir "/bin/nvcc")))
                         (call-with-output-file "make.inc"
                           (lambda (port)
                             (format port "BACKEND = cuda\n")
                             (format port "OPENBLASDIR = ~a\n" openblas-dir)
                             (format port "CUDADIR = ~a\n" cuda-dir)
                             (format port "CC = ~a\n" gcc)
                             (format port "CXX = ~a\n" g++)
                             (format port "FORT = false\n")
                             (format port "NVCC = ~a\n" nvcc)
                             (format port "DEVCC = ~a\n\n" nvcc)
                             (format port "ARCH = ar\n")
                             (format port "ARCHFLAGS = cr\n")
                             (format port "RANLIB = ranlib\n\n")
                             (format port
                                     "GPU_TARGET = Volta Turing Ampere\n\n")
                             (format port "FPIC = -fPIC\n")
                             (format port
                              "CFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -fopenmp -std=c99
")
                             (format port
                              "CXXFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -fopenmp -std=c++11
")
                             (format port
                              "FFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -Wno-unused-dummy-argument
")
                             (format port "LDFLAGS = $(FPIC) -fopenmp\n")
                             (format port "DEVCCFLAGS = -O3 -DNDEBUG \n\n")
                             (format port
                              "LIB = -lopenblas -lcublas -lcusparse -lcudart
")
                             (format port "LIBDIR = -L~a/lib -L~a/lib64\n"
                                     openblas-dir cuda-dir)
                             (format port "INC = -I~a/include\n" cuda-dir))))
                       (invoke "make" "generate")))
                   (replace 'build
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((cuda-dir (assoc-ref inputs "cuda-toolkit")))
                         (invoke "cmake"
                                 "-DMAGMA_ENABLE_CUDA=ON"
                                 (string-append
                                  "-DCMAKE_INSTALL_PREFIX=build/target")
                                 "-DGPU_TARGET=sm_80"
                                 "-DBLA_VENDOR=OpenBLAS"
                                 "-DBUILD_SHARED_LIBS=ON"
                                 "-DUSE_FORTRAN=OFF"
                                 "."
                                 "-Bbuild")
                         (invoke "cmake"
                                 "--build"
                                 "build"
                                 "-j"
                                 (number->string (parallel-job-count))
                                 "--target"
                                 "install"))))
                   (replace 'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (use-modules (guix build utils))
                       (let ((out (assoc-ref outputs "out")))
                         (copy-recursively "build/target/include"
                                           (string-append out "/include"))
                         (install-file "build/target/lib/libmagma.so"
                                       (string-append out "/lib"))
                         (install-file "build/target/lib/libmagma_sparse.so"
                                       (string-append out "/lib")))))
                   (add-after 'install 'create-pkgconfig
                     (lambda* (#:key outputs inputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (cuda-dir (assoc-ref inputs "cuda-toolkit"))
                              (pc-dir (string-append out "/lib/pkgconfig"))
                              (pc-file (string-append pc-dir "/magma.pc"))
                              (cflags (string-join (list "Cflags:"
                                                    "-I${includedir}"
                                                    "-std=c++11"
                                                    "-fopenmp"
                                                    "-Wall -Wno-unused-function"
                                                    (string-append "-I"
                                                                   cuda-dir
                                                                   "/include")
                                                    (string-append "-I" out
                                                                   "/include")
                                                    (string-append "-I" out
                                                     "/source/control")
                                                    (string-append "-I" out
                                                     "/source/magmablas")
                                                    (string-append "-I" out
                                                     "/source/sparse/include")
                                                    (string-append "-I" out
                                                     "/source/sparse/control")
                                                    (string-append "-I" out
                                                     "/source/testing\n")) " "))
                              (libs (string-join (list "Libs:"
                                                       "-L{libdir}"
                                                       "-lmagma_sparse"
                                                       "-lmagma"
                                                       "-llapack"
                                                       "-lblas"
                                                       (string-append "-L"
                                                        cuda-dir "/lib")
                                                       "-lcudart"
                                                       "-lcublas"
                                                       "-lcusparse") " ")))
                         (mkdir-p pc-dir)
                         (call-with-output-file pc-file
                           (lambda (port)
                             (format port "prefix=~a\n" out)
                             (format port "exec_prefix=${prefix}\n")
                             (format port "libdir=${exec_prefix}/lib\n")
                             (format port "includedir=${prefix}/include\n\n")
                             (format port "Name: magma\n")
                             (format port
                              "Description: Matrix Algebra on GPU and Multicore Architectures
")
                             (format port
                                     (string-append "Version: "
                                                    #$(package-version
                                                       this-package) "\n"))
                             (format port cflags)
                             (format port libs))))))
                   (add-after 'create-pkgconfig 'set-rpath
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let* ((libdir (string-append (assoc-ref outputs "out")
                                                     "/lib"))
                              (libmagma-so (string-append libdir
                                                          "/libmagma.so"))
                              (libmagma-sparse-so (string-append libdir
                                                   "/libmagma_sparse.so")))
                         (define libmagma-runpaths
                           (string-join (list (string-append (assoc-ref inputs
                                                              "libc") "/lib")
                                              (string-append (assoc-ref inputs
                                                              "gcc") "/lib")
                                              (string-append (assoc-ref inputs
                                                              "cuda-toolkit")
                                                             "/lib")
                                              (string-append (assoc-ref inputs
                                                              "cuda-toolkit")
                                                             "/lib64")) ":"))
                         (define libmagma-sparse-runpaths
                           (string-join (list libmagma-runpaths libdir) ":"))
                         (make-file-writable libmagma-so)
                         (format #t "Setting RPATH on '~a'...~%" libmagma-so)
                         (invoke "patchelf" "--set-rpath" libmagma-runpaths
                                 "--force-rpath" libmagma-so)
                         (make-file-writable libmagma-sparse-so)
                         (format #t "Setting RPATH on '~a'...~%"
                                 libmagma-sparse-so)
                         (invoke "patchelf" "--set-rpath"
                                 libmagma-sparse-runpaths "--force-rpath"
                                 libmagma-sparse-so))))
                   (add-after 'set-rpath 'add-copyright
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (copyright (string-append out
                                                        "/source/COPYRIGHT")))
                         (install-file copyright
                                       (string-append out "/share")))))
                   (delete 'check))))
    (synopsis "MAGMA linear algebra library with CUDA support")
    (description
     "MAGMA is a high-performance linear algebra library that uses GPUs for
optimized calculations. It supports the CUDA backend and is compatible with multiple
GPU architectures.")
    (home-page "https://bitbucket.org/icl/magma/")
    (license license-gnu:bsd-3)))


;;;
;;; oneAPI MKL.
;;;

;; Work In Progress
;; References:
;; https://gitlab.archlinux.org/archlinux/packaging/packages/intel-oneapi-mkl
;; https://gitlab.archlinux.org/archlinux/packaging/packages/intel-oneapi-basekit
;; https://gitlab.archlinux.org/archlinux/packaging/packages/magma

(define (intel-url subpackage version superversion debversion suffix)
  (string-append "https://apt.repos.intel.com/oneapi/pool/main/intel-oneapi-"
                 subpackage
                 "-"
                 version
                 "-"
                 superversion
                 "-"
                 debversion
                 "_"
                 suffix
                 ".deb"))

(define (make-intel-oneapi subpackage
                           version
                           superversion
                           debversion
                           suffix
                           hash)
  (package
    (name (string-append "intel-oneapi-" subpackage))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (intel-url subpackage version superversion debversion suffix))
       (sha256
        hash)))
    (native-inputs (list tar gzip cpio))
    (build-system gnu-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'check)
                   (delete 'build)
                   (add-before 'install 'extract-deb
                     (lambda _
                       (for-each (lambda (deb)
                                   (format #t "extracting ~a...~%" deb)
                                   (let* ((command (string-append "ar x "
                                                    deb
                                                    " && tar xf data.tar.xz && rm data.tar.xz"
                                                    " && rm control.tar.xz && rm -rf debian-binary"
                                                    " && rm "
                                                    deb))
                                          (status (system command)))
                                     (unless (zero? status)
                                       (error (format #f
                                               "command '~a' failed with ~a"
                                               command status)))))
                                 (find-files "." "\\.deb$"))))
                   (replace 'install
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (define out
                         (assoc-ref outputs "out"))
                       (define source-root
                         (string-append "opt/intel/oneapi/mkl/"
                                        #$(package-version this-package)))
                       (when (directory-exists? source-root)
                         (copy-recursively source-root out)
                         (delete-file-recursively source-root))
                       (copy-recursively "." out))))))
    (home-page "https://software.intel.com/en-us/mkl")
    (synopsis "Intel oneAPI Math Kernel Library")
    (description
     "Intel® Math Kernel Library (MKL) is a proprietary library of
highly optimized, extensively threaded routines for applications that
require maximum performance.")
    (license (license:nonfree (string-append
                               "https://www.intel.com/content/www/us/en/developer/articles/license/"
                               "end-user-license-agreement.html")))))

(define-public intel-oneapi-mkl-classic
  (make-intel-oneapi "mkl-classic"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "07cmn9x1frfql0blghmqhbx28pj4aw7v4022hhcrhj584kdcq5pl")))

(define-public intel-oneapi-mkl-classic-include
  (make-intel-oneapi "mkl-classic-include"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "090sjzkvicxinj26rpp75zcpf3vn3rsbmz4xp0rv7njfi5441lja")))

(define-public intel-oneapi-mkl-classic-include-common
  (make-intel-oneapi "mkl-classic-include-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "08g40zab9wdwaqmjsdafqgqp78l8jg7zbfshavlp50fxl7cw05kh")))

(define-public intel-oneapi-mkl-core
  (make-intel-oneapi "mkl-core"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "13xhp025x956zfwq5c25lga04wddjydyvl0ymngq1d6f4iz21i1f")))

(define intel-oneapi-mkl-core-common
  (make-intel-oneapi "mkl-core-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "0w7hflyppn9pv825bjf902p0g1v4s60bc50lmxj15hcsiwd0cnla")))

(define intel-oneapi-mkl-core-devel
  (make-intel-oneapi "mkl-core-devel"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1z4sxw28nkjwfqiqmhfmxn6lz44diy94r0rcgdr3iykw1gyc1qrv")))

(define intel-oneapi-mkl-core-devel-common
  (make-intel-oneapi "mkl-core-devel-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "194xabclwhl19zvn9nlxdx518xx9gwm5pn1hlm7s1x4wi3g93598")))

(define intel-oneapi-mkl-cluster
  (make-intel-oneapi "mkl-cluster"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "0lzzhf91w4qjgkv1gvxlw7hdn9pakij9h4688ndvz6jws3wvrfjw")))

(define intel-oneapi-mkl-cluster-devel
  (make-intel-oneapi "mkl-cluster-devel"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "0fc19c1q73gf0sxcl0rb6i44jnllkfq5ivn3v9jp351aamjqdl3f")))

(define intel-oneapi-mkl-cluster-devel-common
  (make-intel-oneapi "mkl-cluster-devel-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "0n00k4iiayshj5rjmh8azzrjbnj2k9jz151sxp5m4j63scrv8386")))

(define intel-oneapi-mkl-sycl
  (make-intel-oneapi "mkl-sycl"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "18yrgksqm1p7bg148wn5mm0vxh5qvivnqd25mbvywbfpw78b6kiz")))

(define intel-oneapi-mkl-sycl-blas
  (make-intel-oneapi "mkl-sycl-blas"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "11dalggxrg7wglcf88n1sfkcmz2r5d2fqd88j5x9mghxdcpv9nkc")))

(define intel-oneapi-mkl-sycl-lapack
  (make-intel-oneapi "mkl-sycl-lapack"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1hgbr3n90x3f3a2xgwhicyllcg3z2623kswhscgfys4ln5wzj6hz")))

(define intel-oneapi-mkl-sycl-dft
  (make-intel-oneapi "mkl-sycl-dft"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1zsq2kywpaqw933xxmqicgp03bxwkhph21brljpc96rrlax106sz")))

(define intel-oneapi-mkl-sycl-sparse
  (make-intel-oneapi "mkl-sycl-sparse"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1mvj95z4k72ipsj1wj9bp439q3lgqbwwg64clcggrca74b8bjyvx")))

(define intel-oneapi-mkl-sycl-data-fitting
  (make-intel-oneapi "mkl-sycl-data-fitting"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1ci8wdbccd0bwdnd0z7gmgyxh0hibv1ig3alwmhh9pkq2rv33ny5")))
