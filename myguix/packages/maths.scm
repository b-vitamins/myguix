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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (myguix packages nvidia))

(define-public magma
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
    (license license:bsd-3)))
