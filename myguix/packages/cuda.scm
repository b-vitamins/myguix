;;; Copyright © 2018, 2019, 2020, 2024 Inria
;;; Copyright © 2021-2024 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2024 Atte Torri <atte.torri@protonmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (myguix packages cuda)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((myguix licenses)
                #:prefix nonfree:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public cuda-toolkit-12.1
  (package
    (name "cuda-toolkit")
    (version "12.1.1")
    (source
     (origin
       (uri
        "https://developer.download.nvidia.com/compute/cuda/12.1.0/local_installers/cuda_12.1.0_530.30.02_linux.run")
       (sha256
        (base32 "0764a8zsnmlp1f9912s8nkx700h7zzidr697mnwssw9dq4v90sb8"))
       (method url-fetch)))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (outputs '("out"))
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (ice-9 ftw))
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
                   (replace 'unpack
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define ld.so
                         (search-input-file inputs
                                            #$(glibc-dynamic-linker)))
                       (let ((source (assoc-ref inputs "source")))
                         (invoke "sh" source "--keep" "--noexec")
                         (chdir "pkg"))))
                   (delete 'configure)
                   (delete 'check)
                   (replace 'build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define libc
                         (assoc-ref inputs "libc"))
                       (define gcc-lib
                         (assoc-ref inputs "gcc:lib"))
                       (define ld.so
                         (search-input-file inputs
                                            #$(glibc-dynamic-linker)))
                       (define rpath
                         (string-join (list "$ORIGIN"
                                            (string-append #$output "/lib")
                                            (string-append #$output
                                                           "/nvvm/lib64")
                                            (string-append libc "/lib")
                                            (string-append gcc-lib "/lib"))
                                      ":"))
                       (define (patch-elf file)
                         (make-file-writable file)
                         (format #t "Setting RPATH on '~a'...~%" file)
                         ;; RPATH should be modified before the interpreter. If
                         ;; done the other way around, it nukes the resulting
                         ;; binary.
                         (invoke "patchelf" "--set-rpath" rpath
                                 "--force-rpath" file)
                         (unless (string-contains file ".so")
                           (format #t "Setting interpreter on '~a'...~%" file)
                           (invoke "patchelf" "--set-interpreter" ld.so file)))
                       (for-each (lambda (file)
                                   (when (elf-file? file)
                                     (patch-elf file)))
                                 (find-files "."
                                             (lambda (file stat)
                                               (eq? 'regular
                                                    (stat:type stat)))))))
                   (replace 'install
                     (lambda _
                       (define (copy-from-directory directory)
                         (for-each (lambda (entry)
                                     (define sub-directory
                                       (string-append directory "/" entry))

                                     (define target
                                       (string-append #$output "/"
                                                      (basename entry)))

                                     (when (file-exists? sub-directory)
                                       (copy-recursively sub-directory target)))
                                   '("bin" "targets/x86_64-linux/lib"
                                     "targets/x86_64-linux/include" "nvvm/bin"
                                     "nvvm/include" "nvvm/lib64")))

                       (setenv "COLUMNS" "200")
                       (with-directory-excursion "builds"
                         (for-each copy-from-directory
                                   (scandir "."
                                            (match-lambda
                                              ((or "." "..")
                                               #f)
                                              (_ #t))))
                         (copy-recursively "cuda_nvcc/nvvm/libdevice"
                                           (string-append #$output
                                                          "/nvvm/libdevice")))))
                   (add-after 'install 'install-cupti
                     (lambda _
                       (copy-recursively "builds/cuda_cupti/extras/CUPTI"
                                         #$output)))
                   (add-after 'install 'delete-stray-symlinks
                     (lambda _
                       (delete-file (string-append #$output "/include/include")))))))
    (native-inputs (list which patchelf perl python-2))
    (inputs `(("gcc:lib" ,gcc-11 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (license (nonfree:nonfree
              "https://developer.nvidia.com/nvidia-cuda-license"))))

(define-public cuda-toolkit-12.4
  (package
    (inherit cuda-toolkit-12.1)
    (version "12.1.4")
    (source
     (origin
       (uri
        "https://developer.download.nvidia.com/compute/cuda/12.4.0/local_installers/cuda_12.4.0_550.54.14_linux.run")
       (sha256
        (base32 "05vxwn91hhrc57p8vr3xi5dbjiwdnwdnp2xnrmshajd9xks45a76"))
       (method url-fetch)))))

(define-public cudnn
  (package
    (name "cudnn")
    (version "9.2.1.18")
    (source
     (origin
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "16c34a0ymxhh3867pk53bwf81dicgci5cq5n723nc8isvnkxrqnn"))
       (method url-fetch)))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'check)
                   (replace 'build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define libc
                         (assoc-ref inputs "libc"))
                       (define gcc-lib
                         (assoc-ref inputs "gcc:lib"))
                       (define ld.so
                         (search-input-file inputs
                                            #$(glibc-dynamic-linker)))
                       (define rpath
                         (string-join (list "$ORIGIN"
                                            (string-append #$output "/lib")
                                            (string-append #$output
                                                           "/nvvm/lib64")
                                            (string-append libc "/lib")
                                            (string-append gcc-lib "/lib"))
                                      ":"))

                       (define (patch-elf file)
                         (make-file-writable file)
                         (unless (string-contains file ".so")
                           (format #t "Setting interpreter on '~a'...~%" file)
                           (invoke "patchelf" "--set-interpreter" ld.so file))
                         (format #t "Setting RPATH on '~a'...~%" file)
                         (invoke "patchelf" "--set-rpath" rpath
                                 "--force-rpath" file))

                       (for-each (lambda (file)
                                   (when (elf-file? file)
                                     (patch-elf file)))
                                 (find-files "."
                                             (lambda (file stat)
                                               (eq? 'regular
                                                    (stat:type stat)))))))
                   (replace 'install
                     (lambda _
                       (let ((lib (string-append #$output "/lib"))
                             (include (string-append #$output "/include")))
                         (mkdir-p #$output)
                         (copy-recursively "lib" lib)
                         (copy-recursively "include" include)))))))
    (native-inputs (list patchelf))
    (inputs `(("gcc:lib" ,gcc-13 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "NVIDIA CUDA Deep Neural Network library (cuDNN)")
    (description "This package provides the CUDA Deep Neural Network library.")
    (license (nonfree:nonfree
              "https://docs.nvidia.com/deeplearning/cudnn/sla/index.html"))))

(define-public cutlass
  (package
    (name "cutlass")
    (version "3.5.1")
    (home-page "https://github.com/NVIDIA/cutlass")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h1cvlvmm0mcvsij8382qdzzswy75zyaybgaxj84md73wqvrhcdi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list (string-append "-DGOOGLETEST_DIR="
                                               #$(package-source googletest))
                                "-DCUTLASS_ENABLE_EXAMPLES=NO"
                                "-DCUTLASS_NVCC_ARCHS=80"
                                "-DCUTLASS_LIBRARY_KERNELS=all"
                                "-DCUTLASS_ENABLE_TESTS=NO"
                                "-DCUTLASS_INSTALL_TESTS=NO")
      #:validate-runpath? #f))
    (native-inputs (list python))
    (inputs (list cuda-toolkit-12.4))
    (synopsis
     "CUDA C++ template abstractions for high-performance linear algebra")
    (description
     "CUTLASS is a collection of CUDA C++ template abstractions for implementing
high-performance matrix-matrix multiplication (GEMM) and related computations
at all levels and scales within CUDA.  It incorporates strategies for
hierarchical decomposition and data movement similar to those used to
implement cuBLAS and cuDNN.

CUTLASS decomposes these ``moving parts'' into reusable, modular software
components abstracted by C++ template classes.  Primitives for different
levels of a conceptual parallelization hierarchy can be specialized and tuned
via custom tiling sizes, data types, and other algorithmic policy.  The
resulting flexibility simplifies their use as building blocks within custom
kernels and applications.")
    (license license:bsd-3)))

(define-public nccl
  (package
    (name "nccl")
    (version "2.22.3-1")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/NVIDIA/nccl")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1kwh4950q953c2sr7ir2inyw34mwh5av7cq93j852yd2sqxyyk3v"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CUDA_HOME="
                                          #$(this-package-input "cuda-toolkit")))
      ;; No tests in source.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   ;; No configure script.
                   (delete 'configure)
                   (add-before 'install 'set-prefix
                     (lambda _
                       (setenv "PREFIX"
                               #$output))))))
    (native-inputs (list python which))
    (inputs (list cuda-toolkit-12.4))
    (home-page "https://developer.nvidia.com/nccl")
    (synopsis
     "Optimized primitives for collective multi-GPU communication between
NVIDIA GPUs")
    (description
     "NCCL (pronounced \"Nickel\") is a stand-alone library of standard
communication routines for NVIDIA GPUs, implementing all-reduce,
all-gather, reduce, broadcast, reduce-scatter, as well as any
send/receive based communication pattern. It has been optimized to
achieve high bandwidth on platforms using PCIe, NVLink, NVswitch, as
well as networking using InfiniBand Verbs or TCP/IP sockets. NCCL
supports an arbitrary number of GPUs installed in a single node or
across multiple nodes, and can be used in either single- or
multi-process (e.g., MPI) applications.")
    (license license:bsd-3)))

(define-public cutensor
  (package
    (name "cutensor")
    (version "2.0.1.2")
    (home-page "https://developer.nvidia.com/cutensor")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cutensor/redist/libcutensor/linux-x86_64/libcutensor-linux-x86_64-"
             version "-archive.tar.xz"))
       (sha256
        (base32 "18l6qmfjcn75jsyzlsj66mji8lgab2ih19d0drqavfi2lqna3vgd"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("LICENSE" "LICENSE"))))
    (synopsis "Nvidia cuTENSOR library")
    (description "This package provides the proprietary cuTENSOR
library for NVIDIA GPUs.")
    (license (nonfree:nonfree
              "https://docs.nvidia.com/cuda/cutensor/latest/license.html"))))

(define-public no-float128
  ;; FIXME: We cannot simply add it to 'propagated-inputs' of cuda-toolkit
  ;; because then it would come after glibc in CPLUS_INCLUDE_PATH.
  (package
    (name "no-float128")
    (version "0")
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder #~(begin
                    (use-modules (guix build utils))

                    (let* ((header "/include/bits/floatn.h")
                           (target (string-append #$output
                                                  (dirname header)))
                           (libc #$(this-package-input "libc")))
                      (mkdir-p target)
                      (install-file (string-append libc header) target)
                      (substitute* (string-append target "/"
                                                  (basename header))
                        (("#([[:blank:]]*)define __HAVE_FLOAT128[[:blank:]]+1"
                          _ space)
                         (string-append "#" space "define __HAVE_FLOAT128 0")))))))
    (inputs `(("libc" ,glibc)))
    (home-page "https://hpc.guix.info")
    (synopsis "@file{<bits/floatn.h>} header that disables float128 support")
    (description
     "This package provides a @file{<bits/floatn.h>} header to override that
of glibc and disable float128 support.  This is required allow the use of
@command{nvcc} with CUDA 8.0 and glibc 2.26+.  Otherwise, @command{nvcc} fails like this:

@example
/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(61): error: invalid argument to attribute \"__mode__\"

/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(73): error: identifier \"__float128\" is undefined
@end example

See also
@url{https://devtalk.nvidia.com/default/topic/1023776/cuda-programming-and-performance/-request-add-nvcc-compatibility-with-glibc-2-26/1}.")
    (license license:gpl3+)))
