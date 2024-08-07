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
  #:use-module (myguix packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
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

(define-public cudnn-frontend-98ca4e
  (let ((version "1.5.2")
        (revision "0")
        (commit "98ca4e1941fe3263f128f74f10063a3ea35c7019"))
    (package
      (name "cudnn-frontend")
      (version version)
      (home-page "https://github.com/NVIDIA/cudnn-frontend")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04aglaxh4mgm94qwia293gqn7gmlw5w6mk8nky4k6l1m2615swyd"))
         ;; unit_tests requires Catch2::Catch2WithMani
         (patches (search-patches "cudnn-frontend-remove-all-unit-tests.patch"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; building samples requires Catch2::Catch2WithMain
        #:configure-flags #~(list "-DCUDNN_FRONTEND_BUILD_SAMPLES=OFF")))
      (inputs (list cudnn cuda-toolkit-12.1))
      (propagated-inputs (list catch2))
      (synopsis "Front end API for NVIDIA CUDA Deep Neural Network library")
      (description
       "The cuDNN FrontEnd(FE) API is a C++ header-only library that wraps the cuDNN C backend API. Both the FE and backend APIs are entry points to the same set of functionality that is commonly referred to as the 'graph API'.

While there are two entry points to the graph API (i.e. backend and frontend), it is expected that most users will use the FE API. Reasons being:

    FE API is less verbose without loss of control. All functionality accessible through the backend API is also accessible through the FE API.
    FE API adds functionality on top of the backend API, like errata filters and autotuning.

Also, for those using backend API, FE API source and samples can serve as reference implementation.

In FE v1.0 API, users can describe multiple operations that form subgraph through a persistent @code{cudnn_frontend::graph::Graph} object. Unlike the FE v0.x API, users don't need to worry about specifying shapes and sizes of the intermediate virtual tensors. FE v1.0 API extends the groundwork of earlier versions and introduces a new set of APIs to further simplify the workflow.

Additionally, FE v1.0 API provides python bindings to all API through pybind11. It is recommended that new users of cuDNN start with the frontend v1.0 API. See samples/cpp and samples/python for more details on its usage.")
      (license license:expat))))

(define-public cutlass-bbe579
  (let ((version "3.4.1")
        (revision "0")
        (commit "bbe579a9e3beb6ea6626d9227ec32d0dae119a49"))
    (package
      (name "cutlass")
      (version version)
      (home-page "https://github.com/NVIDIA/cutlass")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i8h7hfa7ixlhk58p7cyam6l7zzbsir6jm6zv3vfjc6cbp8bqlzk"))))
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
      (inputs (list cuda-toolkit-12.1))
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
      (license license:bsd-3))))
