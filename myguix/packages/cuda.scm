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
  #:use-module (myguix packages nvidia)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

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
