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
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((myguix licenses)
                #:prefix nonfree:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public cuda-toolkit
  (package
    (name "cuda-toolkit")
    (version "12.4.0")
    (source
     (origin
       (uri
        "https://developer.download.nvidia.com/compute/cuda/12.4.0/local_installers/cuda_12.4.0_550.54.14_linux.run")
       (sha256
        (base32 "05vxwn91hhrc57p8vr3xi5dbjiwdnwdnp2xnrmshajd9xks45a76"))
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
                   (add-after 'unpack 'remove-superfluous-stuff
                     (lambda _
                       (with-directory-excursion "builds"
                         (for-each delete-file-recursively
                                   '("nsight_compute" "nsight_systems"
                                     "cuda_gdb")))))
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
    (inputs `(("gcc:lib" ,gcc-13 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (license (nonfree:nonfree
              "https://developer.nvidia.com/nvidia-cuda-license"))))
