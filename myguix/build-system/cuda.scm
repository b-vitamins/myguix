;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>

(define-module (myguix build-system cuda)
  #:use-module (gnu packages gcc)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (myguix build-system binary)
  #:use-module (myguix utils)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:export (cuda-license cuda-current-system
                         cuda-module-url
                         guix-system->cuda-system

                         %cuda-build-system-modules
                         lower
                         cuda-build
                         cuda-build-system))

;; Commentary:
;;
;; Standard build procedure for Cuda binary packages.  This is
;; implemented as an extension of `binary-build-system'.
;;
;; Code:

(define %cuda-build-system-modules
  ;; Build-side modules imported by default.
  `((myguix build cuda-build-system)
    (myguix build utils)
    ,@%binary-build-system-modules))

(define (build-patchelf-plan wrapper-plan inputs)
  #~(let ((patchelf-inputs (list #$@(map car inputs))))
      (map (lambda (file)
             (cons file
                   (cons* "out" patchelf-inputs)))
           #$wrapper-plan)))

(define (cuda-license name)
  (license:nonfree (format #f
                    "https://developer.download.nvidia.com/compute/cuda/redist/~a/LICENSE.txt"
                    (string-join (string-split name #\-) "_"))))

(define (guix-system->cuda-system system)
  (match system
    ("x86_64-linux" "linux-x86_64")
    ("aarch64-linux" "linux-aarch64")
    ("powerpc64le-linux" "linux-ppc64le")
    (_ #f)))

(define (cuda-current-system)
  (guix-system->cuda-system (or (%current-target-system)
                                (%current-system))))

(define (cuda-module-url name version)
  (let ((system (cuda-current-system))
        (snake-name (string-join (string-split name #\-) "_")))
    (format #f
     "https://developer.download.nvidia.com/compute/cuda/redist/~a/~a/~a-~a-~a-archive.tar.xz"
     snake-name
     system
     snake-name
     system
     version)))

(define* (lower name
                #:key source
                inputs
                native-inputs
                outputs
                system
                target
                (patchelf (default-patchelf))
                (glibc (default-glibc))
                #:allow-other-keys #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:patchelf #:inputs #:native-inputs))
  (define host-inputs
    `(,@(if source
            `(("source" ,source))
            '())

      ("gcc:lib" ,gcc "lib")
      ("glibc" ,glibc)

      ,@inputs
      ;; Keep the standard inputs of 'gnu-build-system'.
      ,@(standard-packages)))

  (and (not target) ;XXX: no cross-compilation
       (bag (name name)
            (system system)
            (host-inputs host-inputs)
            (build-inputs `(("patchelf" ,patchelf)
                            ,@native-inputs
                            ;; If current system is i686, the *32 packages will be the
                            ;; same as the non-32, but that's OK.
                            ("libc32" ,(to32 glibc))))
            (outputs outputs)
            (build cuda-build)
            (arguments (append (strip-keyword-arguments private-keywords
                                                        arguments)
                               (list #:wrap-inputs (alist-delete "source"
                                                                 host-inputs)))))))

(define* (cuda-build name
                     inputs
                     #:key guile
                     source
                     wrap-inputs
                     (outputs '("out"))
                     (patchelf-inputs ''("gcc" "glibc"))
                     (patchelf-plan ''())
                     (install-plan ''(("." "./")))
                     (search-paths '())
                     (out-of-source? #t)
                     (validate-runpath? #t)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec" "bin"
                                           "sbin"))
                     (phases '(@ (myguix build cuda-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (imported-modules %cuda-build-system-modules)
                     (modules '((myguix build cuda-build-system)
                                (guix build utils)
                                (myguix build utils)))
                     (substitutable? #t)
                     allowed-references
                     disallowed-references)
  "Build SOURCE using binary-build-system."
  (define builder
    (with-imported-modules imported-modules
                           #~(begin
                               (use-modules #$@modules)

                               #$(with-build-variables inputs outputs
                                                       #~(cuda-build #:source #+source
                                                          #:system #$system
                                                          #:outputs %outputs
                                                          #:inputs
                                                          %build-inputs
                                                          #:patchelf-inputs #$patchelf-inputs
                                                          #:patchelf-plan #$patchelf-plan
                                                          #:install-plan #$install-plan
                                                          #:search-paths '#$(map
                                                                             search-path-specification->sexp
                                                                             search-paths)
                                                          #:phases #$phases
                                                          #:out-of-source? #$out-of-source?
                                                          #:validate-runpath? #$validate-runpath?
                                                          #:patch-shebangs? #$patch-shebangs?
                                                          #:strip-binaries? #$strip-binaries?
                                                          #:strip-flags #$strip-flags
                                                          #:strip-directories #$strip-directories)))))

  (mlet %store-monad
        ((guile (package->derivation (or guile
                                         (default-guile)) system
                                     #:graft? #f)))
        (gexp->derivation name
                          builder
                          #:system system
                          #:target #f
                          #:substitutable? substitutable?
                          #:allowed-references allowed-references
                          #:disallowed-references disallowed-references
                          #:guile-for-build guile)))

(define cuda-build-system
  (build-system (name 'cuda)
                (description "The Cuda build system")
                (lower lower)))

;;; cuda.scm ends here
