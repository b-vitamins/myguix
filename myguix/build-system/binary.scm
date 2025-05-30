;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Josselin Poiret <dev@jpoiret.xyz>

(define-module (myguix build-system binary)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (myguix utils)
  #:export (%binary-build-system-modules default-patchelf default-glibc lower
                                         binary-build binary-build-system))

;; Commentary:
;;
;; Standard build procedure for binary packages.  This is implemented as an
;; extension of `copy-build-system'.
;;
;; Code:

(define %binary-build-system-modules
  ;; Build-side modules imported by default.
  `((myguix build binary-build-system)
    (myguix build utils)
    ,@%copy-build-system-modules))

(define (default-patchelf)
  "Return the default patchelf package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages elf))))
    ;; Use the older 0.16 version due to an upstream bug which can segfault
    ;; some binaries.  See <https://github.com/NixOS/patchelf/issues/482>.
    ;; TODO: Set back to patchelf when the package has been updated (or
    ;; patched) to fix this issue.
    (module-ref module
                'patchelf-0.16)))

(define (default-glibc)
  "Return the default glibc package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module
                'glibc)))

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

  (and (not target) ;XXX: no cross-compilation
       (bag (name name)
            (system system)
            (host-inputs `(,@(if source
                                 `(("source" ,source))
                                 '()) ,@inputs
                           ;; Keep the standard inputs of 'gnu-build-system'.
                           ,@(standard-packages)))
            (build-inputs `(("patchelf" ,patchelf)
                            ,@native-inputs
                            ;; If current system is i686, the *32 packages will be the
                            ;; same as the non-32, but that's OK.
                            ("libc32" ,(to32 glibc))))
            (outputs outputs)
            (build binary-build)
            (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (binary-build name
                       inputs
                       #:key guile
                       source
                       (outputs '("out"))
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
                       (phases '(@ (myguix build binary-build-system)
                                   %standard-phases))
                       (system (%current-system))
                       (imported-modules %binary-build-system-modules)
                       (modules '((myguix build binary-build-system)
                                  (guix build utils)
                                  (myguix build utils)))
                       (substitutable? #t)
                       allowed-references
                       disallowed-references)
  "Build SOURCE using PATCHELF, and with INPUTS. This assumes that SOURCE
provides its own binaries."
  (define builder
    (with-imported-modules imported-modules
                           #~(begin
                               (use-modules #$@modules)

                               #$(with-build-variables inputs outputs
                                                       #~(binary-build
                                                          #:source #+source
                                                          #:system #$system
                                                          #:outputs %outputs
                                                          #:inputs
                                                          %build-inputs
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

(define binary-build-system
  (build-system (name 'binary)
                (description "The standard binary build system")
                (lower lower)))

;;; binary.scm ends here
