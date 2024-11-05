;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>

(define-module (myguix build cuda-build-system)
  #:use-module ((guix build gnu-build-system)
                #:prefix gnu:)
  #:use-module ((myguix build binary-build-system)
                #:prefix binary:)
  #:use-module (guix build utils)
  #:use-module (nonguix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (%standard-phases cuda-build))

;; Commentary:
;;
;; Builder-side code of the Cuda binary build procedure.
;;
;; Code:

;;; XXX: Copied from upstream guix in tests/store-deduplication.scm
(define (cartesian-product . lst)
  "Return the Cartesian product of all the given lists."
  (match lst
    ((head)
     (map list head))
    ((head . rest) (let ((others (apply cartesian-product rest)))
                     (apply append
                            (map (lambda (init)
                                   (map (lambda (lst)
                                          (cons init lst)) others)) head))))
    (() '())))

(define* (install-pkg-config-files #:key outputs #:allow-other-keys)
  (if (directory-exists? "pkg-config")
      (with-directory-excursion "pkg-config"
        (for-each (match-lambda
                    ((output file)
                     (substitute* file
                       (("^cudaroot=.*")
                        (string-append "cudaroot=" output "\n"))
                       (("^libdir=.*")
                        (string-append "libdir=" output "/lib\n"))
                       (("^includedir=.*")
                        (string-append "includedir=" output "/include\n")))
                     (install-file file
                                   (string-append output "/share/pkg-config"))
                     (with-directory-excursion (string-append output
                                                "/share/pkg-config")
                       (symlink (basename file)
                                (string-append (string-take file
                                                            (string-index file
                                                             #\-)) ".pc")))))
                  (cartesian-product (map cdr outputs)
                                     (find-files "." "\\.pc"))))
      (format #t "pkg-config directory doesn't exist, nothing to be done.~%")))

(define %standard-phases
  (modify-phases binary:%standard-phases
    (replace 'patchelf
      binary:autopatchelf)
    (add-after 'install 'install-static
      install-static-output)
    (add-after 'install-static 'install-pkg-config-files
      install-pkg-config-files)))

(define* (cuda-build #:key inputs
                     (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs
         #:phases phases
         args))

;;; cuda-build-system.scm ends here
