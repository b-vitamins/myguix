;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (myguix tests guile)
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages guile-xyz)
                #:select (guile-ares-rs)
                #:prefix upstream:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (myguix packages base)
  #:use-module (myguix packages guile)
  #:use-module (srfi srfi-64)
  #:export (run-tests))

(define (fibers-input package)
  (cadr (assoc "guile-fibers" (package-propagated-inputs package))))

(define (profile-compatible? packages)
  (with-store store
    (run-with-store store
      (check-for-collisions (packages->manifest packages) (%current-system)))))

(define (run-tests)
  (test-begin "myguix-guile")

  (test-eq "Ares shares system Shepherd's Fibers package"
           (fibers-input shepherd-1.0)
           (fibers-input guile-ares-rs))
  (test-eq "Ares source is unchanged"
           (package-source upstream:guile-ares-rs)
           (package-source guile-ares-rs))
  (test-assert "Guile bundle selects the local Ares package"
               (memq guile-ares-rs %guile-development))

  (unless (eq? (fibers-input upstream:guile-ares-rs)
               (fibers-input shepherd-1.0))
    (test-error "upstream Ares conflicts with Shepherd"
                profile-collision-error?
                (profile-compatible? (list shepherd-1.0 upstream:guile-ares-rs))))

  (test-assert "Guile bundle coexists with Shepherd 0.10"
               (profile-compatible? (cons shepherd %guile-development)))
  (test-assert "Guile bundle coexists with Shepherd 1.0"
               (profile-compatible? (cons shepherd-1.0 %guile-development)))

  (let ((failures (test-runner-fail-count (test-runner-current))))
    (test-end "myguix-guile")
    (zero? failures)))

(when (member "--run-tests" (command-line))
  (exit (if (run-tests) 0 1)))
