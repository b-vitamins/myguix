;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (myguix packages guile)
  #:use-module (guix packages)
  #:use-module ((gnu packages guile-xyz)
                #:select (guile-ares-rs guile-fibers-1.4)
                #:prefix upstream:))

(define-public guile-ares-rs
  (package
    (inherit upstream:guile-ares-rs)
    ;; Match Shepherd's Fibers version in system profiles.
    (propagated-inputs
     (modify-inputs (package-propagated-inputs upstream:guile-ares-rs)
       (replace "guile-fibers" upstream:guile-fibers-1.4)))))
