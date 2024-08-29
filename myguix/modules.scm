;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>

(define-module (myguix modules)
  #:use-module (ice-9 match)
  #:export (import-myguix-module?))

(define (myguix-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix or Myguix module."
  (match name
    (('guix _ ...)
     #t)
    (('gnu _ ...)
     #t)
    (('myguix _ ...)
     #t)
    (('nongnu _ ...)
     #t)
    (_ #f)))

;; Since we don't use deduplication support in 'populate-store', don't
;; import (guix store deduplication) and its dependencies, which
;; includes Guile-Gcrypt.
(define (import-myguix-module? module)
  "Return true if MODULE is not (guix store deduplication)"
  (and (myguix-module-name? module)
       (not (equal? module
                    '(guix store deduplication)))))
