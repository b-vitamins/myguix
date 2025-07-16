;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Mathieu Othacehe <m.othacehe@gmail.com>

(define-module (myguix ci)
  #:use-module (gnu ci)
  #:use-module (gnu system image)
  #:use-module (myguix system install)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)
  (define systems
    (arguments->systems arguments))

  (append-map (lambda (system)
                (list (image->job store
                                  (image-with-os iso9660-image
                                                 my-installation-os)
                                  #:name "nonfree-iso9660-image"
                                  #:system system))) systems))
