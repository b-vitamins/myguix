;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (myguix packages compression)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages)
  #:use-module (myguix licenses)
  #:use-module (srfi srfi-26))

(define-public unrar
  (package
    (name "unrar")
    (version "7.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.rarlab.com/rar/unrarsrc-" version
                           ".tar.gz"))
       (sha256
        (base32 "09l336li4q7yrpjq22q6da2vrynpqbyb4a9fdxa02k65wkwi6p2h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;No tests.
       #:make-flags (list (string-append "DESTDIR="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://www.rarlab.com/rar_add.htm")
    (synopsis "Extract files from RAR archives")
    (description "The RAR decompression program.  It is nonfree (as in
freedom), but open-source.")
    (license (nonfree "https://www.win-rar.com/gtb_priv.html?&L=0"))))

(define-public miniz-for-pytorch
  (package
    (inherit miniz)
    (version "pytorch-2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/richgel999/miniz")
             (commit "3.0.2")))
       (file-name (git-file-name (package-name miniz) version))
       (sha256
        (base32 "0672q35vjrpakmsr1gwj9k5fwv5ihzhahm19bq4y74wqpn91p7fw"))
       (patches (search-myguix-patches "miniz-for-pytorch-2.7.0.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments miniz)
       ((#:configure-flags flags
         '())
        ;; The changes break the examples.
        `(cons "-DBUILD_EXAMPLES=OFF"
               ,flags))))
    (properties '((hidden? . #t)))))
