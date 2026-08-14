;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Ayan Das <bvits@riseup.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (myguix packages sops)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses)
                #:prefix license:))

(define (sops-source version suffix checksum)
  "Return the origin for SOPS binary for given VERSION, architecture SUFFIX and CHECKSUM."
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/getsops/sops/releases/"
                        "download/v"
                        version
                        "/sops-v"
                        version
                        ".linux."
                        suffix))
    (sha256 (base32 checksum))))

(define-public sops
  (package
    (name "sops")
    (version "3.13.3")
    (source
     #f)
    (native-inputs (cond
                     ((target-aarch64?)
                      `(("binary-source" ,(sops-source version "arm64"
                                           "0xsdp4w79prxwy0izn8qw3ycxfcicsah1hfncqmb3wcfsfnapc2k"))))
                     ((target-x86-64?)
                      `(("binary-source" ,(sops-source version "amd64"
                                           "0szggzhzs98gwj0a0qplmxidkan1k3kg6l0mhwfyjfl7d8sc7gp5"))))
                     (else '())))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((source #+(this-package-native-input "binary-source"))
                 (dest-dir (string-append #$output "/bin"))
                 (dest-file (string-append dest-dir "/sops")))
            (mkdir-p dest-dir)
            (copy-file source dest-file)
            (chmod dest-file #o555)))))
    (synopsis "Simple and flexible tool for managing secrets")
    (description
     "SOPS (Secrets OPerationS) is an editor of encrypted files that supports 
YAML, JSON, ENV, INI and BINARY formats and encrypts with AWS KMS, GCP KMS, 
Azure Key Vault, age, and PGP.  It's particularly useful for storing secrets
in version control systems.")
    (home-page "https://github.com/getsops/sops")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (license license:mpl2.0)))
