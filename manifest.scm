;;; myguix --- Personal Guix channel
;;; Copyright © 2025 Ayan Das <bvits@riseup.net>

;; Development manifest for myguix channel.  To create development environment:
;;
;;     guix shell
;;
;; or
;;
;;     guix shell --pure -m manifest.scm

(use-modules (guix packages)
             (gnu packages))

(concatenate-manifests
 (list ;; Development dependencies for the channel itself
       (package->development-manifest
        (specification->package "guix"))

       ;; Extra packages for testing and development
       (specifications->manifest
        (list "git"
              "git:send-email"
              "gnupg"
              "nss-certs"
              "openssl"))

       ;; Tools for working with packages
       (specifications->manifest
        (list "diffoscope"      ; For comparing build outputs
              "patchelf"        ; For working with binaries
              "strace"))        ; For debugging

       ;; Editor support (adjust to your preference)
       (specifications->manifest
        (list "emacs"
              "emacs-geiser"
              "emacs-geiser-guile"
              "emacs-paredit"
              "emacs-magit"))))
