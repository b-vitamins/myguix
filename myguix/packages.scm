;;; myguix --- Personal Guix channel
;;; Copyright © 2025 Ayan Das <bvits@riseup.net>

(define-module (myguix packages)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:export (search-myguix-patch search-myguix-patches %myguix-patch-path))

;;; Commentary:
;;;
;;; Utilities for packages in the myguix channel.
;;;
;;; Code:

(define %myguix-root-directory
  ;; Root directory of the myguix channel.
  (dirname (dirname (search-path %load-path "myguix/packages.scm"))))

(define %myguix-patch-path
  ;; Path for myguix-specific patches.
  (make-parameter (list (string-append %myguix-root-directory
                                       "/myguix/packages/patches"))))

(define (search-myguix-patch file-name)
  "Search for patch FILE-NAME in myguix's patch directories.
Raise an error if not found."
  (or (search-path (%myguix-patch-path) file-name)
      (raise (formatted-message (G_ "~a: myguix patch not found") file-name))))

(define-syntax-rule (search-myguix-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %MYGUIX-PATCH-PATH."
  (list (search-myguix-patch file-name) ...))

;;; Re-export commonly used bindings from (gnu packages) for convenience.
(re-export search-patches ;For GNU Guix patches
           specification->package specification->package+output)
