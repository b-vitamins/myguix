;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>

(define-module (myguix packages)
  #:use-module (gnu packages)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:replace (%patch-path search-patch)
  #:export (myguix-patches))

;;; Commentary:
;;;
;;; This module refines the default value of some parameters from (gnu
;;; packages) and the syntax/procedures using those.  This allows
;;; 'search-paths' and friends to work without any user intervention.
;;;
;;; Code:

(define %patch-path
  ;; Search patches relative to every load path entry, mirroring the ad hoc
  ;; patch lookup used in other myguix package modules.  Canonicalize entries
  ;; so callers such as 'local-file' never end up with paths relative to the
  ;; current working directory.
  (make-parameter (map (lambda (directory)
                         (string-append (if (absolute-file-name? directory)
                                            directory
                                            (canonicalize-path directory))
                                        "/myguix/patches"))
                       %load-path)))

;;; XXX: The following must be redefined to make use of the overridden
;;; %patch-path parameter above.
(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found") file-name))))

;;; XXX: `search-patches' being syntax, it can't be overridden by the module
;;; system, or so it seems, so we simply rename it.
(define-syntax-rule (myguix-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
