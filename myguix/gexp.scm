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

(define-module (myguix gexp)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (file-like->name slurp-file-like mixed-text-file*))

;;; Commentary:
;;;
;;; This module provides utility functions for working with G-expressions
;;; and file-like objects in GNU Guix.
;;;
;;; Code:

(define (file-like->name file)
  "Extract the name from a file-like object FILE."
  (match file
    ((? local-file? f)
     (local-file-name f))
    ((? plain-file? f)
     (plain-file-name f))
    ((? computed-file? f)
     (computed-file-name f))
    ((? program-file? f)
     (program-file-name f))
    (_ (error "Not a recognized file-like object" file))))

(define* (slurp-file-like file
                          #:key (encoding "UTF-8"))
  "Return a G-expression that reads FILE content as a string."
  (unless (file-like? file)
    (error "Not a file-like object" file))
  #~(begin
      (use-modules (ice-9 textual-ports))
      (call-with-input-file #$file
        get-string-all
        #:encoding #$encoding)))

(define (mixed-text-file* name . parts)
  "Like mixed-text-file but filters out empty strings and #f values."
  (apply mixed-text-file name
         (filter (lambda (x)
                   (and x
                        (or (not (string? x))
                            (not (string-null? x))))) parts)))

;;; gexp.scm ends here
