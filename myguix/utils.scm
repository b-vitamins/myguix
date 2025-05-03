;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (myguix utils)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix colors)
  #:use-module (guix profiles)
  #:use-module (myguix packages nvidia)
  #:export (package-input-grafting with-transformation
                                   specifications->packages+gpu-graft
                                   specifications->manifest+gpu-graft
                                   apply-to-type-of-record
                                   nvidia-operating-system))

(define-public (to32 package64)
  "Build package for i686-linux.
Only x86_64-linux and i686-linux are supported.
- If i686-linux, return the package unchanged.
- If x86_64-linux, return the 32-bit version of the package."
  (match (%current-system)
    ("x86_64-linux" (package
                      (inherit package64)
                      (arguments
                       `(#:system "i686-linux"
                         ,@(package-arguments package64)))))
    (_ package64)))

(define (package-input-grafting replacements)
  "Return a procedure that, when passed a package, grafts its direct and
indirect dependencies, including implicit inputs, according to REPLACEMENTS.
REPLACEMENTS is a list of package pairs; the first element of each pair is the
package to replace, and the second one is the replacement.

Name and version of replacement packages will be padded to meet graft
requirement."
  (package-input-rewriting (map (match-lambda
                                  ((old . new) `(,old unquote
                                                 (package
                                                   (inherit old)
                                                   (replacement (package
                                                                  (inherit new)
                                                                  (name (string-pad-right
                                                                         (package-name
                                                                          new)
                                                                         (string-length
                                                                          (package-name
                                                                           old))
                                                                         #\0))
                                                                  (version (string-pad-right
                                                                            (package-version
                                                                             new)
                                                                            (string-length
                                                                             (package-version
                                                                              old))
                                                                            #\0))))))))
                                replacements)))

;; For concerns and direction of improvement, see this thread:
;; https://lists.gnu.org/archive/html/guix-devel/2024-06/msg00275.html
(define* (with-transformation proc obj
                              #:optional (pred package?))
  "Recursing into child elements, apply PROC to every element of OBJ that
matches PRED."
  (match obj
    ((? pred)
     (proc obj))
    ((? procedure?)
     (lambda args
       (apply values
              (map (cut with-transformation proc <> pred)
                   (call-with-values (lambda ()
                                       (apply obj args)) list)))))
    ((a . b) (cons (with-transformation proc a pred)
                   (with-transformation proc b pred)))
    ((_ ...)
     (map (cut with-transformation proc <> pred) obj))
    (#(_ ...) (vector-map (lambda (vec elt)
                            (with-transformation proc elt pred)) obj))
    ;; `<service-type>' and `<origin>' record types are expected to not be
    ;; modified. Altering them causes very difficult to debug run-time errors.
    ((or (? service-type?)
         (? origin?))
     obj)
    ((? record?)
     (let* ((record-type (record-type-descriptor obj))
            (record-fields (record-type-fields record-type)))
       (apply (record-constructor record-type)
              (map (lambda (field)
                     (let* ((accessor (record-accessor record-type field))
                            (obj (accessor obj)))
                       (with-transformation proc obj pred))) record-fields))))
    (_ obj)))

(define (specifications->nvidia-packages lst)
  "Given SPECS, a list of specifications such as \"emacs@25.2\" or
\"guile:debug\", return a list of packages with all the `mesa' entries replaced
by `nvda'."
  (define (map-caar proc lst)
    "Like `map', but the procedure is applied to the car of each element."
    (map (lambda (element)
           (cons (proc (car element))
                 (cdr element))) lst))
  (begin
    (display (highlight/warn
              "NVIDIA system detected, MESA->NVDA will be grafted.
"))
    (map-caar replace-mesa
              (specifications->packages lst))))

(define (specifications->nvidia-manifest lst)
  "Given SPECS, a list of specifications such as \"emacs@25.2\" or
\"guile:debug\", return a profile manifest with all the `mesa' entries replaced
by `nvda'."
  (packages->manifest (specifications->nvidia-packages lst)))

;; This variables will hold the procedures that gives the correct grafted
;; manifest / package list for the current system GPU drivers. In case of NVIDIA
;; it will graft the `nvidia-driver' over the `mesa' drivers.
(define specifications->packages+gpu-graft
  (if (zero? (system "nvidia-smi &>/dev/null"))
      specifications->nvidia-packages specifications->packages))

(define specifications->manifest+gpu-graft
  (if (zero? (system "nvidia-smi &>/dev/null"))
      specifications->nvidia-manifest specifications->manifest))

(define (apply-to-type-of-record fn type record)
  "Recursing into child fields, apply FN to every field of RECORD which holds a
value of TYPE."
  (let ((record-type (record-type-descriptor record)))
    (apply (record-constructor record-type)
           (map (lambda (field)
                  (let* ((field-predicate (record-predicate type))
                         (field-accessor (record-accessor record-type field))
                         (field-val (field-accessor record)))
                    (cond
                      ((field-predicate field-val)
                       (fn field-val))
                      ((list? field-val)
                       (map (lambda (pkg)
                              (if (package? pkg)
                                  (fn pkg) pkg)) field-val))
                      ((record? field-val)
                       (apply-to-type-of-record fn type field-val))
                      (else field-val))))
                (record-type-fields record-type)))))

(define-syntax-rule (nvidia-operating-system exp ...)
  "Like 'operating-system' but graft mesa with the proprietary NVIDIA driver."
  (apply-to-type-of-record replace-mesa
                           (@ (guix packages) <package>)
                           (operating-system
                             exp ...)))

