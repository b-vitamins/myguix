;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (myguix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (64-bit? make-wrapper concatenate-files build-paths-from-inputs
                    install-static-output))

(define (64-bit? file)
  "Return true if ELF file is in 64-bit format, false otherwise.
See https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#File_header."
  (with-input-from-file file
    (lambda ()
      (= 2
         (array-ref (get-bytevector-n (current-input-port) 5) 4)))
    #:binary #t))

(define* (make-wrapper wrapper
                       real-file
                       #:key (skip-argument-0? #f)
                       #:rest vars)
  "Like `wrap-program' but create WRAPPER around REAL-FILE.
The wrapper automatically changes directory to that of REAL-FILE.

Example:

  (make-wrapper \"bin/foo\" \"sub-dir/original-foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will create 'bin/foo' with the following
contents:

  #!location/of/bin/bash
  export PATH=\"/gnu/.../bar/bin\"
  export CERT_PATH=\"$CERT_PATH${CERT_PATH:+:}/gnu/.../baz/certs:/qux/certs\"
  cd sub-dir
  exec -a $0 sub-dir/original-foo \"$@\"."
  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (match lst
      ((var sep
            '= rest)
       (format #f "export ~a=\"~a\"" var
               (string-join rest sep)))
      ((var sep
            'prefix rest)
       (format #f
               "export ~a=\"~a${~a:+~a}$~a\""
               var
               (string-join rest sep)
               var
               sep
               var))
      ((var sep
            'suffix rest)
       (format #f
               "export ~a=\"$~a${~a+~a}~a\""
               var
               var
               var
               sep
               (string-join rest sep)))
      ((var '= rest)
       (format #f "export ~a=\"~a\"" var
               (string-join rest ":")))
      ((var 'prefix rest)
       (format #f
               "export ~a=\"~a${~a:+:}$~a\""
               var
               (string-join rest ":")
               var
               var))
      ((var 'suffix rest)
       (format #f
               "export ~a=\"$~a${~a:+:}~a\""
               var
               var
               var
               (string-join rest ":")))))

  (define (remove-keyword-arguments lst)
    (match lst
      (() '())
      (((? keyword? _)
        _ lst ...)
       (remove-keyword-arguments lst))
      (_ lst)))

  (mkdir-p (dirname wrapper))
  (call-with-output-file wrapper
    (lambda (port)
      (format port
              (if skip-argument-0? "#!~a~%~a~%cd \"~a\"~%exec \"~a\" \"$@\"~%"
                  "#!~a~%~a~%cd \"~a\"~%exec -a \"$0\" \"~a\" \"$@\"~%")
              (which "bash")
              (string-join (map export-variable
                                (remove-keyword-arguments vars)) "\n")
              (dirname real-file)
              (canonicalize-path real-file))))
  (chmod wrapper #o755))

(define (concatenate-files files result)
  "Make RESULT the concatenation of all of FILES."
  (define (dump file port)
    (put-bytevector port
                    (call-with-input-file file
                      get-bytevector-all)))

  (call-with-output-file result
    (lambda (port)
      (for-each (cut dump <> port) files))))

(define* (install-static-output #:key outputs #:allow-other-keys)
  (let ((out (assoc-ref outputs "out"))
        (static (assoc-ref outputs "static")))
    (if static
        (begin
          (for-each (lambda (file)
                      (if (eq? 'symlink
                               (stat:type (lstat file)))
                          (with-directory-excursion (string-append static
                                                                   "/lib")
                            (symlink (basename (readlink file))
                                     (basename file)))
                          (install-file file
                                        (string-append static "/lib")))
                      (delete-file file))
                    (find-files (string-append out "/lib") "\\.a$"))
          (for-each (cute install-file <>
                          (string-append static "/include"))
                    (find-files (string-append out "/include"))))
        (format #t "no static output, nothing to be done~%"))))
