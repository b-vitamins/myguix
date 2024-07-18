(define-module (myguix packages)
  #:use-module (guix ui)
  #:use-module (guix diagnostics))

(define %patch-path
  (make-parameter (map (lambda (directory)
                         (string-append directory "/myguix/packages/patches"))
                       %load-path)))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found") file-name))))
