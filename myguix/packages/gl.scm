(define-module (myguix packages gl)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages gl)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils))

(define-public mesa-intel-xe-kmd
  (package/inherit mesa
    (name "mesa-intel-xe-kmd")
    (source (origin
              (inherit (package-source mesa))))
    (arguments (substitute-keyword-arguments (package-arguments mesa)
                 ((#:configure-flags flags)
                  #~(cons "-Dintel-xe-kmd=enabled"
                          #$flags))))))

(define mesa/intel-xe-kmd
  (package
    (inherit mesa)
    (replacement mesa-intel-xe-kmd)))

(define-public transform-mesa
  (package-input-rewriting `((,mesa unquote mesa/intel-xe-kmd))))
