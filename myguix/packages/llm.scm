(define-module (myguix packages llm)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control))

;; TODO: Packages that need to be packaged for vLLM:
;; - python-compressed-tensors
;; - python-lm-format-enforcer
;; - python-mistral-common
;; - python-ninja
;; - python-openai-harmony
;; - python-outlines-core
;; - python-partial-json-parser
;; - python-prometheus-fastapi-instrumentator
;; - python-pybase64
;; - python-ray

(define-public python-depyf
  (package
    (name "python-depyf")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "depyf" version))
       (sha256
        (base32 "0i6blns91sqf0pargcj3r91a9vc533q0s8m61z4iq51dncb0kvdg"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require autopep8 which has build issues
    (propagated-inputs (list python-astor python-dill))
    (native-inputs (list git-minimal python-setuptools python-wheel))
    (home-page "https://github.com/thuml/depyf")
    (synopsis "Decompile python functions, from bytecode to source code!")
    (description "Decompile python functions, from bytecode to source code!")
    (license license:expat)))
