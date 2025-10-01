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
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (myguix packages huggingface))

;; TODO: Packages that need to be packaged for vLLM:
;; - python-ninja
;; - python-openai-harmony
;; - python-outlines-core
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

(define-public python-lm-format-enforcer
  (package
    (name "python-lm-format-enforcer")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lm_format_enforcer" version))
       (sha256
        (base32 "1ni8snbglb51lgmvai8rb84gnvxj16bqik4v98lcx73i130q3076"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require interegular which is not packaged
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/noamgat/lm-format-enforcer")
    (synopsis
     "Enforce the output format (JSON Schema, Regex etc) of a language model")
    (description
     "Enforce the output format (JSON Schema, Regex etc) of a language model.")
    (license license:expat)))

(define-public python-pydantic-extra-types
  (package
    (name "python-pydantic-extra-types")
    (version "2.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydantic_extra_types" version))
       (sha256
        (base32 "1n5sl9cczhynh1pkvnjhy2mavgz7j13bn3cf10pl46klrz0a5kqx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require pytest
    (propagated-inputs (list python-pycountry
                             python-pydantic-2
                             python-typing-extensions))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/pydantic/pydantic-extra-types")
    (synopsis "Extra Pydantic types")
    (description "Extra Pydantic types.")
    (license license:expat)))

(define-public python-mistral-common
  (package
    (name "python-mistral-common")
    (version "1.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistral_common" version))
       (sha256
        (base32 "1s2x08m72bn15lhdb72s5qzgk3p738waj252828g01y8x7nh8qlz"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; No tests in tarball
    (propagated-inputs (list python-fastapi
                             python-jsonschema
                             python-numpy
                             python-pillow
                             python-pydantic-2
                             python-pydantic-extra-types
                             python-requests
                             python-tiktoken
                             python-typing-extensions
                             python-uvicorn))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mistralai/mistral-common")
    (synopsis
     "Common utilities library for Mistral AI")
    (description
     "Mistral-common is a library of common utilities for Mistral AI.")
    (license license:asl2.0)))

(define-public python-compressed-tensors
  (package
    (name "python-compressed-tensors")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "compressed_tensors" version))
       (sha256
        (base32 "07sbqk7wc8hs1hcj0ra92n04z8p8a9glx1nxjijdyxgpk6bg3pcm"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require additional dependencies
    (propagated-inputs (list python-frozendict
                             python-pydantic-2
                             python-pytorch
                             python-transformers))
    (native-inputs (list python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/neuralmagic/compressed-tensors")
    (synopsis
     "Library for utilization of compressed safetensors of neural network models")
    (description
     "Library for utilization of compressed safetensors of neural network models.")
    (license license:asl2.0)))

(define-public python-partial-json-parser
  (package
    (name "python-partial-json-parser")
    (version "0.2.1.1.post6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "partial_json_parser" version))
       (sha256
        (base32 "1z1sm7p5z7jnxgbvgf1h9gnis9g9bxm4m274pd624y4nj9l6p2a3"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend))
    (home-page "https://github.com/tecton-ai/partial-json-parser")
    (synopsis "Parse partial JSON generated by LLM")
    (description "Parse partial JSON generated by LLM.")
    (license license:expat)))

(define-public python-prometheus-fastapi-instrumentator
  (package
    (name "python-prometheus-fastapi-instrumentator")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prometheus_fastapi_instrumentator" version))
       (sha256
        (base32 "0pmyki1kzbzmx9wjsm14i5yj4gqvcg362hnbxhm93rd4xqgdcz5y"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-prometheus-client python-starlette))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/trallnag/prometheus-fastapi-instrumentator")
    (synopsis "Instrument your FastAPI app with Prometheus metrics")
    (description "Instrument your @code{FastAPI} app with Prometheus metrics.")
    (license license:isc)))

(define-public python-pybase64
  (package
    (name "python-pybase64")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybase64" version))
       (sha256
        (base32 "1l4jfvwpm0nigz4kwvnf26lbkj8dx16y8bwmblql75pdhg9fzka6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require pytest
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mayeut/pybase64")
    (synopsis "Fast Base64 encoding/decoding")
    (description "Fast Base64 encoding/decoding.")
    (license license:bsd-2)))
