(define-module (myguix packages huggingface)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages machine-learning))

(define-public python-accelerate
  (package
    (name "python-accelerate")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "accelerate" version))
       (sha256
        (base32 "19bkhx9smk3fm6cgb8inwvk16gpp3hhsw219rlf6if2cnvhi7m1p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-huggingface-hub
                             python-numpy
                             python-packaging
                             python-psutil
                             python-pyyaml
                             python-setuptools
                             python-safetensors
                             python-pytorch-cuda))
    (native-inputs (list python-bitsandbytes
                         python-black
                         python-parameterized
                         python-pytest
                         python-pytest-subtests
                         python-pytest-xdist
                         python-rich
                         python-ruff
                         python-setuptools
                         python-scikit-learn
                         python-scipy
                         python-tqdm))
    (home-page "https://github.com/huggingface/accelerate")
    (synopsis "Accelerate")
    (description "Accelerate.")
    (license license:asl2.0)))
