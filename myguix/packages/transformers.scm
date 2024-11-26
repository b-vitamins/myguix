(define-module (myguix packages transformers)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages nvidia))

(define-public python-lion-pytorch
  (package
    (name "python-lion-pytorch")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lion_pytorch" version))
       (sha256
        (base32 "18pp6k02nfd6p2yfqqrz7v1cyi3k11mksl2sq2n87hsp3b53xba6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-torch))
    (home-page "https://github.com/lucidrains/lion-pytorch")
    (synopsis "Lion Optimizer - Pytorch")
    (description "Lion Optimizer - Pytorch.")
    (license license:expat)))

(define-public python-lion-pytorch-cuda
  (package
    (inherit python-lion-pytorch)
    (name "python-lion-pytorch-cuda")
    (propagated-inputs (list python-torch-cuda cuda-toolkit-12.4
                             nvidia-driver-recommended))))
