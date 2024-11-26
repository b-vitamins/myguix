(define-module (myguix packages transformers)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
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

(define-public python-bitsandbytes
  (package
    (name "python-bitsandbytes")
    (version "0.44.1")
    (home-page "https://github.com/bitsandbytes-foundation/bitsandbytes")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jd7nsih9z2zp0aa82sl5kgxaqyjzlgv7hhfbrw9lawc57kl7z6a"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'configure
                     (lambda _
                       (invoke "cmake" "-DCOMPUTE_BACKEND=cpu" "-S" ".")
                       (invoke "make")))
                   (delete 'strip-binaries)
                   (add-after 'install 'add-libbitsandbytes_cpu.so
                     (lambda _
                       (let* ((site (string-append #$output "/lib/python"
                                     #$(version-major+minor (package-version
                                                             python))
                                     "/site-packages/bitsandbytes")))
                         (install-file "bitsandbytes/libbitsandbytes_cpu.so"
                                       site)))))))
    (inputs (list python-setuptools
                  python-pytest
                  python-lion-pytorch
                  python-einops
                  python-wheel
                  python-scipy
                  python-pandas
                  python-matplotlib))
    (native-inputs (list cmake-minimal))
    (synopsis "Fuzzy matching library for Python")
    (description
     "The @code{bitsandbytes} library is a lightweight Python wrapper around CUDA custom functions, in particular 8-bit optimizers, matrix multiplication (LLM.int8()), and 8 & 4-bit quantization functions.

The library includes quantization primitives for 8-bit & 4-bit operations, through @code{bitsandbytes.nn.Linear8bitLt} and @code{bitsandbytes.nn.Linear4bit} and 8-bit optimizers through @code{bitsandbytes.optim module}.

There are ongoing efforts to support further hardware backends, i.e. Intel CPU + GPU, AMD GPU, Apple Silicon. Windows support is quite far along and is on its way as well.

Please head to the official documentation page: @url{https://huggingface.co/docs/bitsandbytes/main}")
    (license license:asl2.0)))
