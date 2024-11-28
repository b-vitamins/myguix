(define-module (myguix packages transformers)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages check)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
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

(define-public python-bitsandbytes-cuda
  (package
    (inherit python-bitsandbytes)
    (name "python-bitsandbytes-cuda")
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'configure
                     (lambda _
                       (invoke "cmake" "-DCOMPUTE_BACKEND=cuda"
                               "-DCOMPUTE_CAPABILITY=80;86" "-S" ".")
                       (invoke "make")))
                   (delete 'strip-binaries)
                   (add-after 'install 'add-libbitsandbytes_cuda124.so
                     (lambda _
                       (let* ((site (string-append #$output "/lib/python"
                                     #$(version-major+minor (package-version
                                                             python))
                                     "/site-packages/bitsandbytes")))
                         (install-file
                          "bitsandbytes/libbitsandbytes_cuda124.so" site)
                         (install-file (string-append #$(this-package-input
                                                         "python-bitsandbytes")
                                        "/lib/python"
                                        #$(version-major+minor (package-version
                                                                python))
                                        "/site-packages/bitsandbytes/libbitsandbytes_cpu.so")
                                       site)))))))
    (inputs (modify-inputs (package-inputs python-bitsandbytes)
              (replace "python-lion-pytorch" python-lion-pytorch-cuda)
              (append python-bitsandbytes)))
    (propagated-inputs (list nvidia-driver-recommended cuda-toolkit-12.4))))

(define-public python-transformers
  (package
    (name "python-transformers")
    (version "4.46.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transformers" version))
       (sha256
        (base32 "1k3jbwlk3v99nyskrgm9gjq5h8059dzq73pzmy13xqrzjjpb7r4f"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-filelock
                             python-huggingface-hub
                             python-numpy
                             python-packaging
                             python-pyyaml
                             python-regex
                             python-requests
                             python-safetensors
                             python-tokenizers
                             python-tqdm))
    (native-inputs (list python-accelerate
                         python-av
                         python-beautifulsoup4
                         python-codecarbon
                         python-cookiecutter
                         python-datasets
                         python-deepspeed
                         python-dill
                         python-evaluate
                         python-faiss
                         python-flax
                         python-fugashi
                         python-gitpython
                         python-ipadic
                         python-isort
                         python-jax
                         python-jaxlib
                         python-kenlm
                         python-keras-nlp
                         python-libcst
                         python-librosa
                         python-nltk
                         python-onnxconverter-common
                         python-onnxruntime
                         python-onnxruntime-tools
                         python-optax
                         python-optuna
                         python-parameterized
                         python-phonemizer
                         python-pillow
                         python-protobuf
                         python-psutil
                         python-pyctcdecode
                         python-pydantic
                         python-pytest
                         python-pytest-rich
                         python-pytest-timeout
                         python-pytest-xdist
                         python-ray
                         python-rhoknp
                         python-rich
                         python-rjieba
                         python-rouge-score
                         python-ruff
                         python-sacrebleu
                         python-sacremoses
                         python-scikit-learn
                         python-scipy
                         python-sentencepiece
                         python-sigopt
                         python-sudachidict-core
                         python-sudachipy
                         python-tensorboard
                         python-tensorflow
                         python-tensorflow-text
                         python-tf2onnx
                         python-timeout-decorator
                         python-timm
                         python-tokenizers
                         python-torch
                         python-torchaudio
                         python-torchvision
                         python-unidic
                         python-unidic-lite
                         python-urllib3))
    (home-page "https://github.com/huggingface/transformers")
    (synopsis
     "State-of-the-art Machine Learning for JAX, PyTorch and TensorFlow")
    (description
     "State-of-the-art Machine Learning for JAX, @code{PyTorch} and @code{TensorFlow}.")
    (license license:asl2.0)))
