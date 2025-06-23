(define-module (myguix packages nlp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module ((guix build utils)
                #:hide (delete))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages huggingface))

(define-public whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/whisper.cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fs15rizz4psd3flfjpdivzvc9w19i3706flisn136ax0k8r7w5n"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;No tests.
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON" "-DGGML_BLAS=ON"
                           "-DGGML_BLAS_VENDOR=OpenBLAS" "-DGGML_OPENMP=ON")
       #:modules ((guix build utils)
                  (guix build cmake-build-system)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-binaries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (build-dir (string-append (getcwd) "/bin/")))
                        ;; Install files that already have correct names
                        (for-each (lambda (file)
                                    (install-file (string-append build-dir
                                                                 file) bin))
                                  '("whisper-cli" "whisper-server"
                                    "whisper-bench"))
                        (for-each (lambda (file)
                                    (let ((orig-file (string-append build-dir
                                                                    file))
                                          (new-file (if (string=? file "main")
                                                        (string-append
                                                         build-dir "/whisper")
                                                        (string-append
                                                         build-dir "/whisper-"
                                                         file))))
                                      (invoke "mv" orig-file new-file)
                                      (install-file new-file bin)))
                                  '("main" "quantize")))))
                  (add-after 'install-binaries 'fix-rpath
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((libdir (string-append (assoc-ref outputs "out")
                                                   "/lib"))
                            (bindir (string-append (assoc-ref outputs "out")
                                                   "/bin")))
                        (for-each (lambda (file)
                                    (let* ((pipe (open-pipe* OPEN_READ
                                                             "patchelf"
                                                             "--print-rpath"
                                                             file))
                                           (line (read-line pipe)))
                                      (and (zero? (close-pipe pipe))
                                           (invoke "patchelf" "--set-rpath"
                                                   (string-append libdir ":"
                                                                  line) file))))
                                  (find-files bindir))))))))
    (inputs (list openblas))
    (native-inputs (list git-minimal patchelf-0.16 python pkg-config))
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (synopsis "Port of OpenAI's Whisper model in C/C++")
    (description
     "This package provides a port to OpenAI's Whisper Automatic Speech Recognition Models. It requires model parameters to be downloaded independently and to be able to run a Whisper model.")
    (license license:expat)))

(define-public whisper-cpp-cuda
  (package
    (inherit whisper-cpp)
    (name "whisper-cpp-cuda")
    (inputs (list cuda-toolkit openblas))
    (arguments
     (substitute-keyword-arguments (package-arguments whisper-cpp)
       ((#:configure-flags flags
         ''())
        #~(append '("-DGGML_CUDA=ON" "-DCMAKE_CUDA_ARCHITECTURES=75;80;86"
                    "-DGGML_CUDA_FA_ALL_QUANTS=true")
                  #$flags))))))

(define-public llama-cpp-cuda
  (package
    (inherit llama-cpp)
    (name "llama-cpp-cuda")
    (inputs (list python))
    (propagated-inputs (list cuda-toolkit python-numpy python-pytorch
                             python-sentencepiece openblas))
    (native-inputs (list git-minimal pkg-config))
    (arguments
     (substitute-keyword-arguments (package-arguments llama-cpp)
       ((#:configure-flags flags
         ''())
        #~(list "-DBUILD_SHARED_LIBS=ON"
                "-DGGML_NATIVE=OFF"
                "-DGGML_BLAS=ON"
                "-DGGML_BLAS_VENDOR=OpenBLAS"
                "-DGGML_OPENMP=ON"
                "-DGGML_CUDA=ON"
                "-DCMAKE_CUDA_ARCHITECTURES=75;80;86"
                "-DGGML_CUDA_FA_ALL_QUANTS=true"))
       ((#:phases phases
         #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'check)))))))

(define-public python-morfessor
  (package
    (name "python-morfessor")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Morfessor" version))
       (sha256
        (base32 "1cmsxyd7ymlqlgam9a6va0x3fqhz0w1mixj0yv2j85rl6k1flfxv"))))
    (build-system python-build-system)
    (home-page "http://morpho.aalto.fi")
    (synopsis "Morfessor")
    (description "Morfessor")
    (license license:bsd-3)))

(define-public python-nmslib
  (let ((pybind11-for-nmslib (package
                               (inherit pybind11)
                               (name "pybind11")
                               (version "2.6.1")
                               (source
                                (origin
                                  (method git-fetch)
                                  (uri (git-reference
                                        (url
                                         "https://github.com/pybind/pybind11")
                                        (commit (string-append "v" version))))
                                  (sha256
                                   (base32
                                    "1wh5b1xnywzxwxkyac2wvyqwzmy1qxs341jjk820r7b825wn6yad"))
                                  (file-name (git-file-name name version))))
                               (arguments
                                (substitute-keyword-arguments (package-arguments
                                                               pybind11)
                                  ((#:phases phases
                                    #~%standard-phases)
                                   #~(modify-phases #$phases
                                       (add-after 'unpack 'skip-failing-test
                                         (lambda _
                                           (substitute* "tests/test_exceptions.py"
                                             ;; This test fails with Python 3.10; skip it.
                                             (("^def test_python_alreadyset_in_destructor(.*)"
                                               _ rest)
                                              (string-append
                                               "def test_python_alreadyset_in_destructor"
                                               rest "\n" "    return\n"))))))))))))
    (package
      (name "python-nmslib")
      (version "2.1.1")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "nmslib" version))
         (sha256
          (base32 "084wl5kl2grr2yi3bibc6i2ak5s7vanwi21wssbwd4bgfskr84lp"))))
      (build-system python-build-system)
      (propagated-inputs (list python-numpy python-psutil pybind11-for-nmslib))
      (home-page "https://github.com/nmslib/nmslib")
      (synopsis "Non-Metric Space Library (NMSLIB)")
      (description "Non-Metric Space Library (NMSLIB)")
      (license license:asl2.0))))

(define-public python-tiktoken
  (package
    (name "python-tiktoken")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tiktoken" version))
       (sha256
        (base32 "0p9cg6n8mzdi4lbbwxrrp26chy5hr14bqmzr3w74kq1qm6k5qanh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases
                       'build))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases
                       'install)))
      #:cargo-inputs `(("rust-pyo3" ,rust-pyo3-0.22)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-fancy-regex" ,rust-fancy-regex-0.13)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-bstr" ,rust-bstr-1))))
    (propagated-inputs (list python-regex python-requests))
    (native-inputs (list python-wrapper python-setuptools
                         python-setuptools-rust python-wheel))
    (home-page "https://github.com/openai/tiktoken")
    (synopsis "tiktoken is a fast BPE tokeniser for use with OpenAI's models")
    (description
     "tiktoken is a fast BPE tokeniser for use with @code{OpenAI's} models.")
    (license license:expat)))

(define-public python-litellm
  (package
    (name "python-litellm")
    (version "1.55.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "litellm" version))
       (sha256
        (base32 "05gs6pawd7gbk8ljlf15i94aibgkzzjvdd2aa9wxvjfl8j9ghip3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require network access and API keys
      #:tests? #f))
    (propagated-inputs (list python-tiktoken
                             python-click
                             python-jinja2
                             python-certifi
                             python-pyyaml
                             python-pydantic))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/BerriAI/litellm")
    (synopsis
     "Library to simplify calling 100+ LLM APIs using the OpenAI format")
    (description
     "LiteLLM is a library to simplify calling Anthropic, Azure, Huggingface,
Replicate, Cohere, OpenAI, and more.  It provides a unified interface following
OpenAI's API format to interact with various large language model providers.")
    (license license:expat)))

(define-public python-llama-cpp-python
  (package
    (name "python-llama-cpp-python")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "llama_cpp_python" version))
       (sha256
        (base32 "0md8vfbsjvkx6s2aacy9idjlfi2k5qrbb359vaqd95pjifyv932h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require model files
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-environment
            (lambda _
              ;; Build CPU-only version
              (setenv "CMAKE_ARGS" "-DGGML_CUDA=OFF -DGGML_METAL=OFF")
              ;; Use system BLAS
              (setenv "CMAKE_ARGS"
                      (string-append (getenv "CMAKE_ARGS")
                       " -DGGML_BLAS=ON -DGGML_BLAS_VENDOR=OpenBLAS"))))
          (delete 'sanity-check)
          (delete 'validate-runpath))))
    (propagated-inputs (list python-numpy python-typing-extensions
                             python-jinja2))
    (native-inputs (list cmake pkg-config python-scikit-build-core
                         python-setuptools python-wheel))
    (inputs (list openblas))
    (home-page "https://github.com/abetlen/llama-cpp-python")
    (synopsis "Python bindings for llama.cpp")
    (description
     "Python bindings for llama.cpp, enabling use of Llama and other large
language models with GPU acceleration via various backends including CUDA,
OpenCL, Metal, and CPU-based inference.")
    (license license:expat)))

(define-public python-ollama
  (package
    (name "python-ollama")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ollama" version))
       (sha256
        (base32 "1yvz1zfx7p9r172vcha76lak9964550ai7ny5nyblff7fd10dnz1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require running Ollama server
      #:tests? #f))
    (propagated-inputs (list python-httpx python-pydantic))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/ollama/ollama-python")
    (synopsis "Python client for Ollama")
    (description
     "Official Python client library for Ollama.  Provides a simple interface
to interact with Ollama's API for running large language models locally.")
    (license license:expat)))

(define-public ollama-binary
  (package
    (name "ollama-binary")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://ollama.com/download/ollama-linux-amd64.tgz")
       (sha256
        (base32 "1g0dqwv37abq6kxxcg3k0jaw5ynhrgm2f0p3qnvgd53szj4vlapg"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; Skip RUNPATH validation as this is a pre-built binary
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-to-source-root
            (lambda _
              ;; The unpack phase puts us in the bin directory, move up
              (chdir "..")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; The tarball extracts bin/ and lib/ directories
                (copy-recursively "bin"
                                  (string-append out "/bin"))
                (copy-recursively "lib"
                                  (string-append out "/lib")))))
          (add-after 'install 'patch-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib")))
                ;; Make binary executable
                (chmod (string-append bin "/ollama") #o755)
                ;; Create wrapper script
                (call-with-output-file (string-append bin "/ollama-wrapper")
                  (lambda (port)
                    (format port
                     "#!/bin/sh
# Ollama wrapper script - sets up model discovery paths and library paths

# Set library path for Ollama libraries
export LD_LIBRARY_PATH=\"~a/ollama:${LD_LIBRARY_PATH}\"

# Default model paths
if [ -z \"$OLLAMA_MODELS\" ]; then
    OLLAMA_MODELS=\"$HOME/.ollama/models\"

    # Add Guix store model paths from propagated inputs
    for dir in ~a/share/ollama/models/*; do
        if [ -d \"$dir\" ]; then
            OLLAMA_MODELS=\"$OLLAMA_MODELS:$dir\"
        fi
    done

    export OLLAMA_MODELS
fi

# Default host if not set
if [ -z \"$OLLAMA_HOST\" ]; then
    export OLLAMA_HOST=\"127.0.0.1:11434\"
fi

exec ~a/bin/.ollama-real \"$@\"
"
                     lib out out)))
                (chmod (string-append bin "/ollama-wrapper") #o755)
                ;; Rename original and make wrapper the default
                (rename-file (string-append bin "/ollama")
                             (string-append bin "/.ollama-real"))
                (symlink "ollama-wrapper"
                         (string-append bin "/ollama"))))))))
    (home-page "https://github.com/ollama/ollama")
    (synopsis "Run large language models locally (binary distribution)")
    (description
     "Ollama is a tool for running large language models locally.  It provides
a simple interface for downloading, managing, and running various LLM models
on your own hardware.  This package uses the pre-built binary distribution.")
    (license license:expat)))

(define-public ollama-model-llama2-7b
  (package
    (name "ollama-model-llama2-7b")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://huggingface.co/TheBloke/Llama-2-7B-GGUF/resolve/main/llama-2-7b.Q4_K_M.gguf")
       (sha256
        (base32 "02qvwm611vd3libkyb4h2027vm4dwmnc4v7d5ngmmni14a620rs5"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out #$output)
                 (model-dir (string-append out "/share/ollama/models/llama2"))
                 (source #$(package-source this-package)))
            (mkdir-p model-dir)
            (copy-file source
                       (string-append model-dir "/7b.gguf"))
            ;; Create manifest for model discovery
            (call-with-output-file (string-append model-dir "/manifest.json")
              (lambda (port)
                (display "{
  \"name\": \"llama2:7b\",
  \"model\": \"7b.gguf\",
  \"parameters\": {
    \"num_ctx\": 2048,
    \"num_gpu\": 1
  }
}" port)))))))
    (home-page "https://ollama.com/library/llama2")
    (synopsis "Llama 2 7B model for Ollama")
    (description
     "Llama 2 is a collection of pretrained and fine-tuned generative text models
ranging in scale from 7 billion to 70 billion parameters.  This package provides
the 7B parameter model in GGUF format for use with Ollama.")
    (license license:expat)))

(define-public ollama-model-mistral-7b
  (package
    (name "ollama-model-mistral-7b")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://huggingface.co/TheBloke/Mistral-7B-v0.1-GGUF/resolve/main/mistral-7b-v0.1.Q4_K_M.gguf")
       (sha256
        (base32 "0374pfy7s638rs02kichrf7s2ks3n08q9cr4b71s1phsx7956qnf"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out #$output)
                 (model-dir (string-append out "/share/ollama/models/mistral"))
                 (source #$(package-source this-package)))
            (mkdir-p model-dir)
            (copy-file source
                       (string-append model-dir "/model.gguf"))
            ;; Create manifest for model discovery
            (call-with-output-file (string-append model-dir "/manifest.json")
              (lambda (port)
                (display "{
  \"name\": \"mistral:7b\",
  \"model\": \"model.gguf\",
  \"parameters\": {
    \"num_ctx\": 8192,
    \"num_gpu\": 1
  }
}" port)))))))
    (home-page "https://ollama.com/library/mistral")
    (synopsis "Mistral 7B model for Ollama")
    (description
     "Mistral 7B is a 7.3B parameter model that outperforms Llama 2 13B on all
benchmarks and matches Llama 1 34B on many benchmarks.")
    (license license:asl2.0)))

(define-public ollama-with-llama2
  (package
    (inherit ollama-binary)
    (name "ollama-with-llama2")
    (propagated-inputs (list ollama-model-llama2-7b))
    (synopsis "Ollama with Llama 2 7B model")
    (description (string-append (package-description ollama-binary)
                  "  This variant includes the Llama 2 7B model."))))

(define-public ollama-with-mistral
  (package
    (inherit ollama-binary)
    (name "ollama-with-mistral")
    (propagated-inputs (list ollama-model-mistral-7b))
    (synopsis "Ollama with Mistral 7B model")
    (description (string-append (package-description ollama-binary)
                  "  This variant includes the Mistral 7B model."))))

(define-public nougat-ocr
  (package
    (name "nougat-ocr")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nougat_ocr" version "whl"))
       (sha256
        (base32 "067ixvp157zi4yj4aw6nf7i4iylldsrlf6p1gln9fl32f4n76xpp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require model downloads
    (propagated-inputs (list python-transformers
                             python-timm
                             python-orjson
                             python-opencv-python-headless
                             python-datasets
                             ;; python-pytorch-lightning
                             python-nltk
                             python-rapidfuzz
                             python-sentencepiece
                             python-sconf
                             python-albumentations
                             python-pypdf
                             python-pypdfium2
                             ;; For [api] extra
                             python-fastapi
                             python-uvicorn
                             python-multipart))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/facebookresearch/nougat")
    (synopsis "Neural Optical Understanding for Academic Documents")
    (description
     "Nougat is a Visual Transformer model that performs Optical Character
Recognition (OCR) on academic documents and outputs structured markup.  It can
convert PDF documents to text with mathematical equations, tables, and other
academic content preserved in markup format.")
    (license license:expat)))
