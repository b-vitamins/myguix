(define-module (myguix packages nlp)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages bioinformatics)
                #:select (python-pyahocorasick))
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module ((gnu packages machine-learning)
                #:hide (python-safetensors python-transformers))
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module ((gnu packages python-web)
                #:hide (python-huggingface-hub))
  #:use-module ((gnu packages python-xyz)
                #:hide (python-deprecated))
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module ((guix build utils)
                #:hide (delete which))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module ((guix build-system python)
                #:hide (pypi-uri))
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((myguix licenses)
                #:prefix myguix-license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages nvidia)
  #:use-module ((myguix packages python-pqrs)
                #:hide (python-future))
  #:use-module ((myguix packages rust-crates-pqrs)
                #:select (lookup-myguix-cargo-inputs))
  #:use-module ((guix build-system cargo)
                #:select (cargo-inputs))
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages audio)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages huggingface))

;; Helper function to use myguix cargo inputs
(define (myguix-cargo-inputs name)
  "Lookup Cargo inputs for NAME from myguix rust-crates-pqrs."
  (or (lookup-myguix-cargo-inputs name)
      (begin
        (format (current-error-port)
                "Warning: no Cargo inputs available for '~a'~%" name)
        '())))

(define-public whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/whisper.cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gn64jw4pr4vfnn2hll7yd98r8yhaqg97hhg5z22vq4j423436kn"))))
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
                  (add-after 'install 'create-whisper-symlink
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin")))
                        ;; Create whisper symlink pointing to whisper-cli
                        (symlink (string-append bin "/whisper-cli")
                                 (string-append bin "/whisper")))))
                  (add-after 'create-whisper-symlink 'fix-rpath
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
    (propagated-inputs (list cuda-toolkit python-numpy python-pytorch-cuda
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
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tiktoken" version))
       (sha256
        (base32 "0ccr2dr0gikyrr9ni0ykskacik876x5zf56biybn6f095gpag2xi"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:imported-modules (append %pyproject-build-system-modules
                                 %cargo-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (assoc-ref py:%standard-phases
                       'build))
          (add-after 'install 'wrap
            (lambda _
              ;; Collection of python- and pyproject-build-system phases
              ;; between 'install and 'check.
              (assoc-ref py:%standard-phases
                         'add-install-to-pythonpath)
              (assoc-ref py:%standard-phases
                         'add-install-to-path)
              (assoc-ref py:%standard-phases
                         'wrap)
              (assoc-ref py:%standard-phases
                         'create-entrypoints)
              (assoc-ref py:%standard-phases
                         'compile-bytecode)))
          (replace 'install
            (assoc-ref py:%standard-phases
                       'install)))))
    (propagated-inputs (list python-regex python-requests))
    (inputs (myguix-cargo-inputs 'python-tiktoken))
    (native-inputs (list python-setuptools python-setuptools-rust python-wheel
                         python-wrapper))
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
                                "This variant includes the Mistral 7B model."))))

(define python-timm-for-nougat
  (package
    (inherit python-timm)
    (name "python-timm")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "timm" version))
       (sha256
        (base32 "07qwj3gifdly4v2sf59layp2m23sx8axb45sk8035i3ndbk94ysx"))))
    (native-inputs (list python-setuptools python-wheel))))

(define-public python-tokenizers-for-nougat
  (package
    (name "python-tokenizers")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tokenizers" version))
       (sha256
        (base32 "14fby8yy2icvs07091rlkb3g89f9wrd7gz7abfz88m6x37hcdsg6"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet #~(begin
                    ;; Only keeping bindings.
                    (for-each (lambda (file)
                                (unless (member file
                                                '("." ".." "bindings"
                                                  "PKG-INFO"))
                                  (delete-file-recursively file)))
                              (scandir "."))
                    (for-each (lambda (file)
                                (unless (member file
                                                '("." ".."))
                                  (rename-file (string-append
                                                "bindings/python/" file) file)))
                              (scandir "bindings/python"))
                    (delete-file-recursively ".cargo")
                    ;; Remove the path dependency on parent tokenizers library
                    ;; (version is already specified)
                    (substitute* "Cargo.toml"
                      (("^path = .*")
                       ""))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-test-flags ''("--no-default-features")
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'check 'python-check
            (lambda _
              (copy-file "target/release/libtokenizers.so"
                         "py_src/tokenizers/tokenizers.so")
              (invoke "python3"
                      "-c"
                      (format #f "import sys; sys.path.append(\"~a/py_src\")"
                              (getcwd))
                      "-m"
                      "pytest"
                      "-s"
                      "-v"
                      "./tests/")))
          (add-after 'install 'install-python
            (lambda _
              (let* ((pversion #$(version-major+minor (package-version python)))
                     (lib (string-append #$output "/lib/python" pversion
                                         "/site-packages/"))
                     (info (string-append lib "tokenizers-"
                                          #$(package-version this-package)
                                          ".dist-info")))
                (mkdir-p info)
                (copy-file "PKG-INFO"
                           (string-append info "/METADATA"))
                (copy-recursively "py_src/tokenizers"
                                  (string-append lib "tokenizers"))))))))
    (native-inputs (list pkg-config python-minimal python-pytest))
    (inputs (cons oniguruma
                  (myguix-cargo-inputs 'python-tokenizers-for-nougat)))
    (home-page "https://huggingface.co/docs/tokenizers")
    (synopsis "Implementation of various popular tokenizers")
    (description
     "This package provides bindings to a Rust implementation of the most used
tokenizers, @code{rust-tokenizers}.")
    (license license:asl2.0)))

(define-public python-safetensors-cuda
  (package
    (inherit python-safetensors)
    (name "python-safetensors-cuda")
    (native-inputs (modify-inputs (package-native-inputs python-safetensors)
                     (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-transformers-cuda
  (package
    (inherit python-transformers)
    (name "python-transformers-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-transformers)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-safetensors" python-safetensors-cuda)))
    (synopsis "Machine Learning for PyTorch and TensorFlow with CUDA PyTorch")
    (description
     "This package provides Transformers with dependencies adjusted to use the
CUDA-enabled PyTorch and Safetensors packages.")))

(define-public python-transformers-for-nougat
  (package
    (inherit python-transformers)
    (name "python-transformers")
    (version "4.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transformers" version))
       (sha256
        (base32 "1mbxhmh5kglxc59h1l5xn6nnfmfyl975vh54n940m9dqhbb7mz65"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Disable tests to avoid SSL certificate issues
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-compatibility
            (lambda _
              ;; Fix pytest 8.x compatibility issue
              (substitute* "src/transformers/testing_utils.py"
                ;; Remove import_path from the _pytest.doctest import
                (("from _pytest\\.doctest import \\(
.*import_path,")
                 "from _pytest.pathlib import import_path
from _pytest.doctest import (")
                ;; Alternative: if the import is on one line
                (("from _pytest\\.doctest import \\(([^)]*), import_path")
                 "from _pytest.pathlib import import_path
from _pytest.doctest import (\\1")
                ;; Remove trailing comma if import_path was last
                (("import_path,\\s*\\)")
                 ")")
                ;; Handle case where import_path might be in the middle
                (("import_path,\\s*")
                 "")))))))
    ;; The imported package contains ~60 more inputs, but they don't seem
    ;; necessary to build a minimal version of the package.
    (propagated-inputs (list python-filelock
                             python-huggingface-hub
                             python-numpy
                             python-packaging
                             python-pytorch-cuda
                             python-pyyaml
                             python-regex
                             python-requests
                             python-safetensors-cuda
                             python-tokenizers-for-nougat
                             python-tqdm))
    (native-inputs (list python-parameterized python-pytest python-setuptools
                         python-wheel))))

(define python-multiprocess-for-nougat
  (package
    (inherit python-multiprocess)
    (version "0.70.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "multiprocess" version))
       (sha256
        (base32 "1h8s2zmlmf8if05bwhcks7zxv9z4rp7bqsmy20a0wak5sh1zf6hn"))))))

(define-public python-dill-for-nougat
  (package
    (inherit python-dill)
    (name "python-dill")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dill" version))
       (sha256
        (base32 "1jlpyw6gqjk4xp6ylblqhhfnnj4vv12785ya79aw89fnk93krgiy"))))))

(define python-datasets-for-nougat
  (package
    (inherit python-datasets)
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-datasets)
                         (replace "python-multiprocess"
                                  python-multiprocess-for-nougat)
                         (replace "python-dill" python-dill-for-nougat)))))

(define-public python-nougat-ocr
  (let ((version "0.0")
        (commit "5a92920d342fb6acf05fc9b594ccb4053dbe8e7a")
        (revision "1"))
    (package
      (name "python-nougat-ocr")
      (version (git-version version revision commit))
      (home-page "https://github.com/facebookresearch/nougat")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hra1jbryps6gx000aq12d5idj16z38k3ps0wfk5hsqv6ai07dq0"))))
      (build-system python-build-system)
      (arguments
       (list
        #:tests? #f ;Tests require model downloads
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-deprecated-parameter
              (lambda _
                ;; Remove deprecated alpha_affine parameter
                (substitute* "nougat/transforms.py"
                  (("alpha_affine=120 \\* 0\\.01,")
                   "# alpha_affine removed - deprecated parameter"))))
            (add-before 'sanity-check 'set-cache-dirs
              (lambda _
                ;; Set environment variables to prevent cache directory creation
                (setenv "HOME" "/tmp")
                (setenv "TRANSFORMERS_CACHE" "/tmp/.cache/huggingface")
                (setenv "HF_HOME" "/tmp/.cache/huggingface")
                (setenv "TORCH_HOME" "/tmp/.cache/torch")
                ;; Create the directories
                (mkdir-p "/tmp/.cache/huggingface/hub")
                (mkdir-p "/tmp/.cache/torch/hub")))
            (delete 'sanity-check))))
      (propagated-inputs (list python-transformers-for-nougat
                               python-timm-for-nougat
                               python-orjson
                               python-opencv-python-headless
                               python-datasets-for-nougat
                               python-pytorch-lightning-cuda
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
      (synopsis "Neural Optical Understanding for Academic Documents")
      (description
       "Nougat is a Visual Transformer model that performs Optical Character
Recognition (OCR) on academic documents and outputs structured markup.  It can
convert PDF documents to text with mathematical equations, tables, and other
academic content preserved in markup format.")
      (license license:expat))))

(define-public python-conformer
  (package
    (name "python-conformer")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "conformer" version))
       (sha256
        (base32 "1nkyfwgpfkjg4c0a5azhs8clz243yp02a4066cf2xwqz8vx39vrj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests fail due to setup.py test command issues
    (propagated-inputs (list python-einops python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/lucidrains/conformer")
    (synopsis "The convolutional module from the Conformer paper")
    (description "The convolutional module from the Conformer paper.")
    (license license:expat)))

(define-public python-conformer-cuda
  (package
    (inherit python-conformer)
    (name "python-conformer-cuda")
    (propagated-inputs (list python-einops python-pytorch-cuda))))

(define-public python-pykakasi
  (package
    (name "python-pykakasi")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pykakasi" version))
       (sha256
        (base32 "18dhcw7myw5idajnfynjbvqxmyf9m0cygfwsavbbi7zmcf72l1gs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k" "not test_aozora"))) ;One test failure
    (propagated-inputs (list python-deprecated python-importlib-resources
                             python-jaconv))
    (native-inputs (list python-coverage
                         python-py-cpuinfo
                         python-pytest
                         python-pytest-benchmark
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://codeberg.org/miurahr/pykakasi")
    (synopsis "Kana kanji simple inversion library")
    (description
     "PyKakasi is a lightweight converter from Japanese Kana-kanji sentences
into Kana-Roman.  It provides Python Natural Language Processing (NLP)
capabilities to transliterate hiragana, katakana and kanji (Japanese text)
into rōmaji (Latin/Roman alphabet).")
    (license license:gpl3+)))

(define-public python-praat-parselmouth
  (package
    (name "python-praat-parselmouth")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "praat_parselmouth" version))
       (sha256
        (base32 "06l5qryfmhkc70fg1hjk35pbwm4aj01ziaia7w205lmrrb7hjkl4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests fail due to setup.py test command issues
    (propagated-inputs (list python-numpy))
    (native-inputs (list cmake ninja python-scikit-build python-setuptools
                         python-wheel))
    (home-page "https://github.com/YannickJadoul/Parselmouth")
    (synopsis "Praat in Python, the Pythonic way")
    (description
     "Parselmouth is a Python library that provides a Pythonic interface to
Praat, the speech analysis software.  It directly accesses Praat's C/C++ code,
ensuring that algorithms and their output are exactly the same as in Praat
while providing an interface that looks like any other Python library.  This
allows Python users to leverage Praat's sophisticated acoustic analysis tools
without needing to learn Praat's scripting language.")
    (license license:gpl3+)))

(define-public python-pkuseg
  (package
    (name "python-pkuseg")
    (version "0.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pkuseg" version))
       (sha256
        (base32 "148yp0l7h8cflxag62pc1iwj5b5liyljnaxwfjaiqwl96vwjn0fx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Test discovery fails with Config object error
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'regenerate-cython
            (lambda _
              ;; Delete pre-generated C/C++ files to force regeneration with
              ;; current Cython version (compatible with Python 3.11)
              (for-each delete-file
                        (find-files "." "\\.(c|cpp)$")))))))
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-cython python-setuptools python-wheel))
    (home-page "https://github.com/lancopku/pkuseg-python")
    (synopsis "A multi-domain Chinese word segmentation toolkit")
    (description
     "pkuseg is a multi-domain Chinese word segmentation toolkit developed by
Peking University.  It provides improved performance over other segmentation
tools, especially for domain-specific texts.")
    (license license:expat)))

(define-public python-s3tokenizer
  (package
    (name "python-s3tokenizer")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "s3tokenizer" version))
       (sha256
        (base32 "19gwsh06h2prhhzz23jfh89d6xmrsyscd3gqnvf7x77wz8snz0i1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests try to download models to home directory
    (propagated-inputs (list python-einops
                             python-numpy
                             onnx
                             pre-commit
                             python-pytorch
                             python-torchaudio
                             python-tqdm))
    (native-inputs (list python-black
                         python-flake8
                         python-isort
                         python-pytest
                         python-scipy
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/xingchensong/S3Tokenizer")
    (synopsis
     "Reverse Engineering of Supervised Semantic Speech Tokenizer (S3Tokenizer) proposed in CosyVoice")
    (description
     "S3Tokenizer is a Supervised Semantic Speech Tokenizer based on the
pre-trained SenseVoice-Large model, which enhances the semantic relationship
of extracted tokens to textual and paralinguistic information.  This is a
reverse-engineered PyTorch implementation of the tokenizer originally proposed
in CosyVoice.")
    (license license:asl2.0)))

(define-public python-addict
  (package
    (name "python-addict")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mewwts/addict")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dcqwmi6xbcc7zmsmq3djhvbybsz806lh837sgbrxppcmw2sfma3"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in the source release.
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mewwts/addict")
    (synopsis "Dictionary with attribute-style access")
    (description
     "Addict is a dictionary whose items can be set using both attribute and
item syntax.")
    (license license:expat)))

(define-public python-textsearch
  (package
    (name "python-textsearch")
    (version "0.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "textsearch" version))
       (sha256
        (base32 "06i4pdkbyq6cazrdb36lh22fqdn2xiqci2y1ridvc5b7271va8rd"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in the source release.
    (propagated-inputs (list python-anyascii python-pyahocorasick))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/kootenpv/textsearch")
    (synopsis "Fast multi-pattern string search")
    (description
     "Textsearch provides fast string and word search helpers backed by an
Aho-Corasick automaton.")
    (license license:expat)))

(define-public python-contractions
  (let ((commit "595188a45c472957427b3a6a07b6ce0492990fad")
        (revision "0"))
    (package
      (name "python-contractions")
      (version (git-version "0.1.72" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kootenpv/contractions")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14l1yg55w0bksd7b7xw37mnfp3s53dfrva1da7k1zc429vlihn0c"))))
      (build-system pyproject-build-system)
      (arguments
       '(#:tests? #f)) ;No tests in the source tree.
      (propagated-inputs (list python-textsearch))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/kootenpv/contractions")
      (synopsis "Expand English contractions")
      (description
       "This package expands English contractions and common slang forms, for
example converting \"you're\" to \"you are\".")
      (license license:expat))))

(define-public python-kaldifst
  (package
    (name "python-kaldifst")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/k2-fsa/kaldifst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nw75q9rffgs0g10xsaffm93082l9rhljlggpq9gwmfb1nj8prvx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;No installed test suite for the Python package.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'provide-cmake-fetchcontent-sources
            (lambda _
              ;; Upstream CMake uses FetchContent for these tarballs.  Put the
              ;; expected files in the source tree so the build stays hermetic.
              (copy-file #$(this-package-native-input "openfst-source")
                         "openfst-1.8.5-2026-04-10.tar.gz")
              (copy-file #$(this-package-native-input "pybind11-source")
                         "pybind11-3.0.0.tar.gz")))
          (add-before 'build 'configure-build
            (lambda _
              (setenv "KALDIFST_MAKE_ARGS" "-j1"))))))
    (native-inputs
     `(("openfst-source"
        ,(origin
           (method url-fetch)
           (uri "https://github.com/csukuangfj/openfst/archive/refs/tags/v1.8.5-2026-04-10.tar.gz")
           (sha256
            (base32 "16d1lsay7kikfjibzc6i5x4q02mxpyqwyawcy6llzgjc7109jm63"))))
       ("pybind11-source"
        ,(origin
           (method url-fetch)
           (uri "https://github.com/pybind/pybind11/archive/refs/tags/v3.0.0.tar.gz")
           (sha256
            (base32 "0310b80vgkwndli77gb3h30klsbdvg512947vblklv165cz1lfs5"))))
       ("cmake" ,cmake)
       ("python-setuptools" ,python-setuptools)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/k2-fsa/kaldifst")
    (synopsis "Kaldi FST Python bindings")
    (description
     "Kaldifst provides Python bindings for Kaldi-style finite-state
transducers.")
    (license license:asl2.0)))

(define-public python-wetext
  (package
    (name "python-wetext")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wetext" version))
       (sha256
        (base32 "08kbfh8kb4qb2mf7sm8gz5jxfjayyqqlpzlln5c51j2aiy3m3xx9"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in the source release.
    (propagated-inputs (list python-click python-contractions python-kaldifst))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pengzhendong/wetext")
    (synopsis "Runtime text normalization")
    (description
     "WeTextProcessing Runtime provides text normalization utilities used by
speech models.")
    (license license:asl2.0)))

(define-public python-modelscope
  (package
    (name "python-modelscope")
    (version "1.36.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modelscope/modelscope")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r48fjgs9ifrs45hk8r09zyjfzrig0bm58igk6zgicb6hnaa42g0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;The upstream suite expects networked model services.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'add-install-to-pythonpath 'prebuild-ast-index
            (lambda _
              ;; ModelScope lazily writes this index next to its own Python
              ;; modules.  Generate it while the output tree is still writable.
              (with-directory-excursion "/tmp"
                (invoke "python" "-c"
                        (string-append
                         "from modelscope.utils.ast_utils import "
                         "generate_ast_template; "
                         "generate_ast_template()"))))))))
    (propagated-inputs (list python-addict
                             python-filelock
                             python-numpy
                             python-packaging
                             python-pyyaml
                             python-requests
                             python-setuptools
                             python-tqdm
                             python-urllib3))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/modelscope/modelscope")
    (synopsis "Model-as-a-service toolkit")
    (description
     "ModelScope provides model discovery, downloading, and pipeline utilities.")
    (license license:asl2.0)))

(define-public python-argbind
  (package
    (name "python-argbind")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "argbind" version))
       (sha256
        (base32 "0iaph74pl38573xgrmz1zjnrd7d3ksy4fylwsn8qm1anmw29q58v"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests pull optional torch/torchvision example deps.
    (propagated-inputs (list python-docstring-parser python-pyyaml))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pseeth/argbind/")
    (synopsis "Bind Python function arguments to command-line flags")
    (description
     "Argbind provides helpers for binding Python function arguments to command
line interfaces.")
    (license license:expat)))

(define-public python-einx
  (package
    (name "python-einx")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "einx" version))
       (sha256
        (base32 "1s32dkjgvlqbsn23b2bfx38cc5xlsxg3cf2kaf8c8w3kq1vzzx3r"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require optional framework backends.
    (propagated-inputs (list python-frozendict python-numpy python-sympy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/fferflo/einx")
    (synopsis "Einstein-inspired tensor operations")
    (description
     "Einx provides NumPy-style tensor operation helpers with Einstein-inspired
notation.")
    (license license:expat)))

(define-public python-loralib
  (package
    (name "python-loralib")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "loralib" version))
       (sha256
        (base32 "0rs5pm7sq32fl32pa712nk542yb5sjmgksdfvmrvjm32994zzk12"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests in the source release.
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/microsoft/LoRA")
    (synopsis "PyTorch layers for low-rank adaptation")
    (description
     "Loralib provides PyTorch layers and helpers for low-rank adaptation of
large models.")
    (license license:expat)))

(define-public python-loralib-cuda
  (package
    (inherit python-loralib)
    (name "python-loralib-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-loralib)
                         (replace "python-pytorch" python-pytorch-cuda)))
    (synopsis "PyTorch layers for low-rank adaptation with CUDA support")))

(define-public python-pyrootutils
  (package
    (name "python-pyrootutils")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyrootutils" version))
       (sha256
        (base32 "1wn5ix9pcjc56c169iikk8rdcw265z5q7dxp704v5fkv2iksr2pm"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require a checked-out project layout.
    (propagated-inputs (list python-dotenv))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ashleve/pyrootutils")
    (synopsis "Project root setup helpers")
    (description
     "Pyrootutils provides helpers for locating a project root and updating the
Python path.")
    (license license:expat)))

(define-public python-descript-audio-codec
  (package
    (name "python-descript-audio-codec")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "descript-audio-codec" version))
       (sha256
        (base32 "1q001cgrizhca7rasd4bjxx9skjk0pa14rbmf0aq51l6290sb9an"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Tests require model assets and the full training stack.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'trim-audiotools-coupling
            (lambda _
              ;; Fish Speech only imports DAC layers, quantizers, and
              ;; CodecMixin.  Avoid importing the full audiotools training and
              ;; notebook stack on those inference-only imports.
              (substitute* "setup.py"
                (("^ *\"argbind>=0\\.3\\.7\",.*") "")
                (("^ *\"descript-audiotools>=0\\.7\\.2\",.*") ""))
              (when (file-exists? "descript_audio_codec.egg-info")
                (delete-file-recursively "descript_audio_codec.egg-info"))
              (call-with-output-file "dac/__init__.py"
                (lambda (port)
                  (display "__version__ = \"1.0.0\"\n" port)
                  (display "__model_version__ = \"latest\"\n" port)))
              (call-with-output-file "dac/model/__init__.py"
                (lambda (port)
                  (display "from .base import CodecMixin\n" port)
                  (display "from .base import DACFile\n" port)))
              (call-with-output-file "dac/nn/__init__.py"
                (lambda (port)
                  (display "from . import layers\n" port)
                  (display "from . import quantize\n" port)))
              (substitute* "dac/model/base.py"
                (("from audiotools import AudioSignal")
                 "class AudioSignal:\n    pass"))
              (substitute* "dac/nn/layers.py"
                (("# Scripting this brings model speed up 1\\.4x")
                 "# Run Snake eagerly for compatibility with this PyTorch build.")
                (("^@torch\\.jit\\.script") "")))))))
    (propagated-inputs (list python-einops
                             python-numpy
                             python-pytorch
                             python-torchaudio
                             python-tqdm))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/descriptinc/descript-audio-codec")
    (synopsis "Neural audio codec components")
    (description
     "Descript Audio Codec provides neural audio codec layers, quantizers, and
model helpers.  This package trims the optional audiotools coupling needed only
for the upstream CLI/training workflow so Fish Speech can use the codec modules
in a smaller inference environment.")
    (license license:expat)))

(define-public python-descript-audio-codec-cuda
  (package
    (inherit python-descript-audio-codec)
    (name "python-descript-audio-codec-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-descript-audio-codec)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchaudio" python-torchaudio-cuda)))
    (synopsis "Neural audio codec components with CUDA support")))

(define-public python-fish-speech
  (let ((commit "a391467946cc9c396c025fc69eba10ef0b38332e")
        (revision "0"))
    (package
      (name "python-fish-speech")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fishaudio/fish-speech")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "171b5jaxh4xyjjqxbpmvi9gxyvckayb16acd8hfrl475hi4assv6"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f ;Tests require the multi-GB model checkpoints.
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'focus-metadata-on-inference
              (lambda _
                ;; Upstream's metadata covers training, API server, Docker, and
                ;; WebUI deployments.  This package keeps the inference/WebUI
                ;; dependency set in Guix and avoids hard PyTorch pins.
                (substitute* "pyproject.toml"
                  (("torch==2\\.8\\.0") "torch")
                  (("torchaudio==2\\.8\\.0") "torchaudio")
                  (("transformers<=4\\.57\\.3") "transformers")
                  (("einx\\[torch\\]==0\\.2\\.2") "einx")
                  (("pydantic==2\\.9\\.2") "pydantic")
                  (("    \"datasets==2\\.18\\.0\",\\n") "")
                  (("    \"lightning>=2\\.1\\.0\",\\n") "")
                  (("    \"tensorboard>=2\\.14\\.1\",\\n") "")
                  (("    \"wandb>=0\\.19\\.0\",\\n") "")
                  (("    \"grpcio>=1\\.58\\.0\",\\n") "")
                  (("    \"kui>=1\\.6\\.0\",\\n") "")
                  (("    \"uvicorn>=0\\.30\\.0\",\\n") "")
                  (("    \"resampy>=0\\.4\\.3\",\\n") "")
                  (("    \"zstandard>=0\\.22\\.0\",\\n") "")
                  (("    \"pydub\",\\n") "")
                  (("    \"pyaudio\",\\n") "")
                  (("    \"modelscope==1\\.17\\.1\",\\n") "")
                  (("    \"opencc-python-reimplemented==0\\.1\\.7\",\\n") "")
                  (("    \"silero-vad\",\\n") "")
                  (("    \"ormsgpack\",\\n") "")
                  (("^packages = \\[\"fish_speech\", \"tools\"\\]")
                   (string-append
                    "include-package-data = true\n\n"
                    "[tool.setuptools.packages.find]\n"
                    "include = [\"fish_speech*\", \"tools*\"]\n"
                    "namespaces = true\n")))))
            (add-after 'unpack 'prune-unsupported-runtime-modules
              (lambda _
                ;; Keep the installed tree aligned with the dependency set
                ;; below: inference, WebUI, codec helpers, and quantization
                ;; tools.  The removed modules are training/data-server/API
                ;; helpers that pull in Lightning, datasets, Kui, ormsgpack,
                ;; pyaudio, and other stacks not provided by this package.
                (for-each
                 (lambda (path)
                   (when (file-exists? path)
                     (delete-file path)))
                 '("fish_speech/train.py"
                   "fish_speech/datasets/semantic.py"
                   "fish_speech/datasets/vqgan.py"
                   "fish_speech/i18n/scan.py"
                   "fish_speech/models/text2semantic/lit_module.py"
                   "fish_speech/utils/instantiators.py"
                   "fish_speech/utils/logger.py"
                   "fish_speech/utils/logging_utils.py"
                   "fish_speech/utils/rich_utils.py"
                   "fish_speech/utils/utils.py"
                   "tools/api_client.py"
                   "tools/api_server.py"
                   "tools/llama/build_dataset.py"
                   "tools/llama/eval_in_context.py"
                   "tools/vqgan/create_train_split.py"))
                (for-each
                 (lambda (path)
                   (when (file-exists? path)
                     (delete-file-recursively path)))
                 '("fish_speech/callbacks"
                   "tools/server"))))
            (add-after 'unpack 'avoid-training-imports
              (lambda _
                ;; fish_speech.utils imports Lightning-oriented helpers from
                ;; training code.  The inference engine only needs these two
                ;; small utilities.
                (call-with-output-file "fish_speech/utils/__init__.py"
                  (lambda (port)
                    (display "import random\n" port)
                    (display "import numpy as np\n" port)
                    (display "import torch\n" port)
                    (display "from .context import autocast_exclude_mps\n\n" port)
                    (display "def set_seed(seed: int):\n" port)
                    (display "    if seed < 0:\n" port)
                    (display "        seed = -seed\n" port)
                    (display "    if seed > (1 << 31):\n" port)
                    (display "        seed = 1 << 31\n" port)
                    (display "    random.seed(seed)\n" port)
                    (display "    np.random.seed(seed)\n" port)
                    (display "    torch.manual_seed(seed)\n" port)
                    (display "    if torch.cuda.is_available():\n" port)
                    (display "        torch.cuda.manual_seed(seed)\n" port)
                    (display "        torch.cuda.manual_seed_all(seed)\n" port)
                    (display "    if torch.backends.cudnn.is_available():\n" port)
                    (display "        torch.backends.cudnn.deterministic = True\n" port)
                    (display "        torch.backends.cudnn.benchmark = False\n" port)
                    (display "\n__all__ = [\"autocast_exclude_mps\", \"set_seed\"]\n"
                             port)))))
            (add-after 'unpack 'avoid-audiotools-import
              (lambda _
                (substitute* "fish_speech/models/dac/modded_dac.py"
                  (("from audiotools import AudioSignal\n") "")
                  (("from audiotools\\.ml import BaseModel\n") "")
                  (("from torch import Tensor, nn\n")
                   (string-append
                    "from torch import Tensor, nn\n\n"
                    "class BaseModel(nn.Module):\n"
                    "    @property\n"
                    "    def device(self):\n"
                    "        return next(self.parameters()).device\n\n")))))
            (add-after 'unpack 'drop-project-root-probing
              (lambda _
                (substitute* '("tools/run_webui.py"
                               "fish_speech/models/dac/inference.py")
                  (("import pyrootutils\n") "")
                  (("pyrootutils.setup_root\\(__file__, indicator=\"\\.project-root\", pythonpath=True\\)\n")
                   ""))))
            (add-after 'unpack 'fail-fast-on-missing-checkpoints
              (lambda _
                (substitute* "fish_speech/models/text2semantic/inference.py"
                  (("    logger.info\\(\"Loading model \\.\\.\\.\"\\)\n")
                   (string-append
                    "    if not (checkpoint_path / \"config.json\").exists():\n"
                    "        raise FileNotFoundError(\n"
                    "            f\"Missing Fish Speech config: {checkpoint_path / 'config.json'}\"\n"
                    "        )\n"
                    "    if (output or prompt_audio) and not (checkpoint_path / \"codec.pth\").exists():\n"
                    "        raise FileNotFoundError(\n"
                    "            f\"Missing Fish Speech codec checkpoint: {checkpoint_path / 'codec.pth'}\"\n"
                    "        )\n"
                    "    logger.info(\"Loading model ...\")\n")))
                (substitute* "tools/run_webui.py"
                  (("    args.precision = torch.half if args.half else torch.bfloat16\n")
                   (string-append
                    "    if not (args.llama_checkpoint_path / \"config.json\").exists():\n"
                    "        raise FileNotFoundError(\n"
                    "            f\"Missing Fish Speech config: {args.llama_checkpoint_path / 'config.json'}\"\n"
                    "        )\n"
                    "    if not args.decoder_checkpoint_path.exists():\n"
                    "        raise FileNotFoundError(\n"
                    "            f\"Missing Fish Speech codec checkpoint: {args.decoder_checkpoint_path}\"\n"
                    "        )\n"
                    "    args.precision = torch.half if args.half else torch.bfloat16\n")))))
            (add-after 'install 'install-data-files
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (site (car (find-files (string-append out "/lib")
                                              "site-packages$"
                                              #:directories? #t)))
                       (fish-site (string-append site "/fish_speech")))
                  (copy-recursively "fish_speech/configs"
                                    (string-append fish-site "/configs"))
                  (copy-recursively "fish_speech/i18n/locale"
                                    (string-append fish-site "/i18n/locale"))
                  (copy-recursively "fish_speech/datasets/protos"
                                    (string-append fish-site
                                                   "/datasets/protos")))))
            (add-after 'create-entrypoints 'install-command-wrappers
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (python (which "python"))
                       (certs #$(file-append
                                  nss-certs-for-test
                                  "/etc/ssl/certs/ca-certificates.crt")))
                  (mkdir-p bin)
                  (for-each
                   (lambda (script)
                     (let ((path (string-append bin "/" (car script)))
                           (module (cdr script)))
                       (call-with-output-file path
                         (lambda (port)
                           (format port "#!~a\n" python)
                           (format port "import os\n")
                           (format port
                                   "os.environ.setdefault('SSL_CERT_FILE', '~a')\n"
                                   certs)
                           (format port
                                   "os.environ.setdefault('REQUESTS_CA_BUNDLE', os.environ['SSL_CERT_FILE'])\n")
                           (format port
                                   "os.environ.setdefault('CURL_CA_BUNDLE', os.environ['SSL_CERT_FILE'])\n")
                           (format port "import runpy\n")
                           (format port "runpy.run_module('~a', run_name='__main__')\n"
                                   module)))
                       (chmod path #o755)))
                   '(("fish-speech-webui" . "tools.run_webui")
                     ("fish-speech-tts" .
                      "fish_speech.models.text2semantic.inference"))))))
            (add-before 'sanity-check 'set-cache-dirs
              (lambda _
                (setenv "HOME" "/tmp")
                (setenv "XDG_CACHE_HOME" "/tmp/.cache")
                (setenv "HF_HOME" "/tmp/.cache/huggingface")
                (setenv "TORCH_HOME" "/tmp/.cache/torch")
                (let ((certs #$(file-append
                                nss-certs-for-test
                                "/etc/ssl/certs/ca-certificates.crt")))
                  (setenv "SSL_CERT_FILE" certs)
                  (setenv "REQUESTS_CA_BUNDLE" certs)
                  (setenv "CURL_CA_BUNDLE" certs))
                (mkdir-p "/tmp/.cache/huggingface/hub")
                (mkdir-p "/tmp/.cache/torch")))
            (replace 'sanity-check
              (lambda _
                (invoke "python" "-c"
                        (string-append
                         "import importlib, pkgutil\n"
                         "import fish_speech, tools\n"
                         "for root in (fish_speech, tools):\n"
                         "    for mod in pkgutil.walk_packages("
                         "root.__path__, root.__name__ + '.'):\n"
                         "        importlib.import_module(mod.name)\n")))))))
      (propagated-inputs (list python-click
                               python-descript-audio-codec
                               python-einops
                               python-einx
                               python-gradio
                               python-hydra-core
                               python-huggingface-hub
                               python-librosa
                               python-loguru
                               python-loralib
                               python-natsort
                               python-numpy
                               python-pydantic
                               python-pyrootutils
                               python-pyyaml
                               python-rich
                               python-safetensors
                               python-soundfile
                               python-tiktoken
                               python-pytorch
                               python-torchaudio
                               python-tqdm
                               python-transformers
                               python-typing-extensions))
      (native-inputs (list python-setuptools python-setuptools-scm python-wheel))
      (home-page "https://github.com/fishaudio/fish-speech")
      (synopsis "Fish Speech S2 text-to-speech inference and WebUI")
      (description
       "Fish Speech provides Fish Audio S2 text-to-speech inference, voice
cloning, and a Gradio WebUI.  This package installs the Python inference
library and command wrappers; model weights are downloaded or mounted
separately, typically under @file{checkpoints/s2-pro}.")
      (license (myguix-license:nonfree "file://LICENSE")))))

(define-public python-fish-speech-cuda
  (package
    (inherit python-fish-speech)
    (name "python-fish-speech-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-fish-speech)
                         (replace "python-descript-audio-codec"
                                  python-descript-audio-codec-cuda)
                         (replace "python-loralib" python-loralib-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchaudio" python-torchaudio-cuda)
                         (replace "python-transformers"
                                  python-transformers-cuda)
                         (replace "python-safetensors"
                                  python-safetensors-cuda)))
    (synopsis "Fish Speech S2 TTS inference and WebUI with CUDA support")
    (description
     "Fish Speech provides Fish Audio S2 text-to-speech inference, voice
cloning, and a Gradio WebUI.  This variant uses CUDA-enabled PyTorch,
TorchAudio, Transformers, Safetensors, LoRA, and DAC packages; model weights
are downloaded or mounted separately, typically under @file{checkpoints/s2-pro}.")))

(define-public python-pyloudnorm
  (package
    (name "python-pyloudnorm")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyloudnorm" version))
       (sha256
        (base32 "07hynwk375n5vfk3dr43x2y9301i5p88x80f2sapfkpaglclxkb3"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'delete-pyproject-toml
                    (lambda _
                      ;; Delete conflicting pyproject.toml to let setup.py handle everything
                      (delete-file "pyproject.toml"))))))
    (propagated-inputs (list python-future python-numpy python-scipy))
    (native-inputs (list python-attrs python-setuptools python-wheel))
    (home-page "https://github.com/csteinmetz1/pyloudnorm")
    (synopsis
     "Implementation of ITU-R BS.1770-4 loudness algorithm in Python.")
    (description
     "Implementation of ITU-R BS.1770-4 loudness algorithm in Python.")
    (license license:expat)))

(define-public python-pyrubberband
  (package
    (name "python-pyrubberband")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyrubberband" version))
       (sha256
        (base32 "1xs9mj4467yl6pai381xv37jz9h8bjma9nfj9bl38qkcrb47ww3l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;Tests require rubberband CLI tool
    (propagated-inputs (list python-numpy python-soundfile))
    (native-inputs (list python-pytest python-pytest-cov python-setuptools
                         python-wheel))
    (home-page "http://github.com/bmcfee/pyrubberband")
    (synopsis "Python module to wrap rubberband")
    (description "Python module to wrap rubberband.")
    (license license:isc)))

(define-public python-voxcpm
  (package
    (name "python-voxcpm")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenBMB/VoxCPM")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06848b4vj56iwpd2b3z6w8sir1rym0x60jwwskp52rg9vm51zfc6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Tests do not cover model inference and some require assets.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-uninstalled-webapp-dependencies
            (lambda _
              ;; The PyPI metadata lists dependencies for the repository-root
              ;; Gradio/Hugging Face Space apps.  Those files are not installed
              ;; by this package; keep the packaged dependency set focused on
              ;; the CLI/library inference path.
              (substitute* "pyproject.toml"
                (("    \"gradio>=6,<7\",\\n") "")
                (("    \"funasr\",\\n") "")
                (("    \"spaces\",\\n") ""))))
          (add-after 'unpack 'disable-audiovae-jit-snake
            (lambda _
              ;; On Guix's PyTorch build the scripted Snake activation can fail
              ;; at runtime with a missing TorchScript static op.  Eager mode is
              ;; slower, but it keeps the AudioVAE encode/decode path reliable.
              (substitute* '("src/voxcpm/modules/audiovae/audio_vae.py"
                             "src/voxcpm/modules/audiovae/audio_vae_v2.py")
                (("# Scripting this brings model speed up 1\\.4x")
                 "# Run Snake eagerly for compatibility with this PyTorch build.")
                (("^@torch\\.jit\\.script") ""))))
          (add-after 'unpack 'default-to-non-jit-inference
            (lambda _
              ;; VoxCPM's optimization path is opportunistic torch.compile/JIT
              ;; work.  On this PyTorch build it can fail before inference, so
              ;; keep the packaged CLI and library defaults on the eager path.
              (substitute* '("src/voxcpm/core.py"
                             "src/voxcpm/model/voxcpm.py"
                             "src/voxcpm/model/voxcpm2.py")
                (("optimize: bool = True,")
                 "optimize: bool = False,")
                (("True by default, but can be disabled for debugging\\.")
                 "False by default in this package; enable only for debugging."))
              (substitute* "src/voxcpm/cli.py"
                (("        \"--no-optimize\",")
                 (string-append
                  "        \"--optimize\",\n"
                  "        dest=\"no_optimize\",\n"
                  "        action=\"store_false\",\n"
                  "        help=\"Enable model optimization during loading\",\n"
                  "    )\n"
                  "    parser.add_argument(\n"
                  "        \"--no-optimize\","))
                (("        help=\"Disable model optimization during loading\",")
                 (string-append
                  "        dest=\"no_optimize\",\n"
                  "        default=True,\n"
                  "        help=\"Disable model optimization during loading (default)\",")))))
          (add-after 'unpack 'default-zipenhancer-when-denoising
            (lambda _
              ;; Upstream's CLI overrides the library's ZipEnhancer default
              ;; with None unless --zipenhancer-path is supplied explicitly.
              ;; Make --denoise fire-and-forget while keeping ordinary runs
              ;; from downloading/loading the denoiser unnecessarily.
              (substitute* "src/voxcpm/cli.py"
                (("    # Build LoRA config if provided\n")
                 (string-append
                  "    if args.denoise and zipenhancer_path is None:\n"
                  "        zipenhancer_path = \"iic/speech_zipenhancer_ans_multiloss_16k_base\"\n"
                  "\n"
                  "    # Build LoRA config if provided\n")))))
          (add-after 'unpack 'set-certificate-default
            (lambda _
              ;; The model download paths import Requests during module import.
              ;; Guix build sandboxes do not expose /run/current-system, so use a
              ;; package-local CA bundle while still respecting user overrides.
              (let ((certs #$(file-append
                              nss-certs-for-test
                              "/etc/ssl/certs/ca-certificates.crt")))
                (substitute* '("src/voxcpm/core.py"
                               "src/voxcpm/zipenhancer.py")
                  (("import os\n")
                   (string-append
                    "import os\n"
                    "os.environ.setdefault(\"SSL_CERT_FILE\", \""
                    certs "\")\n"
                    "os.environ.setdefault(\"REQUESTS_CA_BUNDLE\", "
                    "os.environ[\"SSL_CERT_FILE\"])\n"
                    "os.environ.setdefault(\"CURL_CA_BUNDLE\", "
                    "os.environ[\"SSL_CERT_FILE\"])\n"))))))
          (add-before 'sanity-check 'set-cache-dirs
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "XDG_CACHE_HOME" "/tmp/.cache")
              (setenv "HF_HOME" "/tmp/.cache/huggingface")
              (setenv "TORCH_HOME" "/tmp/.cache/torch")
              (mkdir-p "/tmp/.cache/huggingface/hub")
              (mkdir-p "/tmp/.cache/torch/hub")))
          (replace 'sanity-check
            (lambda _
              (invoke "python" "-c"
                      (string-append
                       "import voxcpm, voxcpm.cli; "
                       "from voxcpm import VoxCPM; "
                       "from voxcpm.zipenhancer import ZipEnhancer; "
                       "from voxcpm.utils.text_normalize import TextNormalizer")))))))
    (propagated-inputs (list python-addict
                             python-argbind
                             python-datasets
                             python-einops
                             python-huggingface-hub
                             python-inflect
                             python-librosa
                             python-matplotlib
                             python-modelscope
                             python-numpy
                             python-pydantic
                             python-regex
                             python-safetensors
                             python-simplejson
                             python-sortedcontainers
                             python-soundfile
                             python-pytorch
                             python-torchaudio
                             python-torchcodec
                             python-tqdm
                             python-transformers
                             python-wetext))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/OpenBMB/VoxCPM")
    (synopsis "Context-aware TTS and voice cloning")
    (description
     "VoxCPM provides tokenizer-free text-to-speech models for context-aware
speech generation and voice cloning.  This package installs the VoxCPM Python
library and command-line interface; model weights are downloaded separately from
Hugging Face or supplied from a local model directory.")
    (license license:asl2.0)))

(define-public python-voxcpm-cuda
  (package
    (inherit python-voxcpm)
    (name "python-voxcpm-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-voxcpm)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchaudio" python-torchaudio-cuda)
                         (replace "python-torchcodec" python-torchcodec-cuda)
                         (replace "python-transformers"
                                  python-transformers-cuda)
                         (replace "python-safetensors"
                                  python-safetensors-cuda)))
    (synopsis "VoxCPM TTS and voice cloning with CUDA support")
    (description
     "VoxCPM provides tokenizer-free text-to-speech models for context-aware
speech generation and voice cloning.  This variant uses CUDA-enabled PyTorch,
TorchAudio, TorchCodec, Transformers, and Safetensors packages.")))

(define-public python-resemble-perth
  (package
    (name "python-resemble-perth")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "resemble_perth" version))
       (sha256
        (base32 "14i3mlrjjjkrsv7zmyrppjfc2jkfqar7x63vav72lyz04by8qs74"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'disable-numba-cache
            (lambda _
              ;; Disable numba JIT caching to avoid issues with read-only store paths
              (setenv "NUMBA_CACHE_DIR" "/tmp")))
          (add-before 'sanity-check 'disable-numba-cache-for-sanity
            (lambda _
              ;; Also disable for sanity check
              (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (propagated-inputs (list python-bitstring
                             python-librosa
                             python-matplotlib
                             python-numpy
                             python-pandas
                             python-pillow
                             python-praat-parselmouth
                             python-pydub
                             python-pyloudnorm
                             python-pyrubberband
                             python-pywavelets
                             python-pyyaml
                             python-scikit-learn
                             python-soundfile
                             python-sox
                             python-tabulate
                             python-tensorboard
                             python-pytorch
                             python-torchaudio
                             python-tqdm))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/resemble-ai/Perth")
    (synopsis "Audio Watermarking and Detection Library")
    (description "Audio Watermarking and Detection Library.")
    (license license:expat)))

(define-public python-chatterbox-tts
  (package
    (name "python-chatterbox-tts")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "chatterbox_tts" version))
       (sha256
        (base32 "05h2fxwa0ng53wdgf9lghzfmjb95m61fn4g6zxgsxfjpkxcmfbvf"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;No tests included
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (propagated-inputs (list python-conformer
                             python-diffusers
                             python-gradio
                             python-librosa
                             python-numpy
                             python-pkuseg
                             python-pykakasi
                             python-resemble-perth
                             python-s3tokenizer
                             python-safetensors
                             python-pytorch
                             python-torchaudio
                             python-transformers))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/resemble-ai/chatterbox")
    (synopsis
     "Chatterbox: Open Source TTS and Voice Conversion by Resemble AI")
    (description
     "Chatterbox: Open Source TTS and Voice Conversion by Resemble AI.")
    (license license:expat)))

(define-public python-chatterbox-tts-cuda
  (package
    (inherit python-chatterbox-tts)
    (name "python-chatterbox-tts-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-chatterbox-tts)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchaudio" python-torchaudio-cuda)
                         (replace "python-conformer" python-conformer-cuda)))
    (synopsis "Chatterbox TTS with CUDA support")
    (description
     "Chatterbox: Open Source TTS and Voice Conversion by Resemble AI, with CUDA acceleration.")))
