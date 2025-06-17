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
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs))

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
    (propagated-inputs
     (list python-tiktoken
           python-click
           python-jinja2
           python-certifi
           python-pyyaml
           python-pydantic))
    (native-inputs
     (list python-poetry-core))
    (home-page "https://github.com/BerriAI/litellm")
    (synopsis "Library to simplify calling 100+ LLM APIs using the OpenAI format")
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
    (propagated-inputs
     (list python-numpy
           python-typing-extensions
           python-jinja2))
    (native-inputs
     (list cmake
           pkg-config
           python-scikit-build-core
           python-setuptools
           python-wheel))
    (inputs
     (list openblas))
    (home-page "https://github.com/abetlen/llama-cpp-python")
    (synopsis "Python bindings for llama.cpp")
    (description
     "Python bindings for llama.cpp, enabling use of Llama and other large
language models with GPU acceleration via various backends including CUDA,
OpenCL, Metal, and CPU-based inference.")
    (license license:expat)))
