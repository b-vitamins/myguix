(define-module (myguix packages llm)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (myguix packages cuda)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config))

(define-public llama-cpp
  (let ((commit "e02b597be3702174e7b47b44cd03e1da1553284b")
        (revision "4"))
    (package
      (name "llama-cpp")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ggerganov/llama.cpp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kz6w6faii7q6cmmvqykb9dsxclci4wxh8jmwv64h8y5lw11qnaj"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON" "-DLLAMA_CUDA=ON")
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'disable-unrunable-tests
                       (lambda _
                         (substitute* '("examples/eval-callback/CMakeLists.txt")
                           (("add_test")
                            "#add_test"))
                         (substitute* '("examples/eval-callback/CMakeLists.txt")
                           (("set_property")
                            "#set_property")))))))
      (inputs (list python nvidia-driver cuda-toolkit-12.1))
      (native-inputs (list pkg-config))
      (properties '((tunable? . #t)))
      (home-page "https://github.com/ggerganov/llama.cpp")
      (synopsis "Port of Facebook's LLaMA model in C/C++")
      (description
       "This package provides a port to Facebook's LLaMA collection
of foundation language models.  It requires models parameters to be downloaded
independently to be able to run a LLaMA model.")
      (license license:expat))))
