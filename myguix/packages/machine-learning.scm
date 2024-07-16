(define-module (myguix packages machine-learning)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages python)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages rust-apps)
  #:use-module (myguix packages rust-pqrs)
  #:use-module (myguix packages cuda)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo))

(define-public python-safetensors
  (package
    (name "python-safetensors")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/safetensors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ck97wnhi53j1qbcl0y9vynn42zmxqic6yqpn2cl9shxbnsiks5"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'chdir
                     (lambda* _
                       (chdir "bindings/python")))
                   (add-after 'prepare-python-module 'build-python-module
                     (assoc-ref py:%standard-phases
                                'build))
                   (add-after 'build-python-module 'install-python-module
                     (assoc-ref py:%standard-phases
                                'install)))
      #:cargo-inputs `(("rust-pyo3" ,rust-pyo3-0.21)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-memmap2" ,rust-memmap2-0.9))))
    (inputs (list maturin))
    (native-inputs (list python-wrapper))
    (home-page "https://github.com/huggingface/safetensors")
    (synopsis "Safely store tensors")
    (description
     "This repository implements a new simple format for storing tensors safely (as opposed to pickle) and that is still fast (zero-copy).")
    (license license:expat)))

(define-public python-tokenizers
  (package
    (name "python-tokenizers")
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/tokenizers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s8jccf3a0bqphrrw5762gg1nx1ajbawywsvvfy6fxlwisvh18dh"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cargo-build-system-modules ,@%pyproject-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix py:)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'chdir
                     (lambda* _
                       (delete-file "bindings/python/.cargo/config.toml")
                       (chdir "bindings/python")))
                   (add-after 'chdir 'version-tokenizers
                     (lambda* _
                       (substitute* "Cargo.toml"
                         (("^\\[dependencies.tokenizers\\].*$" all)
                          (string-append all "version = \"0.19.1\"\n")))))
                   (add-after 'prepare-python-module 'build-python-module
                     (assoc-ref py:%standard-phases
                                'build))
                   (add-after 'build-python-module 'install-python-module
                     (assoc-ref py:%standard-phases
                                'install)))
      #:cargo-inputs `(("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-pyo3" ,rust-pyo3-0.21)
                       ("rust-numpy" ,rust-numpy-0.21)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-onig" ,rust-onig-6)
                       ("rust-itertools" ,rust-itertools-0.12))
      #:cargo-development-inputs `(("rust-pyo3" ,rust-pyo3-0.21)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokenizers" ,rust-tokenizers-0.19))))
    (inputs (list maturin))
    (native-inputs (list python-wrapper))
    (home-page "https://github.com/huggingface/tokenizers")
    (synopsis "Today's most used tokenizers")
    (description
     "Provides an implementation of today's most used tokenizers, with a focus on performance and versatility.")
    (license license:expat)))

(define-public gloo-cuda-20240626
  (let ((version "0.0.0")
        (commit "81925d1c674c34f0dc34dd9a0f2151c1b6f701eb")
        (revision "20240626"))
    (package
      (name "gloo-cuda")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16zs8ndbiv9nppn8bv6lfanzyyssz7g5pawxiqcnafwq3nvxpj9m"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:configure-flags #~'("-DBUILD_SHARED_LIBS=ON" "-DBUILD_TEST=OFF"
                              "-DUSE_CUDA=ON")
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'drop-unsupported-arch
                       (lambda _
                         (substitute* "cmake/Cuda.cmake"
                           (("gloo_known_gpu_archs \"[^\"]+\"")
                            "gloo_known_gpu_archs \"35 50 52 60 61 70 75\"")))))))
      (inputs (list cuda-toolkit-12.1 openssl))
      (native-inputs (list googletest))
      (synopsis "Collective communications library")
      (description
       "Gloo is a collective communications library.  It comes with a
number of collective algorithms useful for machine learning applications.
These include a barrier, broadcast, and allreduce.")
      (home-page "https://github.com/facebookincubator/gloo")
      (license license:bsd-3))))

(define-public tensorpipe-cuda-12.1
  (package
    (inherit tensorpipe)
    (name "tensorpipe-cuda")
    (arguments (substitute-keyword-arguments (package-arguments tensorpipe)
                 ((#:configure-flags flags #~'())
                  #~(cons "-DTP_USE_CUDA=ON" #$flags))))
    (inputs (modify-inputs (package-inputs tensorpipe)
              (append cuda-toolkit-12.1)))))