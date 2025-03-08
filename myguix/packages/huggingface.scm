(define-module (myguix packages huggingface)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-web)
                #:hide (python-huggingface-hub))
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages rust-pqrs)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages nlp))

(define-public python-huggingface-hub
  (package
    (name "python-huggingface-hub")
    (version "0.29.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/huggingface_hub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0067m4vgmcmfdcbfwcbhy4y5w91kx00yv303ci3wpz7rqnmnhvpl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; We don't have sentence_transformers...
      '(list
        "--ignore=contrib/sentence_transformers/test_sentence_transformers.py"
        ;; ...nor do we have InquirerPy...
        "--ignore=tests/test_command_delete_cache.py"
        ;; ...or timm...
        "--ignore=contrib/timm/test_timm.py"
        ;; ...or spacy_huggingface_hub
        "--ignore=contrib/spacy/test_spacy.py"
        ;; These all require internet access
        "--ignore=tests/test_cache_no_symlinks.py"
        "--ignore=tests/test_cache_layout.py"
        "--ignore=tests/test_commit_scheduler.py"
        "--ignore=tests/test_file_download.py"
        "--ignore=tests/test_hf_api.py"
        "--ignore=tests/test_hf_file_system.py"
        "--ignore=tests/test_inference_api.py"
        "--ignore=tests/test_inference_async_client.py"
        "--ignore=tests/test_inference_client.py"
        "--ignore=tests/test_inference_text_generation.py"
        "--ignore=tests/test_login_utils.py"
        "--ignore=tests/test_offline_utils.py"
        "--ignore=tests/test_repocard.py"
        "--ignore=tests/test_repository.py"
        "--ignore=tests/test_snapshot_download.py"
        "--ignore=tests/test_utils_cache.py"
        "--ignore=tests/test_utils_git_credentials.py"
        "--ignore=tests/test_utils_http.py"
        "--ignore=tests/test_utils_pagination.py"
        "--ignore=tests/test_webhooks_server.py"
        "--ignore=tests/test_utils_sha.py"
        "--ignore=tests/test_auth.py"
        "--ignore=tests/test_auth_cli.py"
        "-k"
        (string-append "not test_push_to_hub"
                       " and not test_from_pretrained_model_id_only"
                       " and not test_from_pretrained_model_id_and_revision"))
      #:phases '(modify-phases %standard-phases
                  (add-before 'check 'pre-check
                    ;; Some tests need to write to HOME.
                    (lambda _
                      (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-filelock
                             python-fsspec
                             python-packaging
                             python-pyyaml
                             python-requests
                             python-tqdm
                             python-typing-extensions))
    (native-inputs (list python-aiohttp
                         python-fastapi
                         python-jedi
                         python-jinja2
                         python-mypy
                         python-numpy
                         python-pillow
                         python-pydantic
                         python-pytest
                         python-pytest-mock
                         python-pytest-asyncio
                         python-pytest-cov
                         python-pytest-env
                         python-pytest-rerunfailures
                         python-pytest-vcr
                         python-pytest-xdist
                         python-setuptools
                         python-types-requests
                         python-types-toml
                         python-types-urllib3
                         python-typing-extensions
                         python-urllib3
                         python-wheel))
    (home-page "https://github.com/huggingface/huggingface_hub")
    (synopsis "Client library for accessing the huggingface.co hub")
    (description
     "This package provides a client library to download and publish models,
datasets and other repos on the @url{huggingface.co} hub.")
    (license license:asl2.0)))

(define-public python-safetensors
  (package
    (name "python-safetensors")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/safetensors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05kafi6pspi11iwwsfwl2yi68j9pbjn2vhpj6mbvb66jwlq47c71"))))
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
          (add-after 'unpack 'chdir
            (lambda* _
              (chdir "bindings/python")))
          (add-after 'chdir 'version-tokenizers
            (lambda* _
              (substitute* "Cargo.toml"
                (("^\\[dependencies.safetensors\\].*$" all)
                 (string-append all "version = \"0.5.3\"\n")))))
          (add-after 'build 'build-python-module
            (assoc-ref py:%standard-phases
                       'build))
          (add-after 'build-python-module 'install-python-module
            (assoc-ref py:%standard-phases
                       'install)))
      #:cargo-inputs `(("rust-pyo3" ,rust-pyo3-0.23)
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
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/tokenizers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ns9byxl149r4jawsdlj2g99aardyzmd71syy1769bgb64mgbhv"))))
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
          (add-after 'unpack 'chdir
            (lambda* _
              (delete-file "bindings/python/.cargo/config.toml")
              (chdir "bindings/python")))
          (add-after 'chdir 'version-tokenizers
            (lambda* _
              (substitute* "Cargo.toml"
                (("^\\[dependencies.tokenizers\\].*$" all)
                 (string-append all "version = \"0.21.0\"\n")))))
          (add-after 'build 'build-python-module
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
                       ("rust-pyo3" ,rust-pyo3-0.22)
                       ("rust-numpy" ,rust-numpy-0.22)
                       ("rust-ndarray" ,rust-ndarray-0.15)
                       ("rust-onig" ,rust-onig-6)
                       ("rust-itertools" ,rust-itertools-0.12))
      #:cargo-development-inputs `(("rust-pyo3" ,rust-pyo3-0.22)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokenizers" ,rust-tokenizers-0.21))))
    (inputs (list maturin))
    (native-inputs (list python-wrapper))
    (home-page "https://github.com/huggingface/tokenizers")
    (synopsis "Today's most used tokenizers")
    (description
     "Provides an implementation of today's most used tokenizers, with a focus on performance and versatility.")
    (license license:expat)))

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

(define python-requests-for-datasets
  (package
    (inherit python-requests)
    (name "python-requests")
    (version "2.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests" version))
       (sha256
        (base32 "129j8gidirf8ycpfg6l3v90snpa9fyx061xl4smb7qzkxksiz5fx"))))
    (native-inputs (modify-inputs (package-native-inputs python-requests)
                     (prepend nss-certs-for-test)))))

(define-public python-datasets
  (package
    (name "python-datasets")
    (version "3.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/datasets")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r6rnrsk7cwcrc1ixiqajrj1mnpka0p8hndz8gapzy2dijnxkj5i"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Disable the sanity check, which fails with the following error:
                  ;;
                  ;; File "/gnu/store/...-python-requests-2.32.2/lib/python3.10/site-packages/requests/adapters.py", line 77, in <module>
                  ;; _preloaded_ssl_context.load_verify_locations(
                  ;; FileNotFoundError: [Errno 2] No such file or directory
                  (delete 'sanity-check))))
    (propagated-inputs (list python-aiohttp
                             python-dill
                             python-filelock
                             python-fsspec
                             python-huggingface-hub
                             python-multiprocess
                             python-numpy
                             python-packaging
                             python-pandas
                             python-pyarrow
                             python-pyyaml
                             python-requests-for-datasets
                             python-setuptools
                             python-tqdm
                             python-xxhash))
    (native-inputs (list python-librosa python-pillow python-soundfile
                         python-soxr python-huggingface-hub))
    (home-page "https://github.com/huggingface/datasets")
    (synopsis "HuggingFace community-driven open-source library of datasets")
    (description
     "@code{HuggingFace} community-driven open-source library of datasets.")
    (license license:asl2.0)))

(define-public python-diffusers
  (package
    (name "python-diffusers")
    (version "0.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "diffusers" version))
       (sha256
        (base32 "0c52xaqr05rkg7l2d8fkrvknr9zpr8kcdxwsf9shdfxa4srkc7pb"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Disable the sanity check, which fails with the following error:
                  ;;
                  ;; File "/gnu/store/...-python-diffusers-0.32.2/lib/python3.10/site-packages/diffusers/commands/fp16_safetensors.py", line 27, in <module>
                  ;; import torch
                  ;; ModuleNotFoundError: No module named 'torch'
                  (delete 'sanity-check))))
    (propagated-inputs (list python-filelock
                             python-huggingface-hub
                             python-importlib-metadata
                             python-numpy
                             python-pillow
                             python-regex
                             python-requests
                             python-safetensors))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/huggingface/diffusers")
    (synopsis "State-of-the-art diffusion in PyTorch and JAX.")
    (description "State-of-the-art diffusion in @code{PyTorch} and JAX.")
    (license license:asl2.0)))

(define-public python-transformers
  (package
    (name "python-transformers")
    (version "4.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transformers" version))
       (sha256
        (base32 "17h8sbdkq504zkykmgdgpa10qrp3vkdmlprzfi43zp5qnm0fch3y"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))
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
    (native-inputs (list python-setuptools python-wheel python-pytorch-cuda
                         python-rich))
    (home-page "https://github.com/huggingface/transformers")
    (synopsis
     "State-of-the-art Machine Learning for JAX, PyTorch and TensorFlow")
    (description
     "State-of-the-art Machine Learning for JAX, @code{PyTorch} and @code{TensorFlow}.")
    (license #f)))
