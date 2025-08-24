(define-module (myguix packages huggingface)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
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
  #:use-module ((gnu packages machine-learning)
                #:hide (python-safetensors))
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages python-pqrs)
  #:use-module ((myguix packages rust-crates-pqrs)
                #:select (lookup-myguix-cargo-inputs))
  #:use-module ((myguix utils)
                #:select (myguix-cargo-inputs))
  #:use-module (myguix packages machine-learning))

;; Helper function to use myguix cargo inputs
(define (myguix-cargo-inputs name)
  "Lookup Cargo inputs for NAME from myguix rust-crates-pqrs."
  (or (lookup-myguix-cargo-inputs name)
      (begin
        (format (current-error-port)
                "Warning: no Cargo inputs available for '~a'~%" name)
        '())))

(define-public python-huggingface-hub
  (package
    (name "python-huggingface-hub")
    (version "0.32.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huggingface/huggingface_hub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c1579gz5l9mvb6vib3p4dy4k6m616s528aka37pzszy6j8nj41r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list
                     "--ignore=contrib/sentence_transformers/test_sentence_transformers.py"
                     "--ignore=tests/test_command_delete_cache.py"
                     "--ignore=contrib/timm/test_timm.py"
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
                     ;; These fail due to TLS certificate issues
                     "--ignore=tests/test_inference_providers.py"
                     "--ignore=tests/test_oauth.py"
                     "--ignore=tests/test_xet_download.py"
                     "--ignore=tests/test_xet_upload.py"
                     "-k"
                     (string-append "not test_push_to_hub"
                      " and not test_from_pretrained_model_id_only"
                      " and not test_from_pretrained_model_id_and_revision"))
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'remove-hf-xet-requirement
                    (lambda _
                      (substitute* "setup.py"
                        ;; Remove hf-xet from requirements
                        ((".*hf-xet.*")
                         ""))))
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
                       'install)))))
    (inputs (cons maturin
                  (myguix-cargo-inputs 'python-safetensors)))
    (native-inputs (list python-wrapper))
    (home-page "https://github.com/huggingface/safetensors")
    (synopsis "Safely store tensors")
    (description
     "This repository implements a new simple format for storing tensors safely (as opposed to pickle) and that is still fast (zero-copy).")
    (license license:asl2.0)))

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
    (native-inputs (list python-bitsandbytes-cuda
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

(define-public python-optimum
  (package
    (name "python-optimum")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "optimum" version))
       (sha256
        (base32 "0l852l8d50p82zmqfvhihsrs221ip43zy7mv1qvp7fvqpyps40mm"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs (list python-huggingface-hub python-numpy
                             python-packaging python-pytorch-cuda
                             python-transformers))
    (native-inputs (list python-black
                         python-einops
                         python-parameterized
                         python-pillow
                         python-pytest
                         python-pytest-xdist
                         python-requests
                         python-scikit-learn
                         python-sentencepiece
                         python-setuptools
                         python-torchvision-cuda
                         python-wheel))
    (home-page "https://github.com/huggingface/optimum")
    (synopsis
     "Optimum Library is an extension of the Hugging Face Transformers library, providing a framework to integrate third-party libraries from Hardware Partners and interface with their specific functionality.")
    (description
     "Optimum Library is an extension of the Hugging Face Transformers library,
providing a framework to integrate third-party libraries from Hardware Partners
and interface with their specific functionality.")
    (license license:asl2.0)))

(define-public python-timm
  (package
    (name "python-timm")
    (version "1.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "timm" version))
       (sha256
        (base32 "0m8hkf3p8s7r6vjnpnxagrhh9ndfkmarp2k0dq2mymln1k1knskm"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs (list python-huggingface-hub python-pyyaml
                             python-safetensors python-pytorch-cuda
                             python-torchvision-cuda))
    (native-inputs (list python-pdm-backend))
    (home-page "https://github.com/huggingface/pytorch-image-models")
    (synopsis "PyTorch Image Models")
    (description "@code{PyTorch} Image Models.")
    (license license:asl2.0)))

(define-public python-peft
  (package
    (name "python-peft")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "peft" version))
       (sha256
        (base32 "1p3jmmv2qgzdhzhxgramc2p1gjcbh7nn2cixb9qyzxa2gfpnjval"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (propagated-inputs (list python-accelerate
                             python-huggingface-hub
                             python-numpy
                             python-packaging
                             python-psutil
                             python-pyyaml
                             python-safetensors
                             python-pytorch-cuda
                             python-tqdm
                             python-transformers))
    (native-inputs (list python-black
                         python-datasets
                         python-diffusers
                         python-parameterized
                         python-protobuf
                         python-pytest
                         python-pytest-cov
                         python-pytest-xdist
                         python-scipy
                         python-sentencepiece
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/huggingface/peft")
    (synopsis "Parameter-Efficient Fine-Tuning (PEFT)")
    (description "Parameter-Efficient Fine-Tuning (PEFT).")
    (license license:asl2.0)))
