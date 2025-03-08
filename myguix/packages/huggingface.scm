(define-module (myguix packages huggingface)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graph)
  #:use-module ((gnu packages python-web)
                #:hide (python-huggingface-hub))
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
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
