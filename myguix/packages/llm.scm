(define-module (myguix packages llm)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages java)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages version-control)
  #:use-module (myguix build-system bazel)
  #:use-module (myguix packages bazel)
  #:use-module (myguix packages huggingface)
  #:use-module (srfi srfi-26))

;; TODO: Packages that need to be packaged for vLLM:
;; - python-ninja
;; - python-openai-harmony
;; - python-outlines-core

(define-public python-ray
  (package
    (name "python-ray")
    (version "2.49.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ray-project/ray")
             (commit (string-append "ray-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hghl14sv8kd4x4b0knq31y6yl2jj9r223q6b778rm02js7kxpsl"))
       (patches (map (lambda (patch)
                       (search-path (map (cut string-append <> "/myguix/patches")
                                         %load-path)
                                    patch))
                     '("ray-use-system-python.patch"
                       "ray-use-system-make.patch"
                       "ray-skip-bazel-when-no-targets.patch"
                       "ray-filter-missing-files.patch"
                       "ray-tolerate-missing-generated-dirs.patch")))))
    (build-system bazel-build-system)
    (arguments
     (list
      #:tests? #f
      #:bazel bazel
      ;; Be explicit about building the Cython extension and package zips
      ;; to ensure artifacts exist for setup.py when Bazel is skipped there.
      #:fetch-targets '(list
                        "//:_raylet"
                        "//:raylet_so_files"
                        "//:core_py_proto_zip"
                        "//:serve_py_proto_zip"
                        "//:ray_py_proto_zip"
                        "//:ray_pkg_zip"
                        "//:gen_ray_pkg"
                        "//cpp:gen_ray_cpp_pkg")
      #:build-targets '(list
                        "//:_raylet"
                        "//:raylet_so_files"
                        "//:core_py_proto_zip"
                        "//:serve_py_proto_zip"
                        "//:ray_py_proto_zip"
                        "//:ray_pkg_zip"
                        "//:gen_ray_pkg"
                        "//cpp:gen_ray_cpp_pkg")
      #:bazel-arguments '(list "--cxxopt=-Wno-dangling-reference")
      #:vendored-inputs-hash
      "1rf4slz8jgnwpsancvkvyz5dkivgqgl3lfj49zsyzc0wx3jsq6lr"
      #:modules '((myguix build bazel-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:imported-modules `(,@%bazel-build-system-modules
                           ,@%python-build-system-modules)
      #:bazel-configuration
      #~(let ((python-path (which "python3"))
              (sh-path (which "sh")))
          (setenv "PYTHON3_BIN_PATH" python-path)
          (setenv "PYTHON_BIN_PATH" python-path)
          (setenv "RAY_INSTALL_JAVA" "0")
          (setenv "SHELL" sh-path)
          (setenv "CONFIG_SHELL" sh-path))
      #:phases
      #~(modify-phases (@ (myguix build bazel-build-system) %standard-phases)
          ;; After Bazel build, extract generated Python proto files and
          ;; packaged binaries into the source tree's "python" dir so that
          ;; setup.py can find them when we skip Bazel in the Python build.
          (add-after 'build 'extract-bazel-zips-into-python
            (lambda _
              (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP") "/output"))
                     (bazel-bin "bazel-bin")
                     (z1 (string-append bazel-bin "/ray_py_proto.zip"))
                     (z2 (string-append bazel-bin "/ray_pkg.zip"))
                     (from-bazel-bin (append (if (file-exists? z1) (list z1) '())
                                             (if (file-exists? z2) (list z2) '())))
                     (zips (append from-bazel-bin
                                   (find-files out-dir "ray_py_proto\\.zip$")
                                   (find-files out-dir "ray_pkg\\.zip$")))
                     (dest "python"))
                (unless (pair? zips)
                  (error "Could not locate Bazel-generated Ray package zips"
                         out-dir))
                (for-each (lambda (zip)
                            (invoke "unzip" "-o" "-q" zip "-d" dest))
                          zips)
                ;; Ensure generated directories exist to satisfy setup.py even if empty.
                (mkdir-p (string-append "python/ray/core/generated"))
                (mkdir-p (string-append "python/ray/serve/generated"))
                ;; Ensure vendored thirdparty dirs exist if we skip vendoring.
                (mkdir-p (string-append "python/ray/thirdparty_files"))
                (mkdir-p (string-append "python/ray/_private/runtime_env/agent/thirdparty_files")))))
          ;; Some Bazel layouts may produce the _raylet.so in the output tree
          ;; without packaging it into ray_pkg.zip (or the zip may be missed).
          ;; As a fallback, locate the shared object and copy it into python/ray/.
          (add-after 'extract-bazel-zips-into-python 'ensure-raylet-extension
            (lambda _
              (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP") "/output"))
                     (bazel-ray-dir "bazel-bin/python/ray")
                     (bazel-so (string-append bazel-ray-dir "/_raylet.so"))
                     (found (find-files out-dir "_raylet\\.so$"))
                     (target "python/ray/_raylet.so"))
                ;; Prefer copying the entire bazel-bin/python/ray directory (brings .so and
                ;; any related built artifacts) and then fall back to copying a single .so.
                (when (and (file-exists? bazel-ray-dir) (not (file-exists? target)))
                  (mkdir-p "python/ray")
                  (copy-recursively bazel-ray-dir "python/ray"))
                (when (and (not (file-exists? target)) (file-exists? bazel-so))
                  (mkdir-p "python/ray")
                  (copy-file bazel-so target))
                (when (and (not (file-exists? target)) (pair? found))
                  (mkdir-p "python/ray")
                  (copy-file (car found) target))
                (unless (file-exists? target)
                  (error "_raylet.so not found after Bazel build; cannot proceed")))) )
          (add-after 'unpack 'disable-workspace-status
            (lambda _
              (substitute* ".bazelrc"
                (("build:linux --workspace_status_command=.*")
                 "# workspace_status_command disabled for Guix\n"))))
          (add-after 'unpack-vendored-inputs 'patch-foreign-cc-shebang
            (lambda _
              ;; Patch rules_foreign_cc to use bash directly instead of /usr/bin/env bash
              ;; The shebang() function is in foreign_cc/private/framework/toolchains/linux_commands.bzl
              (let* ((bash-path (which "bash"))
                     (sh-path (which "sh"))
                     ;; After unpack, we're in source dir; bazel output is ../output from source
                     (linux-cmds "../output/external/rules_foreign_cc/foreign_cc/private/framework/toolchains/linux_commands.bzl"))
                (when (file-exists? linux-cmds)
                  (substitute* linux-cmds
                    (("return \"#!/usr/bin/env bash\"")
                     (string-append "return \"#!" bash-path "\""))))
                ;; Patch configure scripts, shell scripts, and Makefiles in vendored dependencies
                (for-each (lambda (file)
                            (catch #t
                              (lambda ()
                                (substitute* file
                                  ;; Patch shebangs
                                  (("#!/bin/sh") (string-append "#!" sh-path))
                                  (("#! /bin/sh") (string-append "#! " sh-path))
                                  ;; Patch explicit /bin/sh invocations in configure scripts
                                  (("/bin/sh ") (string-append sh-path " "))
                                  ;; Patch Makefile.in SHELL variable (uses := syntax)
                                  (("^SHELL := /bin/sh") (string-append "SHELL := " sh-path))
                                  ;; Patch shell variable assignments: SHELL=/bin/sh
                                  (("=/bin/sh") (string-append "=" sh-path))
                                  ;; Patch parameter expansions: ${SHELL-/bin/sh}
                                  (("-/bin/sh") (string-append "-" sh-path))
                                  ;; Patch parameter expansions: ${SHELL:-/bin/sh}
                                  ((":-/bin/sh") (string-append ":-" sh-path))
                                  ;; Patch parameter expansions: ${SHELL:=/bin/sh}
                                  ((":=/bin/sh") (string-append ":=" sh-path))))
                              (lambda (key . args)
                                ;; Skip files that can't be decoded (binary files)
                                #f)))
                          (filter (lambda (f)
                                    (and (not (file-is-directory? f))
                                         (or (access? f X_OK)
                                             (string-suffix? "Makefile" f)
                                             (string-suffix? "Makefile.in" f)
                                             (string-suffix? ".mk" f))))
                                  (find-files "../output/external" ".*"))))))
          (add-after 'patch-foreign-cc-shebang 'patch-openssl-perl
            (lambda _
              ;; Patch openssl BUILD.bazel to use system perl instead of vendored perl
              (let ((openssl-build "../output/external/openssl/BUILD.bazel")
                    (openssl-config "../output/external/openssl/config"))
                (when (file-exists? openssl-build)
                  (substitute* openssl-build
                    ;; Remove perl toolchain dependency
                    ((".*toolchains = \\[\"@rules_perl//:current_toolchain\"\\].*") "")
                    ;; Use perl from PATH instead of vendored $(PERL)
                    (("\"PERL\": \"\\$\\$EXT_BUILD_ROOT\\$\\$/\\$\\(PERL\\)\"") "\"PERL\": \"perl\"")))
                ;; Patch /usr/bin/env references in openssl config script
                (when (file-exists? openssl-config)
                  (substitute* openssl-config
                    (("/usr/bin/env") "env"))))))
          (add-after 'build 'prepare-python
            (lambda _
              (chdir "python")
              ;; Skip Bazel in setup.py; we already built artifacts offline.
              (setenv "SKIP_BAZEL_BUILD" "1")
              (setenv "RAY_INSTALL_JAVA" "0")
              (setenv "SKIP_THIRDPARTY_INSTALL_CONDA_FORGE" "1")
              ;; Safety: ensure expected dirs exist even if zips are empty.
              (mkdir-p "ray/core/generated")
              (mkdir-p "ray/serve/generated")
              (mkdir-p "ray/thirdparty_files")
              (mkdir-p "ray/_private/runtime_env/agent/thirdparty_files")
              ;; Ensure the core native binaries from ray_pkg.zip are present.
              ;; In some situations the earlier unzip step may not have run; do
              ;; it here as a fallback.
              (let* ((pkg-zip (cond
                               ((file-exists? "../bazel-bin/ray_pkg.zip")
                                "../bazel-bin/ray_pkg.zip")
                               (else
                                (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP") "/output"))
                                       (found (find-files out-dir "ray_pkg\\.zip$")))
                                  (and (pair? found) (car found))))))
                     (gcs-path "ray/core/src/ray/gcs/gcs_server"))
                (when (and (not (file-exists? gcs-path)) pkg-zip)
                  (invoke "unzip" "-o" "-q" pkg-zip "-d" "."))
                (unless (file-exists? gcs-path)
                  (error "gcs_server missing after unzip; cannot proceed")))
              ;; Ensure the native extension is present; copy from bazel outputs
              ;; if the zip extraction did not place it here.
              (let* ((target "ray/_raylet.so")
                     (from-workspace "../bazel-bin/python/ray/_raylet.so")
                     (out-dir (string-append (getenv "NIX_BUILD_TOP") "/output"))
                     (found (find-files out-dir "_raylet\\.so$"))
                     (candidates (append (if (file-exists? from-workspace)
                                            (list from-workspace)
                                            '())
                                         found)))
                (when (and (not (file-exists? target)) (pair? candidates))
                  (copy-file (car candidates) target))
                (unless (file-exists? target)
                  (error "_raylet.so not found after Bazel build; cannot proceed")))))
          (add-after 'prepare-python 'create-python-symlink
            (lambda _
              (let* ((bin-dir (string-append (getenv "NIX_BUILD_TOP") "/bin"))
                     (python3-path (which "python3"))
                     (python-link (string-append bin-dir "/python")))
                (mkdir-p bin-dir)
                (symlink python3-path python-link)
                (setenv "PATH" (string-append bin-dir ":" (getenv "PATH"))))))
          ;; Some source files (e.g., from git checkouts) may have mtimes before
          ;; 1980. Python's zipfile (used by setuptools bdist_egg under the hood
          ;; of "install") rejects pre-1980 timestamps. Normalize mtimes to a
          ;; safe value to avoid ZIP timestamp errors.
          ;; Run this after prepare-python (when CWD is python/) and before build.
          (add-after 'prepare-python 'fix-pre-1980-mtimes
            (lambda _
              (invoke "find" "." "-xdev" "-type" "f" "-exec" "touch" "-t" "198001020000" "{}" "+")))
          (add-after 'create-python-symlink 'build-python
            (assoc-ref python:%standard-phases 'build))
          ;; Avoid setuptools creating an egg (which zips files and can fail
          ;; on pre-1980 timestamps). Install in distro mode using
          ;; --single-version-externally-managed to copy files directly.
          (add-after 'build-python 'install-python
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out")))
                (invoke "python" "./setup.py" "install"
                        (string-append "--prefix=" out)
                        "--single-version-externally-managed"
                        (string-append "--record=" (getcwd) "/.installed")))))
          (add-after 'install-python 'post-install-fixes
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (site (let* ((sites (find-files lib "site-packages$" #:directories? #t)))
                             (and (pair? sites) (car sites))))
                     (proto-zip (cond
                                  ((file-exists? "../bazel-bin/ray_py_proto.zip")
                                   "../bazel-bin/ray_py_proto.zip")
                                  (else
                                   (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP") "/output"))
                                          (found (find-files out-dir "ray_py_proto\\.zip$")))
                                     (and (pair? found) (car found))))))
                     (py (which "python3")))
                ;; Fix script shebangs explicitly to store python3.
                (when (file-exists? bin)
                  (for-each (lambda (s)
                              (let ((p (string-append bin "/" s)))
                                (when (file-exists? p)
                                  (substitute* p
                                    (("^#!.*$") (string-append "#!" py "\n"))))))
                            '("ray" "serve" "tune")))
                ;; Ensure generated protos are installed in site-packages.
                (when (and site proto-zip)
                  (invoke "unzip" "-o" "-q" proto-zip "-d" site))
                #t))))))
    (native-inputs
     (list `(,openjdk11 "jdk")
           bazel
           git-minimal
           perl
           python
           python-cython-3
           python-setuptools
           python-wheel
           unzip))
    (propagated-inputs
     (list python-aiohttp
           python-click
           python-colorama
           python-filelock
           python-jsonschema
           python-msgpack
           python-packaging
           python-protobuf
           python-psutil
           python-pyyaml
           python-requests
           python-grpcio
           python-numpy
           python-pyarrow
           ;; Keep only Serve HTTP basics per earlier request.
           python-fastapi
           python-starlette
           python-uvicorn
           python-watchfiles))
    (home-page "https://ray.io")
    (synopsis "Distributed computing framework for machine learning")
    (description
     "Ray is a unified framework for scaling AI and Python applications.  It
provides a simple, universal API for building distributed applications, with
libraries for machine learning, distributed training, hyperparameter tuning,
reinforcement learning, and production serving.")
    (license license:asl2.0)))

(define-public python-depyf
  (package
    (name "python-depyf")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "depyf" version))
       (sha256
        (base32 "0i6blns91sqf0pargcj3r91a9vc533q0s8m61z4iq51dncb0kvdg"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require autopep8 which has build issues
    (propagated-inputs (list python-astor python-dill))
    (native-inputs (list git-minimal python-setuptools python-wheel))
    (home-page "https://github.com/thuml/depyf")
    (synopsis "Decompile python functions, from bytecode to source code!")
    (description "Decompile python functions, from bytecode to source code!")
    (license license:expat)))

(define-public python-lm-format-enforcer
  (package
    (name "python-lm-format-enforcer")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lm_format_enforcer" version))
       (sha256
        (base32 "1ni8snbglb51lgmvai8rb84gnvxj16bqik4v98lcx73i130q3076"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require interegular which is not packaged
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/noamgat/lm-format-enforcer")
    (synopsis
     "Enforce the output format (JSON Schema, Regex etc) of a language model")
    (description
     "Enforce the output format (JSON Schema, Regex etc) of a language model.")
    (license license:expat)))

(define-public python-pydantic-extra-types
  (package
    (name "python-pydantic-extra-types")
    (version "2.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydantic_extra_types" version))
       (sha256
        (base32 "1n5sl9cczhynh1pkvnjhy2mavgz7j13bn3cf10pl46klrz0a5kqx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require pytest
    (propagated-inputs (list python-pycountry
                             python-pydantic-2
                             python-typing-extensions))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/pydantic/pydantic-extra-types")
    (synopsis "Extra Pydantic types")
    (description "Extra Pydantic types.")
    (license license:expat)))

(define-public python-mistral-common
  (package
    (name "python-mistral-common")
    (version "1.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mistral_common" version))
       (sha256
        (base32 "1s2x08m72bn15lhdb72s5qzgk3p738waj252828g01y8x7nh8qlz"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; No tests in tarball
    (propagated-inputs (list python-fastapi
                             python-jsonschema
                             python-numpy
                             python-pillow
                             python-pydantic-2
                             python-pydantic-extra-types
                             python-requests
                             python-tiktoken
                             python-typing-extensions
                             python-uvicorn))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mistralai/mistral-common")
    (synopsis
     "Common utilities library for Mistral AI")
    (description
     "Mistral-common is a library of common utilities for Mistral AI.")
    (license license:asl2.0)))

(define-public python-compressed-tensors
  (package
    (name "python-compressed-tensors")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "compressed_tensors" version))
       (sha256
        (base32 "07sbqk7wc8hs1hcj0ra92n04z8p8a9glx1nxjijdyxgpk6bg3pcm"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require additional dependencies
    (propagated-inputs (list python-frozendict
                             python-pydantic-2
                             python-pytorch
                             python-transformers))
    (native-inputs (list python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/neuralmagic/compressed-tensors")
    (synopsis
     "Library for utilization of compressed safetensors of neural network models")
    (description
     "Library for utilization of compressed safetensors of neural network models.")
    (license license:asl2.0)))

(define-public python-partial-json-parser
  (package
    (name "python-partial-json-parser")
    (version "0.2.1.1.post6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "partial_json_parser" version))
       (sha256
        (base32 "1z1sm7p5z7jnxgbvgf1h9gnis9g9bxm4m274pd624y4nj9l6p2a3"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pdm-backend))
    (home-page "https://github.com/tecton-ai/partial-json-parser")
    (synopsis "Parse partial JSON generated by LLM")
    (description "Parse partial JSON generated by LLM.")
    (license license:expat)))

(define-public python-prometheus-fastapi-instrumentator
  (package
    (name "python-prometheus-fastapi-instrumentator")
    (version "7.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prometheus_fastapi_instrumentator" version))
       (sha256
        (base32 "0pmyki1kzbzmx9wjsm14i5yj4gqvcg362hnbxhm93rd4xqgdcz5y"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-prometheus-client python-starlette))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/trallnag/prometheus-fastapi-instrumentator")
    (synopsis "Instrument your FastAPI app with Prometheus metrics")
    (description "Instrument your @code{FastAPI} app with Prometheus metrics.")
    (license license:isc)))

(define-public python-pybase64
  (package
    (name "python-pybase64")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pybase64" version))
       (sha256
        (base32 "1l4jfvwpm0nigz4kwvnf26lbkj8dx16y8bwmblql75pdhg9fzka6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))  ; Tests require pytest
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mayeut/pybase64")
    (synopsis "Fast Base64 encoding/decoding")
    (description "Fast Base64 encoding/decoding.")
    (license license:bsd-2)))
