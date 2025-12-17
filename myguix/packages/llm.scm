(define-module (myguix packages llm)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages java)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages oneapi)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages version-control)
  #:use-module (myguix build-system bazel)
  #:use-module (myguix packages bazel)
  #:use-module (myguix packages huggingface)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages nvidia)
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
                       (search-path (map (cut string-append <>
                                              "/myguix/patches") %load-path)
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
      #:fetch-targets '(list "//:_raylet"
                             "//:raylet_so_files"
                             "//:core_py_proto_zip"
                             "//:serve_py_proto_zip"
                             "//:ray_py_proto_zip"
                             "//:ray_pkg_zip"
                             "//:gen_ray_pkg"
                             "//cpp:gen_ray_cpp_pkg")
      #:build-targets '(list "//:_raylet"
                             "//:raylet_so_files"
                             "//:core_py_proto_zip"
                             "//:serve_py_proto_zip"
                             "//:ray_py_proto_zip"
                             "//:ray_pkg_zip"
                             "//:gen_ray_pkg"
                             "//cpp:gen_ray_cpp_pkg")
      #:bazel-arguments '(list "--cxxopt=-Wno-dangling-reference")
      #:vendored-inputs-hash
      "0yqbdj2917rfkwzgl3aciym66r0c856k7qrimvql3pqyslfd1nfy"
      #:modules '((myguix build bazel-build-system)
                  ((guix build python-build-system)
                   #:prefix python:)
                  (guix build utils))
      #:imported-modules `(,@%bazel-build-system-modules ,@%python-build-system-modules)
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
              (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP")
                                             "/output"))
                     (bazel-bin "bazel-bin")
                     (z1 (string-append bazel-bin "/ray_py_proto.zip"))
                     (z2 (string-append bazel-bin "/ray_pkg.zip"))
                     (from-bazel-bin (append (if (file-exists? z1)
                                                 (list z1)
                                                 '())
                                             (if (file-exists? z2)
                                                 (list z2)
                                                 '())))
                     (zips (append from-bazel-bin
                                   (find-files out-dir "ray_py_proto\\.zip$")
                                   (find-files out-dir "ray_pkg\\.zip$")))
                     (dest "python"))
                (unless (pair? zips)
                  (error "Could not locate Bazel-generated Ray package zips"
                         out-dir))
                (for-each (lambda (zip)
                            (invoke "unzip"
                                    "-o"
                                    "-q"
                                    zip
                                    "-d"
                                    dest)) zips)
                ;; Ensure generated directories exist to satisfy setup.py even if empty.
                (mkdir-p (string-append "python/ray/core/generated"))
                (mkdir-p (string-append "python/ray/serve/generated"))
                ;; Ensure vendored thirdparty dirs exist if we skip vendoring.
                (mkdir-p (string-append "python/ray/thirdparty_files"))
                (mkdir-p (string-append
                          "python/ray/_private/runtime_env/agent/thirdparty_files")))))
          ;; Some Bazel layouts may produce the _raylet.so in the output tree
          ;; without packaging it into ray_pkg.zip (or the zip may be missed).
          ;; As a fallback, locate the shared object and copy it into python/ray/.
          (add-after 'extract-bazel-zips-into-python 'ensure-raylet-extension
            (lambda _
              (let* ((out-dir (string-append (getenv "NIX_BUILD_TOP")
                                             "/output"))
                     (bazel-ray-dir "bazel-bin/python/ray")
                     (bazel-so (string-append bazel-ray-dir "/_raylet.so"))
                     (found (find-files out-dir "_raylet\\.so$"))
                     (target "python/ray/_raylet.so"))
                ;; Prefer copying the entire bazel-bin/python/ray directory (brings .so and
                ;; any related built artifacts) and then fall back to copying a single .so.
                (when (and (file-exists? bazel-ray-dir)
                           (not (file-exists? target)))
                  (mkdir-p "python/ray")
                  (copy-recursively bazel-ray-dir "python/ray"))
                (when (and (not (file-exists? target))
                           (file-exists? bazel-so))
                  (mkdir-p "python/ray")
                  (copy-file bazel-so target))
                (when (and (not (file-exists? target))
                           (pair? found))
                  (mkdir-p "python/ray")
                  (copy-file (car found) target))
                (unless (file-exists? target)
                  (error
                   "_raylet.so not found after Bazel build; cannot proceed")))))
          (add-after 'unpack 'disable-workspace-status
            (lambda _
              (substitute* ".bazelrc"
                (("build:linux --workspace_status_command=.*")
                 "# workspace_status_command disabled for Guix
"))))
          (add-after 'unpack-vendored-inputs 'patch-foreign-cc-shebang
            (lambda _
              ;; Patch rules_foreign_cc to use bash directly instead of /usr/bin/env bash
              ;; The shebang() function is in foreign_cc/private/framework/toolchains/linux_commands.bzl
              (let* ((bash-path (which "bash"))
                     (sh-path (which "sh"))
                     ;; After unpack, we're in source dir; bazel output is ../output from source
                     (linux-cmds
                      "../output/external/rules_foreign_cc/foreign_cc/private/framework/toolchains/linux_commands.bzl"))
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
                                       (("#!/bin/sh")
                                        (string-append "#!" sh-path))
                                       (("#! /bin/sh")
                                        (string-append "#! " sh-path))
                                       ;; Patch explicit /bin/sh invocations in configure scripts
                                       (("/bin/sh ")
                                        (string-append sh-path " "))
                                       ;; Patch Makefile.in SHELL variable (uses := syntax)
                                       (("^SHELL := /bin/sh")
                                        (string-append "SHELL := " sh-path))
                                       ;; Patch shell variable assignments: SHELL=/bin/sh
                                       (("=/bin/sh")
                                        (string-append "=" sh-path))
                                       ;; Patch parameter expansions: ${SHELL-/bin/sh}
                                       (("-/bin/sh")
                                        (string-append "-" sh-path))
                                       ;; Patch parameter expansions: ${SHELL:-/bin/sh}
                                       ((":-/bin/sh")
                                        (string-append ":-" sh-path))
                                       ;; Patch parameter expansions: ${SHELL:=/bin/sh}
                                       ((":=/bin/sh")
                                        (string-append ":=" sh-path))))
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
                    ((".*toolchains = \\[\"@rules_perl//:current_toolchain\"\\].*")
                     "")
                    ;; Use perl from PATH instead of vendored $(PERL)
                    (("\"PERL\": \"\\$\\$EXT_BUILD_ROOT\\$\\$/\\$\\(PERL\\)\"")
                     "\"PERL\": \"perl\"")))
                ;; Patch /usr/bin/env references in openssl config script
                (when (file-exists? openssl-config)
                  (substitute* openssl-config
                    (("/usr/bin/env")
                     "env"))))))
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
                                (else (let* ((out-dir (string-append (getenv
                                                                      "NIX_BUILD_TOP")
                                                       "/output"))
                                             (found (find-files out-dir
                                                     "ray_pkg\\.zip$")))
                                        (and (pair? found)
                                             (car found))))))
                     (gcs-path "ray/core/src/ray/gcs/gcs_server"))
                (when (and (not (file-exists? gcs-path)) pkg-zip)
                  (invoke "unzip"
                          "-o"
                          "-q"
                          pkg-zip
                          "-d"
                          "."))
                (unless (file-exists? gcs-path)
                  (error "gcs_server missing after unzip; cannot proceed")))
              ;; Ensure the native extension is present; copy from bazel outputs
              ;; if the zip extraction did not place it here.
              (let* ((target "ray/_raylet.so")
                     (from-workspace "../bazel-bin/python/ray/_raylet.so")
                     (out-dir (string-append (getenv "NIX_BUILD_TOP")
                                             "/output"))
                     (found (find-files out-dir "_raylet\\.so$"))
                     (candidates (append (if (file-exists? from-workspace)
                                             (list from-workspace)
                                             '()) found)))
                (when (and (not (file-exists? target))
                           (pair? candidates))
                  (copy-file (car candidates) target))
                (unless (file-exists? target)
                  (error
                   "_raylet.so not found after Bazel build; cannot proceed")))))
          (add-after 'prepare-python 'create-python-symlink
            (lambda _
              (let* ((bin-dir (string-append (getenv "NIX_BUILD_TOP") "/bin"))
                     (python3-path (which "python3"))
                     (python-link (string-append bin-dir "/python")))
                (mkdir-p bin-dir)
                (symlink python3-path python-link)
                (setenv "PATH"
                        (string-append bin-dir ":"
                                       (getenv "PATH"))))))
          ;; Some source files (e.g., from git checkouts) may have mtimes before
          ;; 1980. Python's zipfile (used by setuptools bdist_egg under the hood
          ;; of "install") rejects pre-1980 timestamps. Normalize mtimes to a
          ;; safe value to avoid ZIP timestamp errors.
          ;; Run this after prepare-python (when CWD is python/) and before build.
          (add-after 'prepare-python 'fix-pre-1980-mtimes
            (lambda _
              (invoke "find"
                      "."
                      "-xdev"
                      "-type"
                      "f"
                      "-exec"
                      "touch"
                      "-t"
                      "198001020000"
                      "{}"
                      "+")))
          (add-after 'create-python-symlink 'build-python
            (assoc-ref python:%standard-phases
                       'build))
          ;; Avoid setuptools creating an egg (which zips files and can fail
          ;; on pre-1980 timestamps). Install in distro mode using
          ;; --single-version-externally-managed to copy files directly.
          (add-after 'build-python 'install-python
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out")))
                (invoke "python"
                        "./setup.py"
                        "install"
                        (string-append "--prefix=" out)
                        "--single-version-externally-managed"
                        (string-append "--record="
                                       (getcwd) "/.installed")))))
          (add-after 'install-python 'post-install-fixes
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (site (let* ((sites (find-files lib "site-packages$"
                                                     #:directories? #t)))
                             (and (pair? sites)
                                  (car sites))))
                     (proto-zip (cond
                                  ((file-exists?
                                    "../bazel-bin/ray_py_proto.zip")
                                   "../bazel-bin/ray_py_proto.zip")
                                  (else (let* ((out-dir (string-append (getenv
                                                                        "NIX_BUILD_TOP")
                                                         "/output"))
                                               (found (find-files out-dir
                                                       "ray_py_proto\\.zip$")))
                                          (and (pair? found)
                                               (car found))))))
                     (py (which "python3")))
                ;; Fix script shebangs explicitly to store python3.
                (when (file-exists? bin)
                  (for-each (lambda (s)
                              (let ((p (string-append bin "/" s)))
                                (when (file-exists? p)
                                  (substitute* p
                                    (("^#!.*$")
                                     (string-append "#!" py "\n"))))))
                            '("ray" "serve" "tune")))
                ;; Ensure generated protos are installed in site-packages.
                (when (and site proto-zip)
                  (invoke "unzip"
                          "-o"
                          "-q"
                          proto-zip
                          "-d"
                          site)) #t))))))
    (native-inputs (list `(,openjdk11 "jdk")
                         bazel
                         git-minimal
                         perl
                         python
                         python-cython-3
                         python-setuptools
                         python-wheel
                         unzip))
    (propagated-inputs (list python-aiohttp
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
     (list
      #:tests? #f)) ;Tests require autopep8 which has build issues
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
     (list
      #:tests? #f)) ;Tests require interegular which is not packaged
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
     (list
      #:tests? #f)) ;Tests require pytest
    (propagated-inputs (list python-pycountry python-pydantic-2
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
     (list
      #:tests? #f)) ;No tests in tarball
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
    (synopsis "Common utilities library for Mistral AI")
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
     (list
      #:tests? #f)) ;Tests require additional dependencies
    (propagated-inputs (list python-frozendict python-pydantic-2
                             python-pytorch python-transformers))
    (native-inputs (list python-setuptools python-setuptools-scm python-wheel))
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
     (list
      #:tests? #f)) ;Tests require pytest
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mayeut/pybase64")
    (synopsis "Fast Base64 encoding/decoding")
    (description "Fast Base64 encoding/decoding.")
    (license license:bsd-2)))

(define-public python-vllm
  (package
    (name "python-vllm")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vllm" version))
       (sha256
        (base32 "1bmzj9ikmb3rkxmd1xqj812dr3alv3nzhxkscn6igi794i6acdgl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex))
      #:imported-modules %pyproject-build-system-modules
      #:phases
      #~(modify-phases %standard-phases
          ;; Ensure the vLLM extension has proper RUNPATH to find PyTorch libs
          ;; right before Guix validates RUNPATHs.
          (add-before 'validate-runpath 'fix-vllm-rpath
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (so-files (find-files out
                                           (lambda (file stat)
                                             (and (eq? 'regular
                                                       (stat:type stat))
                                                  (regexp-exec (make-regexp
                                                                ".*/site-packages/vllm/_C.*\\.so$")
                                                               file)))))
                     (py (open-pipe* OPEN_READ "python3" "-c"
                          "import os, torch; print(os.path.join(os.path.dirname(torch.__file__), 'lib'))"))
                     (py-out (read-line py))
                     (py-status (close-pipe py))
                     (torch-lib (let ((cand (and (zero? py-status) py-out)))
                                  (if (and cand
                                           (file-exists? cand)) cand
                                      (let* ((tor (assoc-ref inputs
                                                             "python-pytorch")))
                                        (and tor
                                             (let ((c (find-files (string-append
                                                                   tor "/lib")
                                                       ".*/site-packages/torch/lib$"
                                                       #:directories? #t)))
                                               (and (pair? c)
                                                    (car c)))))))))
                (unless (pair? so-files)
                  (error "vLLM shared object not found for RPATH patching" out))
                (unless torch-lib
                  (error "PyTorch lib directory not found under input"
                         (assoc-ref inputs "python-pytorch")))
                (for-each (lambda (file)
                            (let* ((p (open-pipe* OPEN_READ "patchelf"
                                                  "--print-rpath" file))
                                   (line (read-line p))
                                   (status (close-pipe p))
                                   (existing (if (and (zero? status) line)
                                                 line ""))
                                   (new-rpath (if (string=? existing "")
                                                  torch-lib
                                                  (string-append torch-lib ":"
                                                                 existing))))
                              (invoke "patchelf" "--set-rpath" new-rpath file)))
                          so-files))))
          (replace 'sanity-check
            (lambda _
              #t)))))
    (propagated-inputs (list python-aiohttp
                             python-blake3
                             python-cachetools
                             python-cbor2
                             python-cloudpickle
                             python-compressed-tensors
                             python-depyf
                             python-diskcache
                             python-einops
                             python-fastapi
                             python-filelock
                             python-gguf
                             python-lark
                             python-lm-format-enforcer
                             python-mistral-common
                             python-msgspec
                             python-numpy
                             python-openai
                             python-openai-harmony
                             python-opencv-python-headless
                             python-outlines-core
                             python-partial-json-parser
                             python-pillow
                             python-prometheus-fastapi-instrumentator
                             python-prometheus-client
                             python-protobuf
                             python-psutil
                             python-py-cpuinfo
                             python-pybase64
                             python-pydantic
                             python-json-logger
                             python-pyyaml
                             python-pyzmq
                             python-ray
                             python-regex
                             python-requests
                             python-scipy
                             python-sentencepiece
                             python-setproctitle
                             python-tiktoken
                             python-tokenizers
                             python-pytorch
                             python-torchaudio
                             python-torchvision
                             python-tqdm
                             python-transformers
                             python-typing-extensions
                             python-watchfiles))
    (native-inputs (list python-setuptools
                         python-setuptools-scm
                         python-wheel
                         cmake
                         ninja
                         bash-minimal
                         pkg-config
                         patchelf))
    (inputs (list onednn
                  cpp-httplib
                  openssl
                  brotli
                  numactl
                  python-pytorch))
    (home-page "https://vllm.ai")
    (synopsis
     "A high-throughput and memory-efficient inference and serving engine for LLMs")
    (description
     "This package provides a high-throughput and memory-efficient inference and
serving engine for LLMs.")
    (license license:asl2.0)))

;; CUDA-enabled variant: links against python-pytorch-cuda and embeds
;; CUDA/NCCL/cuDNN runpaths alongside torch's lib directory.
(define-public python-vllm-cuda
  (package
    (inherit python-vllm)
    (name "python-vllm-cuda")
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (guix build union))
      #:imported-modules `(,@%pyproject-build-system-modules (guix build union))
      #:phases
      #~(modify-phases %standard-phases
          ;; Limit parallel compile to reduce peak memory usage
          (add-before 'build 'limit-parallel-build
            (lambda _
              (setenv "CMAKE_BUILD_PARALLEL_LEVEL" "6")
              (setenv "MAX_JOBS" "6")
              (setenv "NINJAFLAGS" "-j6")
              (setenv "MAKEFLAGS" "-j6")))
          ;; Make setuptools stop requesting flash-attn/flashmla targets.
          ;; These are network-fetched external projects upstream; we skip
          ;; them in Guix to keep builds hermetic.
          (add-before 'build 'disable-flash-targets-simple
            (lambda _
              (when (file-exists? "setup.py")
                (substitute* "setup.py"
                  (("ext_modules\\.append[^\\n]*_vllm_fa2_C")
                   "# Guix: disabled _vllm_fa2_C")
                  (("ext_modules\\.append[^\\n]*_vllm_fa3_C")
                   "# Guix: disabled _vllm_fa3_C")
                  (("ext_modules\\.append[^\\n]*_flashmla_C")
                   "# Guix: disabled _flashmla_C")
                  (("ext_modules\\.append[^\\n]*_flashmla_extension_C")
                   "# Guix: disabled _flashmla_extension_C")
                  (("cmake_args += .*VLLM_PYTHON_PATH")
                   "cmake_args += ['-DVLLM_PYTHON_PATH={}'.format(':'.join(sys.path))]
        cmake_args += ['-DCAFFE2_USE_CUDNN=ON','-DCAFFE2_USE_CUSPARSELT=ON','-DUSE_CUDSS=ON','-DCAFFE2_USE_CUFILE=ON']")))))
          ;; Avoid network fetches for external subprojects.
          (add-before 'build 'disable-external-projects
            (lambda _
              (when (file-exists? "CMakeLists.txt")
                (substitute* "CMakeLists.txt"
                  (("include\\(cmake/external_projects/flashmla.cmake\\)")
                   "message(STATUS \"Guix: Skipping FlashMLA external project\")")
                  (("include\\(cmake/external_projects/vllm_flash_attn.cmake\\)")
                   "message(STATUS \"Guix: Skipping vllm-flash-attn external project\")"))
                ;; Provide stub targets so setuptools' build_ext doesn't fail
                ;; when it asks CMake to build these components.
                (let ((stubs (string-append
                              "
# Guix: define stub targets for skipped external projects
"
                              "add_custom_target(_vllm_fa2_C)\n"
                              "add_custom_target(_vllm_fa3_C)\n"
                              "add_custom_target(_flashmla_C)\n"
                              "add_custom_target(_flashmla_extension_C)
")))
                  (let ((port (open-file "CMakeLists.txt" "a")))
                    (display stubs port)
                    (close-port port))))))
          ;; CUTLASS MLA (Blackwell FMHA) pulls headers from examples/common
          ;; that are not available in some CUTLASS releases. Disable MLA to
          ;; avoid build breakage while keeping all other kernels.
          (add-before 'build 'disable-cutlass-mla
            (lambda _
              (when (file-exists? "CMakeLists.txt")
                (substitute* "CMakeLists.txt"
                  (("cuda_archs_loose_intersection\\(MLA_ARCHS \"10.0a\" \"\\$\\{CUDA_ARCHS\\}\"\\)")
                   "set(MLA_ARCHS)")
                  (("if\\(\\$\\{CMAKE_CUDA_COMPILER_VERSION\\} VERSION_GREATER_EQUAL 12.8 AND MLA_ARCHS\\)")
                   "if(0)
  message(STATUS \"Guix: CUTLASS MLA disabled\")")))))
          ;; Ensure Torch CUDA components are enabled during vLLM configure.
          (add-before 'build 'enable-torch-cuda-features
            (lambda _
              (when (file-exists? "CMakeLists.txt")
                (substitute* "CMakeLists.txt"
                  (("find_package\\(Torch REQUIRED\\)")
                   (string-append "set(CAFFE2_USE_CUDNN ON)\n"
                                  "set(CAFFE2_USE_CUSPARSELT ON)\n"
                                  "set(USE_CUDSS ON)\n"
                                  "set(CAFFE2_USE_CUFILE ON)\n"
                                  "find_package(Torch REQUIRED)"))))))
          ;; Define CUTLASS include directories explicitly so headers under
          ;; tools/util can be found without relying on CUTLASS' CMake.
          (add-before 'build 'define-cutlass-includes
            (lambda _
              (when (file-exists? "CMakeLists.txt")
                (substitute* "CMakeLists.txt"
                  (("FetchContent_MakeAvailable\\(cutlass\\)")
                   (string-append "FetchContent_MakeAvailable(cutlass)\n"
                    "# Guix: provide include dirs for header-only builds
"
                    "set(CUTLASS_DIR ${VLLM_CUTLASS_SRC_DIR})
"
                    "set(CUTLASS_INCLUDE_DIR ${VLLM_CUTLASS_SRC_DIR}/include)
"
                    "set(CUTLASS_TOOLS_UTIL_INCLUDE_DIR ${VLLM_CUTLASS_SRC_DIR}/tools/util/include)"))))))
          ;; Ensure Torch CUDA components are enabled when configuring vLLM.
          (add-before 'build 'enable-torch-cuda-features
            (lambda _
              (when (file-exists? "CMakeLists.txt")
                (substitute* "CMakeLists.txt"
                  (("find_package\\(Torch REQUIRED\\)")
                   (string-append "set(CAFFE2_USE_CUDNN ON)\n"
                                  "set(CAFFE2_USE_CUSPARSELT ON)\n"
                                  "set(USE_CUDSS ON)\n"
                                  "set(CAFFE2_USE_CUFILE ON)\n"
                                  "find_package(Torch REQUIRED)"))))))
          (add-before 'build 'use-existing-torch
            (lambda _
              ;; Instruct vLLM to use the already-provided PyTorch installation
              ;; (mirrors `python use_existing_torch.py`).
              (when (file-exists? "use_existing_torch.py")
                (invoke (which "python3") "use_existing_torch.py"))))
          (add-before 'build 'configure-cuda
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((cuda (assoc-ref inputs "cuda-toolkit"))
                     (bin (string-append cuda "/bin")))
                (setenv "CUDA_HOME" cuda)
                (setenv "PATH"
                        (string-append bin ":"
                                       (getenv "PATH"))))
              ;; Provide vLLM a local CUTLASS source tree; avoids any network fetch.
              (use-modules (guix build union))
              (let* ((cutlass-root (string-append (getenv "NIX_BUILD_TOP")
                                                  "/cutlass-src"))
                     (hdr (assoc-ref inputs "cutlass-headers"))
                     (tools (assoc-ref inputs "cutlass-tools"))
                     (py (assoc-ref inputs "cutlass-python")))
                ;; Combine headers, tools and python helpers into a single
                ;; tree so vLLM can point CUTLASS_DIR at one place.
                (union-build cutlass-root
                             (filter identity
                                     (list hdr tools py)))
                (setenv "VLLM_CUTLASS_SRC_DIR" cutlass-root)
                ;; vLLM's generation scripts import `cutlass_library` from
                ;; CUTLASS tools; make it importable during build.
                (let* ((cutlass-tools-py (string-append cutlass-root
                                          "/tools/library/scripts/py"))
                       (cutlass-python (string-append cutlass-root "/python"))
                       (cur (or (getenv "PYTHONPATH") ""))
                       (pp (string-join (filter file-exists?
                                                (list cutlass-tools-py
                                                      cutlass-python)) ":")))
                  (when (and pp
                             (not (string-null? pp)))
                    (setenv "PYTHONPATH"
                            (if (zero? (string-length cur)) pp
                                (string-append pp ":" cur))))))))
          ;; Provide env hints so Torch CMake can locate cuDNN/cuSPARSELt/cuDSS
          ;; from our inputs when configuring vLLM.
          (add-before 'build 'torch-cuda-env
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((cudnn (assoc-ref inputs "cudnn"))
                     (cuslt (assoc-ref inputs "cusparselt"))
                     (cudss (assoc-ref inputs "cudss"))
                     (cp (or (getenv "CMAKE_PREFIX_PATH") ""))
                     (prefixes (string-join (filter identity
                                                    (list cudnn cuslt cudss))
                                            ":")))
                (when (and prefixes
                           (not (string-null? prefixes)))
                  (setenv "CMAKE_PREFIX_PATH"
                          (if (zero? (string-length cp)) prefixes
                              (string-append prefixes ":" cp))))
                (when cudnn
                  (setenv "CUDNN_ROOT" cudnn)
                  (setenv "CUDNN_INCLUDE_DIR"
                          (string-append cudnn "/include"))
                  (setenv "CUDNN_LIBRARY"
                          (string-append cudnn "/lib")))
                (when cuslt
                  (setenv "CUSPARSELT_ROOT" cuslt)
                  (setenv "CUSPARSELT_INCLUDE_DIR"
                          (string-append cuslt "/include"))
                  (setenv "CUSPARSELT_LIBRARY"
                          (string-append cuslt "/lib")))
                (when cudss
                  (setenv "CUDSS_ROOT" cudss)
                  (setenv "CUDSS_INCLUDE_DIR"
                          (string-append cudss "/include"))
                  (setenv "CUDSS_LIBRARY"
                          (string-append cudss "/lib"))))))
          (add-after 'install 'fix-vllm-rpath-cuda
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (so-files (find-files out
                                           (lambda (file stat)
                                             (and (eq? 'regular
                                                       (stat:type stat))
                                                  (regexp-exec (make-regexp
                                                                ".*/site-packages/vllm/_C.*\\.so$")
                                                               file)))))
                     ;; Torch lib (via import, fallback to packaged path)
                     (py (open-pipe* OPEN_READ "python3" "-c"
                          "import os, torch; print(os.path.join(os.path.dirname(torch.__file__), 'lib'))"))
                     (py-out (read-line py))
                     (py-status (close-pipe py))
                     (torch-lib (let ((cand (and (zero? py-status) py-out)))
                                  (if (and cand
                                           (file-exists? cand)) cand
                                      (let* ((tor (assoc-ref inputs
                                                             "python-pytorch")))
                                        (and tor
                                             (let ((c (find-files (string-append
                                                                   tor "/lib")
                                                       ".*/site-packages/torch/lib$"
                                                       #:directories? #t)))
                                               (and (pair? c)
                                                    (car c))))))))
                     ;; CUDA/NVIDIA libs
                     (cuda (assoc-ref inputs "cuda-toolkit"))
                     (cudnn (assoc-ref inputs "cudnn"))
                     (nccl (assoc-ref inputs "nccl"))
                     (cuda-lib (and cuda
                                    (string-append cuda "/lib64")))
                     (cudnn-lib (and cudnn
                                     (string-append cudnn "/lib")))
                     (nccl-lib (and nccl
                                    (string-append nccl "/lib"))))
                (unless (pair? so-files)
                  (error
                   "vLLM shared object not found for RPATH patching (CUDA)"
                   out))
                (unless torch-lib
                  (error "PyTorch lib directory not found under input"
                         (assoc-ref inputs "python-pytorch")))
                (for-each (lambda (file)
                            (let* ((p (open-pipe* OPEN_READ "patchelf"
                                                  "--print-rpath" file))
                                   (line (read-line p))
                                   (status (close-pipe p))
                                   (existing (if (and (zero? status) line)
                                                 line ""))
                                   (prefix (string-join (filter identity
                                                                (list
                                                                 torch-lib
                                                                 cuda-lib
                                                                 cudnn-lib
                                                                 nccl-lib))
                                                        ":"))
                                   (new-rpath (if (string=? existing "")
                                                  prefix
                                                  (string-append prefix ":"
                                                                 existing))))
                              (invoke "patchelf" "--set-rpath" new-rpath file)))
                          so-files)))))))
    (inputs (modify-inputs (package-inputs python-vllm)
              (replace "python-pytorch" python-pytorch-cuda)
              (append cuda-toolkit
                      cudnn
                      nccl
                      cusparselt
                      cudss
                      cutlass-headers
                      cutlass-tools
                      cutlass-python)))
    (propagated-inputs (modify-inputs (package-propagated-inputs python-vllm)
                         (replace "python-pytorch" python-pytorch-cuda)))
    (synopsis "vLLM with CUDA support")))
