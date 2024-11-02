(define-module (myguix packages machine-learning)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
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
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix build-system bazel)
  #:use-module (myguix packages rust-pqrs)
  #:use-module (myguix packages bazel)
  #:use-module (ice-9 match))

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
                   (add-after 'build 'build-python-module
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

(define-public static-protobuf
  (package
    (inherit protobuf)
    (name "protobuf-static")
    (outputs (list "out"))
    (arguments
     (substitute-keyword-arguments (package-arguments protobuf)
       ((#:configure-flags flags)
        #~(list "-DBUILD_SHARED_LIBS=OFF" "-Dprotobuf_USE_EXTERNAL_GTEST=ON"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'move-static-libraries)))))))

(define-public eigen-for-python-ml-dtypes
  (let ((commit "7bf2968fed5f246c0589e1111004cb420fcd7c71")
        (revision "1"))
    (package
      (inherit eigen)
      (name "eigen-for-python-ml-dtypes")
      (version (git-version "3.4.90" revision commit))
      (source
       (origin
         (inherit (package-source eigen))
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/libeigen/eigen.git")
               (commit commit)))
         (sha256
          (base32 "0yq69h7pasbzq5r83d974xi031r0z2y2x0my1rz5crky54i1j0r7"))
         (patches '())
         (file-name (git-file-name name version))))
      (arguments
       (substitute-keyword-arguments (package-arguments eigen)
         ((#:tests? flag #f)
          #f))))))

(define-public python-ml-dtypes
  (package
    (name "python-ml-dtypes")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ml_dtypes" version))
       (sha256
        (base32 "04f61zkizfgmf2pqlsdgskj1r1gg6l5j1nj2p8v4yk2b36cqyxv0"))
       (modules '((guix build utils)))
       (snippet
        ;; Do not use bundled eigen.
        '(delete-file-recursively "third_party/eigen"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;there are none
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'use-eigen-package
                     (lambda _
                       (substitute* "setup.py"
                         (("third_party/eigen")
                          (string-append #$(this-package-input
                                            "eigen-for-python-ml-dtypes")
                                         "/include/eigen3"))))))))
    (inputs (list eigen-for-python-ml-dtypes))
    (propagated-inputs (list python-numpy))
    (native-inputs (list pybind11 python-absl-py python-pylint python-pytest
                         python-pytest-xdist))
    (home-page "https://github.com/jax-ml/ml_dtypes")
    (synopsis "NumPy dtype extensions used in machine learning")
    (description
     "This package is a stand-alone implementation of several
NumPy @code{dtype} extensions used in machine learning libraries, including:

@itemize
@item @code{bfloat16}: an alternative to the standard @code{float16} format
@item @code{float8_*}: several experimental 8-bit floating point
  representations including:
  @itemize
  @item @code{float8_e4m3b11fnuz}
  @item @code{float8_e4m3fn}
  @item @code{float8_e4m3fnuz}
  @item @code{float8_e5m2}
  @item @code{float8_e5m2fnuz}
  @end itemize
@item @code{int4} and @code{uint4}: low precision integer types.
@end itemize
")
    (license license:asl2.0)))

;; This one is self-contained to avoid problems when using a
;; mismatched version of the protobuf library.
(define-public python-protobuf-for-tensorflow-2
  (package
    (name "python-protobuf-for-tensorflow-2")
    ;; This matches the C++ version 3.21.9.  I don't make the rules.
    (version "4.21.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32 "16asi3sdq3mqir2irlkixyshlmbjb08gmzl4rbwpfakdv69i9wk1"))))
    (build-system python-build-system)
    (home-page "https://github.com/google/protobuf")
    (synopsis "Protocol buffers is a data interchange format")
    (description
     "Protocol buffers are a language-neutral, platform-neutral extensible
mechanism for serializing structured data.")
    (license license:bsd-3)))

(define-public python-simple-parsing
  (package
    (name "python-simple-parsing")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "simple_parsing" version))
       (sha256
        (base32 "1s0s6hc6qz66x4rzfrnshf5vwdfk7d1015a1nrhyn2wgglr008ri"))))
    (build-system pyproject-build-system)
    ;; TODO: There is some sort of path issue, because most tests fail
    ;; outright with this error: ImportError: attempted relative
    ;; import with no known parent package
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-docstring-parser python-typing-extensions))
    (native-inputs (list python-numpy python-pytest python-pytest-benchmark
                         python-pytest-regressions python-pytest-xdist))
    (home-page "https://github.com/lebrice/SimpleParsing")
    (synopsis
     "Utility for simplifying and cleaning up argument parsing scripts")
    (description "This package provides a small utility for simplifying and
cleaning up argument parsing scripts.")
    (license license:expat)))

(define-public python-tensorstore
  (let ((tensorstore-python-packages (list "absl_py"
                                           ;; "alabaster"
                                           ;; "annotated_types"
                                           "appdirs"
                                           ;; "appnope"
                                           "asttokens"
                                           "attrs"
                                           "aws_sam_translator"
                                           "aws_xray_sdk"
                                           "babel"
                                           "blinker"
                                           "boto3"
                                           "botocore"
                                           "certifi"
                                           "cffi"
                                           "cfn_lint"
                                           "charset_normalizer"
                                           "click"
                                           "cloudpickle"
                                           "colorama"
                                           ;; "crc32c"
                                           "cryptography"
                                           "decorator"
                                           "docker"
                                           "docutils"
                                           "ecdsa"
                                           "exceptiongroup"
                                           "executing"
                                           "flask"
                                           "flask_cors"
                                           "googleapis_common_protos"
                                           "graphql_core"
                                           "grpcio"
                                           "idna"
                                           "imagesize"
                                           "importlib_metadata"
                                           "iniconfig"
                                           "ipython"
                                           "itsdangerous"
                                           "jedi"
                                           "jinja2"
                                           "jmespath"
                                           ;; "jschema_to_python"
                                           "jsondiff"
                                           "jsonpatch"
                                           "jsonpickle"
                                           "jsonpointer"
                                           "jsonschema"
                                           ;; "jsonschema_path"
                                           ;; "jsonschema_specifications"
                                           "junit_xml"
                                           "lazy_object_proxy"
                                           "markupsafe"
                                           "matplotlib_inline"
                                           "ml_dtypes"
                                           "moto"
                                           "mpmath"
                                           "networkx"
                                           "numpy"
                                           "openapi_schema_validator"
                                           "openapi_spec_validator"
                                           "packaging"
                                           "parso"
                                           ;; "pathable"
                                           "pbr"
                                           "pexpect"
                                           "platformdirs"
                                           "pluggy"
                                           "prompt_toolkit"
                                           "protobuf"
                                           "ptyprocess"
                                           "pure_eval"
                                           ;; "py_partiql_parser"
                                           "pyasn1"
                                           "pycparser"
                                           "pydantic"
                                           ;; "pydantic_core"
                                           ;; "pydantic_extra_types"
                                           "pygments"
                                           "pyparsing"
                                           "pytest"
                                           "pytest_asyncio"
                                           "python_dateutil"
                                           "python_jose"
                                           ;; "pywin32"
                                           "pyyaml"
                                           ;; "referencing"
                                           "regex"
                                           "requests"
                                           "requests_toolbelt"
                                           "responses"
                                           "rfc3339_validator"
                                           "rpds_py"
                                           "rsa"
                                           "s3transfer"
                                           "sarif_om"
                                           ;; "scalpl"
                                           "setuptools"
                                           "six"
                                           "snowballstemmer"
                                           "sphinx"
                                           ;; "sphinx_immaterial"
                                           "sphinxcontrib_applehelp"
                                           "sphinxcontrib_devhelp"
                                           "sphinxcontrib_htmlhelp"
                                           "sphinxcontrib_jsmath"
                                           "sphinxcontrib_qthelp"
                                           "sphinxcontrib_serializinghtml"
                                           "sshpubkeys"
                                           "stack_data"
                                           "sympy"
                                           "tomli"
                                           "traitlets"
                                           "typing_extensions"
                                           "urllib3"
                                           "wcwidth"
                                           "websocket_client"
                                           "werkzeug"
                                           "wrapt"
                                           "xmltodict"
                                           "yapf"
                                           "zipp")))
    (package
      (name "python-tensorstore")
      (version "0.1.52")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/tensorstore")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hapkxnxcmn90xnk9ldb6nkszbnmb5zyw8x4m10wd605zxapmlhd"))
         (modules '((guix build utils)))
         (snippet
          ;; We need to patch the build system to avoid embedding
          ;; /gnu/store locations for a handful of numpy headers.
          '(substitute* "third_party/python/python_configure.bzl"
             (("numpy_include = _get_numpy_include.*")
              "numpy_include = \"/tmp/numpy-include\"\n")))))
      (build-system bazel-build-system)
      (arguments
       (list
        #:modules '((guix-science build bazel-build-system)
                    ((guix build pyproject-build-system)
                     #:prefix pyproject:)
                    (guix build utils))
        #:imported-modules `(,@%bazel-build-system-modules ,@%pyproject-build-system-modules)
        #:tests? #f ;there are none
        #:bazel bazel-6.4
        #:fetch-targets '(list
                          "//python/tensorstore:_tensorstore__shared_objects")
        #:build-targets '(list
                          "//python/tensorstore:_tensorstore__shared_objects")
        #:bazel-configuration #~(begin
                                  ;; Make numpy headers available at expected location.  See
                                  ;; snippet above for more information.
                                  (let ((python-version #$(version-major+minor
                                                           (package-version (this-package-input
                                                                             "python-wrapper")))))
                                    (copy-recursively (string-append #$(this-package-input
                                                                        "python-numpy")
                                                       "/lib/python"
                                                       python-version
                                                       "/site-packages/numpy/core/include/numpy")
                                                      "/tmp/numpy-include"))
                                  ;; You can get the list of possible values of
                                  ;; TENSORSTORE_SYSTEM_PYTHON_LIBS and
                                  ;; TENSORSTORE_SYSTEM_LIBS by searching the tensorstore
                                  ;; checkout for system_build_file.  Any match is a possible
                                  ;; replacement.
                                  (setenv "TENSORSTORE_SYSTEM_PYTHON_LIBS"
                                          (string-join '#$tensorstore-python-packages
                                                       ","))
                                  (setenv "TENSORSTORE_SYSTEM_LIBS"
                                          (string-join (list
                                                        ;; We don't seem to have a
                                                        ;; conventional library with headers,
                                                        ;; even though we use rust-blake3
                                                        ;; successfully in python-blake3.
                                                        ;; "blake3"
                                                        ;; when building with our variant we
                                                        ;; get this error: reference to
                                                        ;; ‘basic_json’ is ambiguous
                                                        ;; "com_github_nlohmann_json"
                                                        "com_github_pybind_pybind11"
                                                        ;; "com_google_boringssl"
                                                        "com_google_brotli"
                                                        "com_google_snappy"
                                                        "jpeg"
                                                        "libtiff"
                                                        "libwebp"
                                                        "nasm"
                                                        "net_zlib"
                                                        "net_zstd"
                                                        "org_aomedia_avif"
                                                        "org_blosc_cblosc"
                                                        "org_lz4"
                                                        "org_nghttp2"
                                                        "org_sourceware_bzip2"
                                                        "org_tukaani_xz"
                                                        "png"
                                                        "se_curl") ",")))
        #:bazel-arguments #~(list "-c"
                             "opt"
                             ;; We need a more recent version of platforms, because the
                             ;; included cpu package does not define cpu:wasm32.
                             (string-append "--override_repository=platforms="
                              #$(this-package-native-input "bazel-platforms"))
                             "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
                             "--action_env=GUIX_PYTHONPATH"
                             "--host_action_env=GUIX_PYTHONPATH"
                             "--action_env=TENSORSTORE_SYSTEM_PYTHON_LIBS"
                             "--host_action_env=TENSORSTORE_SYSTEM_PYTHON_LIBS"
                             "--action_env=TENSORSTORE_SYSTEM_LIBS"
                             "--host_action_env=TENSORSTORE_SYSTEM_LIBS"
                             "--action_env=PYTHON_LIB_PATH"
                             "--host_action_env=PYTHON_LIB_PATH"
                             "--action_env=PYTHON_BIN_PATH"
                             "--host_action_env=PYTHON_BIN_PATH"
                             (string-append "--python_path="
                                            #$(this-package-input
                                               "python-wrapper") "/bin/python"))
        #:vendored-inputs-hash
        "1sdwk3fnf0gfk1h2fb5082r87ahzhswznj21p9d1y0g0x97hw6zk"
        #:phases #~(modify-phases (@ (guix-science build bazel-build-system)
                                     %standard-phases)
                     (add-after 'unpack 'patch-python-build-system
                       (lambda _
                         (substitute* "pyproject.toml"
                           (("oldest-supported-numpy")
                            "numpy"))
                         ;; This rule expects that
                         ;; _tensorstore.cpython-310-x86_64-linux-gnu.so exists,
                         ;; but we've only built _tensorstore.so.
                         (substitute* "setup.py"
                           (("os.path.basename\\(ext_full_path\\)")
                            "'_tensorstore.so'")
                           (("'fallback_version': '0.0.0'")
                            (string-append "'fallback_version': '"
                                           #$version "'")))
                         ;; Make numpy headers available at expected location.  See
                         ;; snippet above for more information.
                         (let ((python-version #$(version-major+minor (package-version
                                                                       (this-package-input
                                                                        "python-wrapper")))))
                           (copy-recursively (string-append #$(this-package-input
                                                               "python-numpy")
                                              "/lib/python" python-version
                                              "/site-packages/numpy/core/include/numpy")
                                             "/tmp/numpy-include"))))
                     (add-after 'build 'prepare-python
                       (lambda _
                         (setenv "TENSORSTORE_PREBUILT_DIR"
                                 (string-append (getcwd) "/bazel-bin/python/"))))
                     (add-after 'prepare-python 'build-python
                       (assoc-ref pyproject:%standard-phases
                                  'build))
                     (add-after 'build-python 'install-python
                       (assoc-ref pyproject:%standard-phases
                                  'install))
                     (add-after 'install-python 'create-entrypoints
                       (assoc-ref pyproject:%standard-phases
                                  'create-entrypoints))
                     (add-after 'create-entrypoints 'compile-bytecode
                       (assoc-ref pyproject:%standard-phases
                                  'compile-bytecode)))))
      (propagated-inputs (list python-absl-py
                               python-appdirs
                               python-asttokens
                               python-attrs
                               python-aws-sam-translator
                               python-aws-xray-sdk
                               python-babel
                               python-blinker
                               python-boto3
                               python-botocore
                               python-certifi
                               python-cffi
                               python-cfn-lint
                               python-charset-normalizer
                               python-click
                               python-cloudpickle
                               python-colorama
                               python-cryptography
                               python-dateutil
                               python-decorator
                               python-docker
                               python-docutils
                               python-ecdsa
                               python-exceptiongroup
                               python-executing
                               python-flask
                               python-flask-cors
                               python-googleapis-common-protos
                               python-graphql-core
                               python-grpcio
                               python-idna
                               python-imagesize
                               python-importlib-metadata
                               python-iniconfig
                               python-ipython
                               python-itsdangerous
                               python-jedi
                               python-jinja2
                               python-jmespath
                               python-jose
                               python-jsondiff
                               python-jsonpatch
                               python-jsonpickle
                               python-jsonpointer
                               python-jsonschema
                               python-junit-xml
                               python-lazy-object-proxy
                               python-markupsafe
                               python-matplotlib-inline
                               python-ml-dtypes
                               python-moto
                               python-mpmath
                               python-networkx
                               python-numpy
                               python-openapi-schema-validator
                               python-openapi-spec-validator
                               python-packaging
                               python-parso
                               python-pbr
                               python-pexpect
                               python-platformdirs
                               python-pluggy
                               python-prompt-toolkit
                               python-protobuf
                               python-ptyprocess
                               python-pure-eval
                               python-pyasn1
                               python-pycparser
                               ;; python-pydantic ;we don't have *core and *extra_types
                               python-pygments
                               python-pyparsing
                               python-pytest
                               python-pytest-asyncio
                               python-pyyaml
                               python-regex
                               python-requests
                               python-requests-toolbelt
                               python-responses
                               python-rfc3339-validator
                               python-rpds-py
                               python-rsa
                               python-s3transfer
                               python-sarif-om
                               python-setuptools
                               python-six
                               python-snowballstemmer
                               python-sphinx
                               python-sphinxcontrib-applehelp
                               python-sphinxcontrib-devhelp
                               python-sphinxcontrib-htmlhelp
                               python-sphinxcontrib-jsmath
                               python-sphinxcontrib-qthelp
                               python-sphinxcontrib-serializinghtml
                               python-sshpubkeys
                               python-stack-data
                               python-sympy
                               python-tomli
                               python-traitlets
                               python-typing-extensions
                               python-urllib3
                               python-wcwidth
                               python-websocket-client
                               python-werkzeug
                               python-wrapt
                               python-xmltodict
                               python-yapf
                               python-zipp))
      (inputs (list brotli
                    c-blosc
                    curl
                    libavif
                    libjpeg-turbo
                    libpng
                    libtiff
                    libwebp
                    lz4
                    nasm
                    nghttp2
                    ;; nlohmann-json ;our version seems to be too old
                    python-wrapper
                    snappy
                    xz
                    `(,zstd "lib")))
      (native-inputs `(("pybind11" ,pybind11-2.10)
                       ("python-pytest" ,python-pytest)
                       ("python-setuptools" ,python-setuptools)
                       ("python-setuptools-scm" ,python-setuptools-scm)
                       ("python-wheel" ,python-wheel)
                       ("bazel-platforms" ,(origin
                                             (method git-fetch)
                                             (uri (git-reference (url
                                                                  "https://github.com/bazelbuild/platforms")
                                                                 (commit
                                                                  "0.0.8")))
                                             (file-name (git-file-name
                                                         "bazel-platforms"
                                                         "0.0.8"))
                                             (sha256 (base32
                                                      "1wx2348w49vxr3z9kjfls5zsrwr0div6r3irbvdlawan87sx5yfs"))))))
      (home-page "https://github.com/google/tensorstore")
      (synopsis
       "Library for reading and writing large multi-dimensional arrays")
      (description
       "TensorStore is a C++ and Python software library
designed for storage and manipulation of large multi-dimensional
arrays that:

@itemize
@item Provides advanced, fully composable indexing operations and
  virtual views.
@item Provides a uniform API for reading and writing multiple array
  formats, including zarr and N5.
@item Natively supports multiple storage systems, such as local and
  network filesystems, Google Cloud Storage, Amazon S3-compatible object
  stores, HTTP servers, and in-memory storage.
@item Offers an asynchronous API to enable high-throughput access even
  to high-latency remote storage.
@item Supports read caching and transactions, with strong atomicity,
  isolation, consistency, and durability (ACID) guarantees.
@item Supports safe, efficient access from multiple processes and
  machines via optimistic concurrency.
@end itemize
")
      (license license:asl2.0))))

(define-public python-keras-for-tensorflow
  (package
    (name "python-keras")
    (version "2.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keras" version))
       (sha256
        (base32 "0s6ciib94x5qinj4pdfr4774yx5jxv055d6xclds25d08712rwax"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;needs tensorflow
      #:phases
      ;; We need this for tensorflow, so we can't have tensorflow
      ;; here, and this causes the sanity check to fail.  That fine,
      ;; because this is not sane.
      '(modify-phases %standard-phases
         (delete 'sanity-check))))
    (propagated-inputs (list python-absl-py
                             python-dm-tree
                             python-h5py
                             python-namex
                             python-numpy
                             python-rich))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "Deep learning API")
    (description
     "Keras is a deep learning API written in Python,
running on top of the machine learning platform TensorFlow.  It was
developed with a focus on enabling fast experimentation and providing
a delightful developer experience.")
    (license license:asl2.0)))

;; Remember to update python-keras-for-tensorflow when upgrading this
;; package.  The versions must match.
(define-public tensorflow
  (let ((tensorflow-system-libs (list
                                 ;; "absl_py"
                                 ;; "astor_archive"
                                 ;; "astunparse_archive"
                                 ;; "boringssl"
                                 ;; "com_github_googlecloudplatform_google_cloud_cpp"
                                 "com_github_grpc_grpc"
                                 ;; "com_google_absl"
                                 ;; "com_google_protobuf"
                                 ;; "com_googlesource_code_re2"
                                 "curl"
                                 "cython"
                                 ;; "dill_archive"
                                 "double_conversion"
                                 "flatbuffers"
                                 ;; "functools32_archive"
                                 "gast_archive"
                                 "gif"
                                 "hwloc"
                                 "icu"
                                 "jsoncpp_git"
                                 "libjpeg_turbo"
                                 "nasm"
                                 "nsync"
                                 "opt_einsum_archive"
                                 ;; "org_sqlite"
                                 "pasta"
                                 "png"
                                 ;; "pybind11" ;Our 2.8.1 does not support "const_name" attribute
                                 "six_archive"
                                 ;; "snappy"
                                 ;; "tblib_archive"
                                 "termcolor_archive"
                                 "typing_extensions_archive"
                                 "wrapt"
                                 "zlib")))
    (package
      (name "tensorflow")
      (version "2.13.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tensorflow/tensorflow/")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09mfskmpvpbq919wibnw3bnhi1y3hkx3qrzm72gdr0gsivn1yb3w"))))
      (build-system bazel-build-system)
      (arguments
       (list
        #:tests? #f ;there are none
        #:bazel-configuration
        ;; TODO: is the union build *really* necessary?
        (with-imported-modules (source-module-closure '((guix build utils)
                                                        (guix build union)
                                                        (guix build
                                                         gnu-build-system)))
                               #~(begin
                                   (use-modules (guix build union))
                                   (union-build (string-append (getenv
                                                                "NIX_BUILD_TOP")
                                                 "/site-packages")
                                                (parse-path (getenv
                                                             "GUIX_PYTHONPATH")))
                                   (setenv "PYTHON_LIB_PATH"
                                           (string-append (getenv
                                                           "NIX_BUILD_TOP")
                                                          "/site-packages"))
                                   (setenv "PYTHON_BIN_PATH"
                                           (string-append #$(this-package-input
                                                             "python-wrapper")
                                                          "/bin/python"))
                                   (setenv "TF_PYTHON_VERSION"
                                           #$(version-major+minor (package-version
                                                                   (this-package-input
                                                                    "python-wrapper"))))
                                   (setenv "TF_SYSTEM_LIBS"
                                           (string-join '#$tensorflow-system-libs
                                                        ","))))
        #:fetch-targets '(list
                          "//tensorflow/tools/pip_package:build_pip_package"
                          "//tensorflow/tools/lib_package:libtensorflow")
        #:build-targets '(list
                          "//tensorflow/tools/pip_package:build_pip_package"
                          "//tensorflow/tools/lib_package:libtensorflow")
        #:bazel-arguments #~(list
                             "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
                             "--action_env=PYTHON_LIB_PATH"
                             "--host_action_env=PYTHON_LIB_PATH"
                             "--action_env=PYTHON_BIN_PATH"
                             "--host_action_env=PYTHON_BIN_PATH"
                             (string-append "--python_path="
                                            #$(this-package-input
                                               "python-wrapper") "/bin/python"))
        #:vendored-inputs-hash
        "0bqlwf9br68mrm5ambnm3dg31gnpsa12wfm2q2gqszknhmk1nyj8"
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack-vendored-inputs 'configure
                       (lambda _
                         (define bazel-out
                           (string-append (getenv "NIX_BUILD_TOP") "/output"))
                         ;; XXX: Our version of protobuf leads to "File already
                         ;; exists in database" when loading in Python.
                         (substitute* (string-append bazel-out
                                       "/external/tf_runtime/third_party/systemlibs/protobuf.BUILD")
                           (("-lprotobuf")
                            "-l:libprotobuf.a")
                           (("-lprotoc")
                            "-l:libprotoc.a"))
                         ;; Do not mess with RUNPATH
                         (substitute* "tensorflow/tools/pip_package/build_pip_package.sh"
                           (("patchelf ")
                            "echo -- "))
                         (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
                         (setenv "USER" "homeless-shelter")
                         (setenv "TF_SYSTEM_LIBS"
                                 (string-join '#$tensorflow-system-libs ","))))
                     (add-after 'build 'install
                       (lambda _
                         ;; Install library
                         (mkdir-p #$output)
                         (invoke "tar" "-xf"
                          "bazel-bin/tensorflow/tools/lib_package/libtensorflow.tar.gz"
                          "-C"
                          #$output)

                         ;; Write pkgconfig file
                         (mkdir-p (string-append #$output "/lib/pkgconfig"))
                         (call-with-output-file (string-append #$output
                                                 "/lib/pkgconfig/tensorflow.pc")
                           (lambda (port)
                             (format port
                              "Name: TensorFlow
Version: ~a
Description: Library for computation using data flow graphs for scalable machine learning
Requires:
Libs: -L~a/lib -ltensorflow
Cflags: -I~a/include/tensorflow
"
                              #$version
                              #$output
                              #$output)))

                         ;; Install python bindings
                         ;; Build the source code, then copy it to the "python" output.
                         ;;
                         ;; TODO: build_pip_package includes symlinks so we must
                         ;; dereference them.
                         (let ((here (string-append (getcwd) "/dist")))
                           (invoke
                            "bazel-bin/tensorflow/tools/pip_package/build_pip_package"
                            "--src" here)
                           (copy-recursively here
                                             #$output:python)))))))
      (outputs '("out" "python"))
      (inputs (list curl
                    double-conversion
                    flatbuffers-23.1
                    giflib
                    grpc
                    hwloc
                    icu4c
                    jsoncpp
                    libjpeg-turbo
                    libpng
                    nasm
                    nsync
                    openssl
                    static-protobuf
                    pybind11
                    python-absl-py
                    python-cython
                    python-numpy
                    python-scipy
                    python-six
                    python-wrapper
                    ;; Wrong version of snappy?
                    ;; external/tsl/tsl/platform/default/port.cc:328:11: error:
                    ;; 'RawCompressFromIOVec' is not a member of 'snappy'; did
                    ;; you mean 'RawUncompressToIOVec'?
                    ;; snappy
                    zlib))
      ;; TODO: these inputs probably should not be propagated.  They are
      ;; only needed for building the Python sources.
      (propagated-inputs (list python-absl-py
                               python-cachetools
                               python-certifi
                               python-charset-normalizer
                               python-flatbuffers
                               python-gast
                               python-google-pasta
                               python-grpcio
                               python-h5py
                               python-idna
                               python-jax
                               python-markdown
                               python-markupsafe
                               python-ml-dtypes
                               python-numpy
                               python-oauthlib
                               python-opt-einsum
                               python-packaging
                               python-portpicker
                               python-protobuf-for-tensorflow-2
                               python-psutil
                               python-pyasn1
                               python-requests
                               python-requests-oauthlib
                               python-rsa
                               python-scipy
                               python-six
                               python-termcolor
                               python-typing-extensions
                               python-urllib3
                               python-werkzeug
                               python-wrapt))
      (native-inputs (list perl python-lit python-pypa-build python-setuptools
                           python-wheel))
      (home-page "https://tensorflow.org")
      (synopsis "Machine learning framework")
      (description
       "TensorFlow is a flexible platform for building and
training machine learning models.  It provides a library for high
performance numerical computation and includes high level Python APIs,
including both a sequential API for beginners that allows users to
build models quickly by plugging together building blocks and a
subclassing API with an imperative style for advanced research.")
      (license license:asl2.0))))

(define-public python-tensorflow
  (package
    (inherit tensorflow)
    (name "python-tensorflow")
    (source
     #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (replace 'unpack
                     (lambda _
                       (mkdir-p "source")
                       (copy-recursively #$tensorflow:python "source")
                       (chdir "source")
                       ;; XXX: the "python" output of the tensorflow package
                       ;; contains broken symlinks.
                       (delete-file-recursively "third_party/eigen3")
                       (mkdir-p "third_party/eigen3")
                       (copy-recursively #$eigen-for-python-ml-dtypes
                                         "third_party/eigen3")
                       (with-output-to-file "third_party/eigen3/LICENSE"
                         (lambda ()
                           (display "")))))
                   (add-after 'unpack 'relax-dependencies
                     (lambda _
                       (substitute* "setup.py"
                         ;; We don't have tensorflow-io yet
                         (("'tensorflow-io-gcs-filesystem.*")
                          "None,")
                         (("'platform_system.*")
                          "")
                         ;; Versions above 0.4 break tests, but that's okay
                         ;; because we aren't running them.
                         (("gast >= 0.2.1, <= 0.4.0")
                          "gast >= 0.2.1")
                         (("'typing_extensions>=3.6.6,<4.6.0'")
                          "'typing_extensions>=3.6.6'")
                         ;; Drop all of tensorboard and tensorflow_estimator
                         (("'(tensorboard|tensorflow_estimator) >.*',")
                          " None,")
                         ;; Our clang bindings have a different name.
                         (("libclang")
                          "clang")
                         ;; No tensorboard, sorry.
                         (("standard_or_nightly\\('tensorboard = tensorboard.main:run_main', None\\),")
                          "")
                         (("'import_pb_to_tensorboard = tensorflow.python.tools.import_pb_to_tensorboard:main',")
                          "")
                         ;; We don't have tensorflow-estimator yet.
                         (("'estimator_ckpt_converter = '")
                          "")
                         (("'tensorflow_estimator.python.estimator.tools.checkpoint_converter:main',")
                          ""))))
                   ;; XXX: this is really ugly, but many shared objects cannot
                   ;; find libtensorflow_framework.so.2 and libbfloat16.so.so
                   (add-after 'unpack 'find-tensorflow-libraries
                     (lambda _
                       ;; XXX: not all .so files need this.
                       (let ((libraries (find-files "." ".*\\.so$")))
                         (for-each (lambda (lib)
                                     (make-file-writable lib)
                                     (system (format #f
                                              "patchelf --set-rpath ~a:~a:$(patchelf --print-rpath ~a) ~a"
                                              ;; for libtensorflow_framework.so.2
                                              (string-append #$(this-package-input
                                                                "tensorflow")
                                                             "/lib")
                                              ;; for libbfloat16.so.so
                                              (string-append #$output
                                               "/lib/python3.10/site-packages/tensorflow/tsl/python/lib/core/")
                                              lib
                                              lib))) libraries))))
                   (add-after 'install 'install-missing-libraries
                     (lambda _
                       ;; libtensorflow_cc.so.2 is not installed.  See
                       ;; https://github.com/tensorflow/tensorflow/issues/60326.
                       (let ((dir (string-append #$output
                                   "/lib/python3.10/site-packages/tensorflow/"))
                             (lib (string-append "libtensorflow_cc.so."
                                                 #$(package-version
                                                    this-package))))
                         (install-file (string-append "tensorflow/" lib) dir)
                         (with-directory-excursion dir
                           (symlink lib "libtensorflow_cc.so.2"))))))))
    (outputs '("out"))
    (propagated-inputs (modify-inputs (package-propagated-inputs tensorflow)
                         (append python-clang-13 python-keras-for-tensorflow)))
    (inputs (list tensorflow))
    (native-inputs (list eigen-for-python-ml-dtypes patchelf
                         `(,tensorflow "python")))))

;; This package provides *independent* modules that are meant to be
;; imported selectively.  Each module has its own Bazel BUILD file,
;; but no separate pyproject.toml.  Bundling everything up as a single
;; package defeats the original purpose.  We should probably split
;; this up into as many packages as there are modules.
(define-public python-etils
  (package
    (name "python-etils")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/etils/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xhnsr4n6dxsn25jiblf5qpk8jj9rgm4yb3gq4zyxffqxd1nlplg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; The lazy imports require python-dataclass-array, but that
      ;; package needs etils at build time.
      '(list "--ignore=etils/epy/lazy_imports_utils_test.py"
             ;; This needs internet access
             "-k" "not test_public_access")))
    (inputs (list ffmpeg-5)) ;for mediapy
    (propagated-inputs (list jupyter
                             python-absl-py
                             python-chex
                             python-dm-tree
                             python-fsspec
                             python-importlib-resources
                             python-jax
                             python-mediapy
                             python-numpy
                             python-optree
                             python-packaging
                             python-protobuf
                             python-pylint
                             python-pytorch
                             python-simple-parsing
                             python-tensorflow
                             python-tqdm
                             python-typing-extensions
                             python-zipp))
    (native-inputs (list python-flit-core python-pytest python-pytest-subtests
                         python-pytest-xdist))
    (home-page "https://github.com/google/etils/")
    (synopsis "Collection of common Python utils")
    (description "This is a collection of independent Python modules
providing utilities for various projects.")
    (license license:asl2.0)))

(define python-jaxlib/wheel
  (let ((jaxlib-system-libs (list "absl_py"
                                  "com_github_grpc_grpc"
                                  "curl"
                                  "cython"
                                  "double_conversion"
                                  "flatbuffers"
                                  "gast_archive"
                                  "gif"
                                  "hwloc"
                                  "icu"
                                  "jsoncpp_git"
                                  "libjpeg_turbo"
                                  "lmdb"
                                  "zlib")))
    (package
      (name "python-jaxlib")
      (version "0.4.20")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/jax")
               (commit (string-append "jaxlib-v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15dmxmfjybg1289v822cmk9raagl9mcbkjk990xa0f91sx91gdjq"))))
      (build-system bazel-build-system)
      (arguments
       (list
        #:tests? #f ;there are none
        #:bazel-configuration #~(setenv "TF_SYSTEM_LIBS"
                                        (string-join '#$jaxlib-system-libs ","))
        #:fetch-targets '(list "//jaxlib/tools:build_wheel"
                               "@mkl_dnn_v1//:mkl_dnn")
        #:build-targets '(list "//jaxlib/tools:build_wheel")
        #:run-command #~(list (string-append "--output_path="
                                             #$output)
                              (string-append "--cpu="
                                             #$(match (or (%current-target-system)
                                                          (%current-system))
                                                 ("x86_64-linux" "x86_64")
                                                 ("i686-linux" "i686")
                                                 ("mips64el-linux" "mips64")
                                                 ("aarch64-linux" "aarch64"))))
        #:bazel-arguments #~(list "-c"
                                  "opt"
                                  ;; We need a more recent version of platforms, because the
                                  ;; included cpu package does not define cpu:wasm32.
                                  (string-append
                                   "--override_repository=platforms="
                                   #$(this-package-native-input
                                      "bazel-platforms"))
                                  "--config=mkl_open_source_only"
                                  (string-append "--define="
                                                 "PROTOBUF_INCLUDE_PATH="
                                                 #$static-protobuf "/include"))
        #:vendored-inputs-hash
        "1fa4f8qx0765zdwmqaz1jnc60nvb3j4qxqy0mxrpqj58qdclycfs"
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack-vendored-inputs 'configure
                       (lambda _
                         ;; XXX: Our version of protobuf leads to "File already
                         ;; exists in database" when loading jax in Python.
                         ;; Using the static library is what Nix does, but it
                         ;; doesn't help us.
                         (let ((bazel-out (string-append (getenv
                                                          "NIX_BUILD_TOP")
                                                         "/output")))
                           (setenv "JAXLIB_RELEASE" "1")
                           (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")
                           (setenv "TF_SYSTEM_LIBS"
                                   (string-join '#$jaxlib-system-libs ","))
                           (call-with-output-file ".jax_configure.bazelrc"
                             (lambda (port)
                               ;; build --define PROTOBUF_INCLUDE_PATH=" #$(this-package-input "protobuf") "/include
                               (display (string-append
                                         "
build --strategy=Genrule=standalone
build --repo_env PYTHON_BIN_PATH="
                                         #$(this-package-input
                                            "python-wrapper")
                                         "/bin/python\nbuild --python_path="
                                         #$(this-package-input
                                            "python-wrapper")
                                         "/bin/python
build --distinct_host_configuration=false
build --features=-layering_check
build --experimental_strict_java_deps=off
build --strict_proto_deps=off
build --config=mkl_open_source_only
build --toolchain_resolution_debug=\".*\"
build --local_ram_resources=HOST_RAM*.5
build --local_cpu_resources=HOST_CPUS*.75
")
                                        port)))))))))
      (inputs (list curl
                    double-conversion
                    flatbuffers
                    giflib
                    grpc
                    hwloc
                    icu4c
                    jsoncpp
                    libjpeg-turbo
                    openssl
                    ;; XXX: With our own version of Protobuf we see this error
                    ;; on "import jax" (downstream of this package):
                    ;;
                    ;; [libprotobuf ERROR google/protobuf/descriptor_database.cc:642] File already exists in database: xla/xla_data.proto
                    ;; [libprotobuf FATAL google/protobuf/descriptor.cc:1984] CHECK failed: GeneratedDatabase()->Add(encoded_file_descriptor, size):
                    ;; terminate called after throwing an instance of 'google::protobuf::FatalException'
                    ;; what():  CHECK failed: GeneratedDatabase()->Add(encoded_file_descriptor, size):
                    ;; protobuf-3.20
                    ;; `(,protobuf-3.20 "static")
                    pybind11
                    python-absl-py
                    python-numpy
                    python-scipy
                    python-six
                    python-wrapper
                    ;; Wrong version of snappy?
                    ;; external/tsl/tsl/platform/default/port.cc:328:11: error:
                    ;; 'RawCompressFromIOVec' is not a member of 'snappy'; did
                    ;; you mean 'RawUncompressToIOVec'?
                    ;; snappy
                    zlib))
      (propagated-inputs (list python-absl-py
                               python-importlib-metadata
                               python-gast
                               python-ml-dtypes
                               python-numpy
                               python-opt-einsum
                               python-protobuf-for-tensorflow-2
                               python-scipy))
      (native-inputs `(("python-pypa-build" ,python-pypa-build)
                       ("python-setuptools" ,python-setuptools)
                       ("python-wheel" ,python-wheel)
                       ("bazel-platforms" ,(origin
                                             (method git-fetch)
                                             (uri (git-reference (url
                                                                  "https://github.com/bazelbuild/platforms")
                                                                 (commit
                                                                  "0.0.8")))
                                             (file-name (git-file-name
                                                         "bazel-platforms"
                                                         "0.0.8"))
                                             (sha256 (base32
                                                      "1wx2348w49vxr3z9kjfls5zsrwr0div6r3irbvdlawan87sx5yfs"))))))
      (home-page "https://github.com/google/jax")
      (synopsis "Differentiate, compile, and transform Numpy code.")
      (description
       "JAX is Autograd and XLA, brought together for
high-performance numerical computing, including large-scale machine
learning research.  With its updated version of Autograd, JAX can
automatically differentiate native Python and NumPy functions. It can
differentiate through loops, branches, recursion, and closures, and it
can take derivatives of derivatives of derivatives. It supports
reverse-mode differentiation (a.k.a. backpropagation) via grad as well
as forward-mode differentiation, and the two can be composed
arbitrarily to any order.")
      (license license:asl2.0))))

(define-public python-jaxlib
  (package
    (inherit python-jaxlib/wheel)
    (source
     #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'unpack)
                   (replace 'build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (mkdir-p "dist")
                       (install-file (car (find-files (assoc-ref inputs
                                                       "python-jaxlib")
                                                      "\\.whl$")) "dist"))))))
    (native-inputs (list python-jaxlib/wheel))))

;; Keep in sync with jaxlib above
(define-public python-jax
  (package
    (name "python-jax")
    (version "0.4.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jax" version))
       (sha256
        (base32 "1b6j3svq35f06iygc8nh3k862d0nvss9l5fi7533gadim1isg5pa"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;unclear how to run them
    (propagated-inputs (list python-importlib-metadata
                             python-jaxlib
                             python-ml-dtypes
                             python-numpy
                             python-opt-einsum
                             python-scipy))
    (home-page "https://github.com/google/jax")
    (synopsis "Differentiate, compile, and transform Numpy code")
    (description
     "JAX is Autograd and XLA, brought together for
high-performance numerical computing, including large-scale machine
learning research.  With its updated version of Autograd, JAX can
automatically differentiate native Python and NumPy functions. It can
differentiate through loops, branches, recursion, and closures, and it
can take derivatives of derivatives of derivatives. It supports
reverse-mode differentiation (a.k.a. backpropagation) via grad as well
as forward-mode differentiation, and the two can be composed
arbitrarily to any order.")
    (license license:asl2.0)))

(define-public python-jmp
  (package
    (name "python-jmp")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jmp" version))
       (sha256
        (base32 "0c0p7srwx19lzwvl0by016l35bmz1jfsn2pz1ykp57vsgkyv1zjx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test fails. The error is "Unable to resolve runtime symbol:
      ;; `truncsfhf2', which looks more like a toolchain error.
      '(list "--ignore=jmp/_src/policy_test.py")))
    (propagated-inputs (list python-absl-py python-dataclasses python-jax
                             python-jaxlib python-numpy))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/google-deepmind/jmp")
    (synopsis "JMP is a mixed precision library for JAX")
    (description
     "This library implements support for mixed precision training in JAX.
It provides two key abstractions.  These abstractions are mixed
precision policies and loss scaling.")
    (license license:asl2.0)))

(define-public python-chex
  (package
    (name "python-chex")
    ;; A newer version exists but is not compatible with our numpy.
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "chex" version))
       (sha256
        (base32 "0y9a2jjbjih2g4dn8q66y0b2b5kzxjrnir8x68cj0fcgnkyr80sr"))))
    (build-system pyproject-build-system)
    ;; Our version of np.testing.assert_array_equal does not support
    ;; the "strict" argument.
    (arguments
     (list
      #:test-flags '(list "-k" "not test_assert_trees_all_equal_strict_mode")
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'compatibility
                    (lambda _
                      (substitute* "chex/_src/asserts.py"
                        (("strict=strict")
                         "")))))))
    (propagated-inputs (list python-absl-py
                             python-jax
                             python-jaxlib
                             python-numpy
                             python-toolz
                             python-typing-extensions))
    (native-inputs (list python-cloudpickle python-dm-tree python-pytest))
    (home-page "https://github.com/deepmind/chex")
    (synopsis "Chex: Testing made fun, in JAX!")
    (description
     "Chex is a library of utilities for helping to write
reliable JAX code.  This includes utils to help:

@itemize
@item Instrument your code (e.g. assertions)
@item Debug (e.g. transforming @code{pmaps} in @code{vmaps} within a
  context manager).
@item Test JAX code across many @code{variants} (e.g. jitted vs
  non-jitted).
@end itemize
")
    (license license:asl2.0)))

(define-public python-optax
  (package
    (name "python-optax")
    ;; 0.1.6 needs a more recent numpy
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "optax" version))
       (sha256
        (base32 "0bhgaaxvqli3b2081zp9ycb2c2hqba0fcpigaqjxbnsidyspk8qa"))))
    (build-system pyproject-build-system)
    ;; Tests require haiku, tensorflow, and flax, but flax needs
    ;; optax.
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-absl-py python-chex python-jax
                             python-jaxlib python-numpy))
    (native-inputs (list python-dm-tree python-pytest))
    (home-page "https://github.com/google-deepmind/optax/")
    (synopsis "Gradient processing and optimization library for JAX")
    (description "Optax is a gradient processing and optimization
library for JAX.")
    (license license:asl2.0)))

(define-public python-orbax-checkpoint
  (package
    (name "python-orbax-checkpoint")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/orbax")
             (commit "e68a1cd8c997caccc87fc5c1134847294be45ead")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02njj5jkcg5j58krj6z2y6sfi49zd9ic8r7v34fnbgkr648ay87q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests require flax, but flax needs orbax.  Luckily there's a
      ;; goal to remove the dependency on flax as evidenced by this
      ;; comment in utils_test.py:
      ;;
      ;; # TODO(b/275613424): Eliminate flax dependency in Orbax
      ;; test suite.
      ;;
      ;; One can only hope.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'chdir
                     (lambda _
                       (chdir "checkpoint"))))))
    (propagated-inputs (list python-absl-py
                             python-cached-property
                             python-etils
                             python-importlib-resources
                             python-jax
                             python-jaxlib
                             python-msgpack
                             python-nest-asyncio
                             python-numpy
                             python-pyyaml
                             python-tensorstore
                             python-typing-extensions))
    (native-inputs (list python-flit-core python-pytest python-pytest-xdist))
    (home-page "https://github.com/google/orbax")
    (synopsis "Utility libraries for JAX users")
    (description
     "Orbax is a namespace providing common utility
libraries for JAX users.  Orbax also includes a serialization library
for JAX users, enabling the exporting of JAX models to the TensorFlow
SavedModel format.")
    (license license:asl2.0)))

(define-public python-flax
  (package
    (name "python-flax")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/flax")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yqi7b8wmnvz0kgbb0pn1iwjr0l72crxfbch37b315g08wkwcn3z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "--pyargs"
                          "tests"
                          ;; We don't have tensorboard
                          "--ignore=tests/tensorboard_test.py"
                          ;; These tests are failing bacause flax might only work
                          ;; on CPUs that have AVX support.
                          "--ignore=tests/serialization_test.py"
                          "--ignore=tests/linen/linen_test.py"
                          "--ignore=tests/linen/linen_recurrent_test.py"
                          "--ignore=tests/linen/linen_dtypes_test.py"
                          ;; These tests try to use a fixed number of CPUs that may
                          ;; exceed the number of CPUs available at build time.
                          "--ignore=tests/jax_utils_test.py")
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'ignore-deprecations
                    (lambda _
                      (substitute* "pyproject.toml"
                        (("\"error\",")
                         "")))))))
    (propagated-inputs (list python-einops
                             python-jax
                             python-optax
                             python-orbax-checkpoint
                             python-msgpack
                             python-numpy
                             python-pyyaml
                             python-rich
                             python-tensorstore
                             python-typing-extensions))
    (native-inputs (list opencv
                         python-nbstripout
                         python-ml-collections
                         python-mypy
                         python-pytorch
                         python-pytest
                         python-pytest-cov
                         python-pytest-xdist
                         python-setuptools-scm
                         python-tensorflow))
    (home-page "https://github.com/google/flax")
    (synopsis "Neural network library for JAX designed for flexibility")
    (description "Flax is a neural network library for JAX that is
designed for flexibility.")
    (license license:asl2.0)))

(define-public python-dm-haiku
  (package
    (name "python-dm-haiku")
    (version "0.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google-deepmind/dm-haiku")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qmnzxb7i8826im3ns8dwpga9x1k60xrnwsv3nzjwf0mvfybbxy6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "--ignore-glob=examples/**"
                          ;; These fail because we don't use tf-nightly.
                          "--ignore=haiku/_src/integration/jax2tf_test.py"
                          ;; extendhfsf2 not found due to the mismatch in serialization version.
                          "--ignore=haiku/_src/mixed_precision_test.py"
                          ;; truncsfhf2 not found, which looks more like a toolchain error.
                          "--ignore=haiku/_src/reshape_test.py"
                          ;; Disable doctests with unreasonable requirements on tf-nightly
                          "--ignore=haiku/_src/integration/doctest_test.py")))
    (propagated-inputs (list python-absl-py
                             python-chex
                             python-cloudpickle
                             python-dill
                             python-dm-tree
                             python-flax
                             python-jax
                             python-jaxlib
                             python-jmp
                             python-numpy
                             python-optax
                             python-tabulate
                             python-tensorflow
                             python-virtualenv))
    (native-inputs (list python-mock python-pytest-xdist))
    (home-page "https://github.com/google-deepmind/dm-haiku")
    (synopsis "Sonnet for JAX")
    (description
     "Haiku is a simple neural network library for JAX.
It is developed by some of the authors of Sonnet, a neural network
library for TensorFlow.")
    (license license:asl2.0)))

