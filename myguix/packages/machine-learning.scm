(define-module (myguix packages machine-learning)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module ((gnu packages bioinformatics)
                #:hide (seek))
  #:use-module (gnu packages c)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages check)
  #:use-module ((gnu packages compression)
                #:hide (miniz-for-pytorch))
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-science)
  #:use-module ((gnu packages python-web)
                #:hide (python-jose))
  #:use-module ((gnu packages python-xyz)
                #:hide (python-future))
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
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
  #:use-module (myguix packages compression)
  #:use-module (myguix packages llm)
  #:use-module (myguix packages llvm-pqrs)
  #:use-module (myguix packages maths)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages video)
  #:use-module (myguix packages bazel)
  #:use-module (myguix packages huggingface)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-eigen-package
            (lambda _
              (substitute* "setup.py"
                (("third_party/eigen")
                 (string-append #$(this-package-input
                                   "eigen-for-python-ml-dtypes")
                                "/include/eigen3"))))))))
    (inputs (list eigen-for-python-ml-dtypes))
    (propagated-inputs (list python-numpy))
    (native-inputs (list pybind11
                         python-absl-py
                         python-pylint
                         python-pytest
                         python-pytest-xdist
                         python-setuptools
                         python-wheel))
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
(define-public python-protobuf-for-tensorflow
  (package
    (name "python-protobuf-for-tensorflow")
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
    (native-inputs (list python-numpy
                         python-pytest
                         python-pytest-benchmark
                         python-pytest-regressions
                         python-pytest-xdist
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/lebrice/SimpleParsing")
    (synopsis
     "Utility for simplifying and cleaning up argument parsing scripts")
    (description "This package provides a small utility for simplifying and
cleaning up argument parsing scripts.")
    (license license:expat)))

(define-public python-bitsandbytes
  (package
    (name "python-bitsandbytes")
    (version "0.44.1")
    (home-page "https://github.com/bitsandbytes-foundation/bitsandbytes")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jd7nsih9z2zp0aa82sl5kgxaqyjzlgv7hhfbrw9lawc57kl7z6a"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda _
              (invoke "cmake" "-DCOMPUTE_BACKEND=cpu" "-S" ".")
              (invoke "make")))
          (delete 'strip-binaries)
          (add-after 'install 'add-libbitsandbytes_cpu.so
            (lambda _
              (let* ((site (string-append #$output "/lib/python"
                                          #$(version-major+minor (package-version
                                                                  python))
                                          "/site-packages/bitsandbytes")))
                (install-file "bitsandbytes/libbitsandbytes_cpu.so" site)))))))
    (inputs (list python-setuptools
                  python-pytest
                  python-lion-pytorch
                  python-einops
                  python-wheel
                  python-scipy
                  python-pandas
                  python-matplotlib))
    (native-inputs (list cmake-minimal))
    (synopsis "Fuzzy matching library for Python")
    (description
     "The @code{bitsandbytes} library is a lightweight Python wrapper around CUDA custom functions, in particular 8-bit optimizers, matrix multiplication (LLM.int8()), and 8 & 4-bit quantization functions.

The library includes quantization primitives for 8-bit & 4-bit operations, through @code{bitsandbytes.nn.Linear8bitLt} and @code{bitsandbytes.nn.Linear4bit} and 8-bit optimizers through @code{bitsandbytes.optim module}.

There are ongoing efforts to support further hardware backends, i.e. Intel CPU + GPU, AMD GPU, Apple Silicon. Windows support is quite far along and is on its way as well.

Please head to the official documentation page: @url{https://huggingface.co/docs/bitsandbytes/main}")
    (license license:asl2.0)))

(define-public python-bitsandbytes-cuda
  (package
    (inherit python-bitsandbytes)
    (name "python-bitsandbytes-cuda")
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda _
              (invoke "cmake" "-DCOMPUTE_BACKEND=cuda"
                      "-DCOMPUTE_CAPABILITY=80;86" "-S" ".")
              (invoke "make")))
          (delete 'strip-binaries)
          (add-after 'install 'add-libbitsandbytes_cuda124.so
            (lambda _
              (let* ((site (string-append #$output "/lib/python"
                                          #$(version-major+minor (package-version
                                                                  python))
                                          "/site-packages/bitsandbytes")))
                (install-file "bitsandbytes/libbitsandbytes_cuda124.so" site)
                (install-file (string-append #$(this-package-input
                                                "python-bitsandbytes")
                               "/lib/python"
                               #$(version-major+minor (package-version python))
                               "/site-packages/bitsandbytes/libbitsandbytes_cpu.so")
                              site)))))))
    (inputs (modify-inputs (package-inputs python-bitsandbytes)
              (replace "python-lion-pytorch" python-lion-pytorch-cuda)
              (append python-bitsandbytes)))
    (propagated-inputs (list cuda-toolkit))))

(define-public python-sarif-om
  (package
    (name "python-sarif-om")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sarif_om" version))
       (sha256
        (base32 "167gb8xjm0310km3w1s12bqldbv7zyklkr4j5900vq4361ml2pyd"))))
    (build-system python-build-system)
    (propagated-inputs (list python-attrs))
    (native-inputs (list python-pbr))
    (home-page "https://github.com/microsoft/sarif-python-om")
    (synopsis "Python implementation of the SARIF 2.1.0 object model")
    (description
     "This module contains classes for the object model defined
by the @url{https://sarifweb.azurewebsites.net,Static Analysis Results
Interchange Format (SARIF)} file format.")
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
        #:modules '((myguix build bazel-build-system)
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
        #:bazel-configuration
        #~(begin
            ;; Make numpy headers available at expected location.  See
            ;; snippet above for more information.
            (let ((python-version #$(version-major+minor (package-version (this-package-input
                                                                           "python-wrapper")))))
              (copy-recursively (string-append #$(this-package-input
                                                  "python-numpy")
                                 "/lib/python" python-version
                                 "/site-packages/numpy/core/include/numpy")
                                "/tmp/numpy-include"))
            ;; You can get the list of possible values of
            ;; TENSORSTORE_SYSTEM_PYTHON_LIBS and
            ;; TENSORSTORE_SYSTEM_LIBS by searching the tensorstore
            ;; checkout for system_build_file.  Any match is a possible
            ;; replacement.
            (setenv "TENSORSTORE_SYSTEM_PYTHON_LIBS"
                    (string-join '#$tensorstore-python-packages ","))
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
        #:bazel-arguments
        #~(list "-c"
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
                          #$(this-package-input "python-wrapper")
                          "/bin/python"))
        #:vendored-inputs-hash
        "1sdwk3fnf0gfk1h2fb5082r87ahzhswznj21p9d1y0g0x97hw6zk"
        #:phases
        #~(modify-phases (@ (myguix build bazel-build-system) %standard-phases)
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
                (let ((python-version #$(version-major+minor (package-version (this-package-input
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
      (native-inputs `(("pybind11" ,pybind11)
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
        #:bazel-arguments
        #~(list
           "--extra_toolchains=@bazel_tools//tools/python:autodetecting_toolchain_nonstrict"
           "--action_env=PYTHON_LIB_PATH"
           "--host_action_env=PYTHON_LIB_PATH"
           "--action_env=PYTHON_BIN_PATH"
           "--host_action_env=PYTHON_BIN_PATH"
           (string-append "--python_path="
                          #$(this-package-input "python-wrapper")
                          "/bin/python"))
        #:vendored-inputs-hash
        "0bqlwf9br68mrm5ambnm3dg31gnpsa12wfm2q2gqszknhmk1nyj8"
        #:phases
        #~(modify-phases %standard-phases
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
                    flatbuffers
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
                               python-protobuf-for-tensorflow
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

(define-public tensorflow-cuda
  (package
    (inherit tensorflow)
    (name "tensorflow-cuda")
    (version (package-version tensorflow))
    (arguments
     (substitute-keyword-arguments (package-arguments tensorflow)
       ((#:bazel _ #f)
        bazel-6.1)
       ((#:vendored-inputs-hash _)
        "02hypf92w8sp8f2bpc9x2xb0px18i0vsw5022xgwdq1l8vs0cap9")
       ((#:bazel-configuration conf)
        #~(begin
            #$conf
            ;; When building with CUDA, Bazel uses ldconfig and
            ;; complains that it can't open /etc/ld.so.cache.
            ;; So we fake ldconfig.
            (mkdir-p "/tmp/dummy-ldconfig")
            (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
            (setenv "PATH"
                    (string-append "/tmp/dummy-ldconfig:"
                                   (getenv "PATH")))))
       ((#:bazel-arguments args)
        #~(append #$args
                  (list "--config=cuda"
                        "--local_ram_resources=8192"
                        "--action_env=TF_NEED_CUDA=1"
                        (string-append "--action_env=TF_CUDA_PATHS="
                                       #$(this-package-input "cuda-toolkit")
                                       ","
                                       #$(this-package-input "cudnn"))
                        (string-append "--action_env=TF_CUDA_VERSION="
                                       #$(version-major+minor (package-version
                                                               (this-package-input
                                                                "cuda-toolkit"))))
                        (string-append "--action_env=TF_CUDNN_VERSION="
                                       #$(version-major (package-version (this-package-input
                                                                          "cudnn")))))))
       ((#:phases phases)
        (with-imported-modules (source-module-closure '((guix build utils)
                                                        (guix build union)
                                                        (guix build
                                                         gnu-build-system)
                                                        (guix-science build
                                                         bazel-build-system)))
                               #~(modify-phases #$phases
                                   (add-before 'configure 'patch-compiler-wrapper
                                     (lambda _
                                       (let ((bazel-out (string-append (getenv
                                                                        "NIX_BUILD_TOP")
                                                         "/output")))
                                         (patch-shebang
                                          "third_party/gpus/crosstool/clang/bin/crosstool_wrapper_driver_is_not_gcc.tpl")

                                         ;; This wrapper insists on passing
                                         ;; no-canonical-prefixes, which makes it
                                         ;; impossible for GCC to find
                                         ;; architecture-specific headers like
                                         ;; bits/c++config.h.
                                         (substitute* "third_party/gpus/crosstool/cc_toolchain_config.bzl.tpl"
                                           (("\"-no-canonical-prefixes\",")
                                            "")))))
                                   ;; XXX: this should be a function of the features
                                   ;; supported by the given CUDA library version.  Our
                                   ;; CUDA 11 version does not support the virtual
                                   ;; architecture "compute_90", for example.
                                   (add-after 'configure 'set-cuda-capabilities
                                     (lambda _
                                       (setenv "TF_CUDA_COMPUTE_CAPABILITIES"
                                        "sm_52,sm_60,sm_70,sm_80,compute_90")))
                                   (add-after 'set-cuda-capabilities 'configure-with-cuda
                                     (lambda _
                                       ;; When building with CUDA, Bazel uses ldconfig and
                                       ;; complains that it can't open /etc/ld.so.cache.
                                       ;; So we fake ldconfig.
                                       (mkdir-p "/tmp/dummy-ldconfig")
                                       (symlink (which "true")
                                        "/tmp/dummy-ldconfig/ldconfig")
                                       (setenv "PATH"
                                               (string-append
                                                "/tmp/dummy-ldconfig:"
                                                (getenv "PATH")))
                                       ;; Bazel expects the GCC and CUDA toolchains to be
                                       ;; under the same prefix.
                                       (use-modules (guix build union))
                                       (let ((toolchain (string-append (getenv
                                                                        "NIX_BUILD_TOP")
                                                         "/toolchain")))
                                         (union-build toolchain
                                                      (cons #$(this-package-input
                                                               "cuda-toolkit")
                                                            (match '#$(standard-packages)
                                                              (((labels directories . rest)
                                                                ...)
                                                               directories))))
                                         (setenv "GCC_HOST_COMPILER_PREFIX"
                                                 (string-append toolchain
                                                                "/bin"))
                                         (setenv "GCC_HOST_COMPILER_PATH"
                                                 (string-append toolchain
                                                                "/bin/gcc")))))
                                   (replace 'install
                                     (lambda _
                                       ;; Install library
                                       (mkdir-p #$output)
                                       (invoke "tar" "-xf"
                                        "bazel-bin/tensorflow/tools/lib_package/libtensorflow.tar.gz"
                                        "-C"
                                        #$output)

                                       ;; Write pkgconfig file
                                       (mkdir-p (string-append #$output
                                                 "/lib/pkgconfig"))
                                       (call-with-output-file (string-append #$output
                                                               "/lib/pkgconfig/tensorflow.pc")
                                         (lambda (port)
                                           (format port
                                            "Name: TensorFlow CUDA
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
                                       (let ((here (string-append (getcwd)
                                                                  "/dist")))
                                         (invoke
                                          "bazel-bin/tensorflow/tools/pip_package/build_pip_package"
                                          "--gpu" "--src" here)
                                         (copy-recursively here
                                                           #$output:python)))))))))
    (inputs (modify-inputs (package-inputs tensorflow)
              (replace "python-jax" python-jax-cuda)
              ;; See compatibility matrix here:
              ;; https://www.tensorflow.org/install/source#gpu
              (append cuda-toolkit-11.8 cudnn-8.6)))
    ;; For crosstool_wrapper_driver_is_not_gcc
    (native-inputs (modify-inputs (package-native-inputs tensorflow)
                     (append python-wrapper)))))

(define python-keras-for-tensorflow
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
                             python-rich
                             python-setuptools))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "Deep learning API")
    (description
     "Keras is a deep learning API written in Python,
running on top of the machine learning platform TensorFlow.  It was
developed with a focus on enabling fast experimentation and providing
a delightful developer experience.")
    (license license:asl2.0)))

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
      #:phases
      #~(modify-phases %standard-phases
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
                (("numpy >= 1.22, <= 1.24.3")
                 "numpy >= 1.22")
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
                                                       "tensorflow") "/lib")
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
                                        #$(package-version this-package))))
                (install-file (string-append "tensorflow/" lib) dir)
                (with-directory-excursion dir
                  (symlink lib "libtensorflow_cc.so.2"))))))))
    (outputs '("out"))
    (propagated-inputs (modify-inputs (package-propagated-inputs tensorflow)
                         (append python-astunparse python-clang-13
                                 python-keras-for-tensorflow)))
    (inputs (list tensorflow))
    (native-inputs (list eigen-for-python-ml-dtypes patchelf
                         `(,tensorflow "python") python-setuptools
                         python-wheel))))

(define-public python-tensorflow-cuda
  (package
    (inherit tensorflow-cuda)
    (name "python-tensorflow-cuda")
    (source
     #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (mkdir-p "source")
              (copy-recursively #$tensorflow-cuda:python "source")
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
                                                       "tensorflow-cuda")
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
                                        #$(package-version this-package))))
                (install-file (string-append "tensorflow/" lib) dir)
                (with-directory-excursion dir
                  (symlink lib "libtensorflow_cc.so.2"))))))))
    (outputs '("out"))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       tensorflow-cuda)
                         (append python-clang-13 python-keras-for-tensorflow)))
    (inputs (list tensorflow-cuda))
    (native-inputs (list eigen-for-python-ml-dtypes patchelf
                         `(,tensorflow-cuda "python")))))

(define* (pypi-wheel-url name
                         version
                         #:optional (dist "py3")
                         (python "py3")
                         (abi "none")
                         (platform "any"))
  (string-append "https://files.pythonhosted.org/packages"
                 "/"
                 dist
                 "/"
                 (string-take name 1)
                 "/"
                 name
                 "/"
                 name
                 "-"
                 version
                 "-"
                 python
                 "-"
                 abi
                 "-"
                 platform
                 ".whl"))

;; This package is a stop-gap that builds Tensorboard from the pre-built wheels, instead of
;; packaging it from scratch, which requires a Bazel+NPM build
(define-public python-tensorboard-data-server
  (package
    (name "python-tensorboard-data-server")
    (version "0.7.2")
    (build-system pyproject-build-system)
    (source
     (origin
       (method url-fetch)
       (uri (pypi-wheel-url "tensorboard_data_server" version))
       (sha256
        (base32 "1nqdimbqzdzbjklzggvvgrxzk04f17f0bv1n72c8i5c80p9101ky"))
       (file-name (string-append name "-" version ".whl"))))
    (arguments
     (list
      #:tests? #f ;Tests are not distributed with the wheel
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key source #:allow-other-keys)
              (mkdir-p "dist")
              (install-file source "dist"))))))
    (home-page "https://www.tensorflow.org")
    (synopsis "Fast data loading for TensorBoard")
    (description
     "The Tensorboard Data Server is the backend component of TensorBoard that efficiently processes
and serves log data. It improves TensorBoard's performance by handling large-scale event files
asynchronously, enabling faster data loading and reduced memory usage.")
    (license license:asl2.0)))

;; This package is a stop-gap that builds Tensorboard from the pre-built wheels, instead of
;; packaging it from scratch, which requires a Bazel+NPM build
(define-public python-tensorboard
  (package
    (name "python-tensorboard")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-wheel-url "tensorboard" version))
       (sha256
        (base32 "1asyfhkd7xw2yvk122vrkzrr9dm9f3zm0b50xwm3xxs52y1a8z0h"))
       (file-name (string-append name "-" version ".whl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;Tests are not distributed with the wheel
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key source #:allow-other-keys)
              (mkdir-p "dist")
              (install-file source "dist")))

          (add-before 'sanity-check 'relax-dependencies
            (lambda* (#:key outputs #:allow-other-keys)
              (for-each (lambda (metadata)
                          (substitute* metadata
                            ;; Current grpcio in Guix is 1.47
                            (("grpcio>=1.48.2")
                             "grpcio")))
                        (find-files (string-append #$output "/lib") "METADATA")))))))

    (propagated-inputs (list python-absl-py
                             python-grpcio
                             python-markdown
                             python-numpy
                             python-packaging
                             python-protobuf
                             python-six
                             python-tensorboard-data-server
                             python-werkzeug
                             python-setuptools)) ;Not declared, but used at runtime
    (home-page "https://www.tensorflow.org")
    (synopsis "TensorFlow's Visualization Toolkit")
    (description
     "Tensorboard is a visualization toolkit for TensorFlow, designed to provide metrics tracking,
model visualization, and performance analysis. It allows users to generate interactive dashboards
for monitoring training progress, visualizing computational graphs, and analyzing data distributions.")
    (license license:asl2.0)))

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
      #:tests? #f))
    (inputs (list ffmpeg-5)) ;for mediapy
    (propagated-inputs (list jupyter
                             python-absl-py
                             python-dm-tree
                             python-fsspec
                             python-importlib-resources
                             python-mediapy
                             python-numpy
                             python-optree
                             python-packaging
                             python-protobuf
                             python-pylint
                             python-pytorch
                             python-simple-parsing
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

(define-public python-etils-cuda
  (package
    (inherit python-etils)
    (name "python-etils-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-etils)
                         (replace "python-pytorch" python-pytorch-cuda)))))

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
        #:bazel-configuration
        #~(setenv "TF_SYSTEM_LIBS"
                  (string-join '#$jaxlib-system-libs ","))
        #:fetch-targets '(list "//jaxlib/tools:build_wheel"
                               "@mkl_dnn_v1//:mkl_dnn")
        #:build-targets '(list "//jaxlib/tools:build_wheel")
        #:run-command
        #~(list (string-append "--output_path="
                               #$output)
                (string-append "--cpu="
                               #$(match (or (%current-target-system)
                                            (%current-system))
                                   ("x86_64-linux" "x86_64")
                                   ("i686-linux" "i686")
                                   ("mips64el-linux" "mips64")
                                   ("aarch64-linux" "aarch64"))))
        #:bazel-arguments
        #~(list "-c"
                "opt"
                ;; We need a more recent version of platforms, because the
                ;; included cpu package does not define cpu:wasm32.
                (string-append "--override_repository=platforms="
                               #$(this-package-native-input "bazel-platforms"))
                "--config=mkl_open_source_only"
                (string-append "--define=" "PROTOBUF_INCLUDE_PATH="
                               #$static-protobuf "/include"))
        #:vendored-inputs-hash
        "1fa4f8qx0765zdwmqaz1jnc60nvb3j4qxqy0mxrpqj58qdclycfs"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack-vendored-inputs 'configure
              (lambda _
                ;; XXX: Our version of protobuf leads to "File already
                ;; exists in database" when loading jax in Python.
                ;; Using the static library is what Nix does, but it
                ;; doesn't help us.
                (let ((bazel-out (string-append (getenv "NIX_BUILD_TOP")
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
                                #$(this-package-input "python-wrapper")
                                "/bin/python\nbuild --python_path="
                                #$(this-package-input "python-wrapper")
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
                               python-protobuf-for-tensorflow
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

(define python-jaxlib/wheel-cuda
  (package
    (inherit python-jaxlib/wheel)
    (name "python-jaxlib-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-jaxlib/wheel)
       ((#:fetch-targets _)
        '(list "//jaxlib/tools:build_wheel"
               "//jaxlib/tools:build_gpu_plugin_wheel" "@mkl_dnn_v1//:mkl_dnn"))
       ((#:vendored-inputs-hash _)
        "0n4p27rsb0hz7prk4lm2z9qlbi5vsrdvmd0a04kiw1kl7qzkkxxs")
       ((#:bazel-configuration conf)
        #~(begin
            #$conf
            ;; When building with CUDA, Bazel uses ldconfig and
            ;; complains that it can't open /etc/ld.so.cache.
            ;; So we fake ldconfig.
            (mkdir-p "/tmp/dummy-ldconfig")
            (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
            (setenv "PATH"
                    (string-append "/tmp/dummy-ldconfig:"
                                   (getenv "PATH")))))
       ((#:bazel-arguments args)
        #~(append #$args
                  (list "--config=cuda"
                        (string-append "--action_env=CUDA_TOOLKIT_PATH="
                                       #$(this-package-input "cuda-toolkit"))
                        (string-append "--action_env=CUDNN_INSTALL_PATH="
                                       #$(this-package-input "cudnn"))
                        (string-append "--action_env=TF_CUDA_PATHS="
                                       #$(this-package-input "cuda-toolkit")
                                       ","
                                       #$(this-package-input "cudnn"))
                        (string-append "--action_env=TF_CUDA_VERSION="
                                       #$(version-major+minor (package-version
                                                               (this-package-input
                                                                "cuda-toolkit"))))
                        (string-append "--action_env=TF_CUDNN_VERSION="
                                       #$(version-major (package-version (this-package-input
                                                                          "cudnn")))))))
       ((#:run-command cmd)
        #~(list (string-append "--output_path="
                               #$output)
                (string-append "--cpu="
                               #$(match (or (%current-target-system)
                                            (%current-system))
                                   ("x86_64-linux" "x86_64")
                                   ("i686-linux" "i686")
                                   ("mips64el-linux" "mips64")
                                   ("aarch64-linux" "aarch64")
                                   ;; Prevent errors when querying this
                                   ;; package on unsupported platforms,
                                   ;; e.g. when running "guix package
                                   ;; --search="
                                   (_ "UNSUPPORTED")))))
       ((#:phases phases)
        (with-imported-modules (source-module-closure '((guix build utils)
                                                        (guix build union)
                                                        (guix build
                                                         gnu-build-system)
                                                        (guix-science build
                                                         bazel-build-system)))
                               #~(modify-phases #$phases
                                   (add-before 'configure 'patch-compiler-wrapper
                                     (lambda _
                                       (let ((bazel-out (string-append (getenv
                                                                        "NIX_BUILD_TOP")
                                                         "/output")))
                                         (patch-shebang (string-append
                                                         bazel-out
                                                         "/external/xla/third_party/tsl/third_party/gpus/crosstool/clang/bin/crosstool_wrapper_driver_is_not_gcc.tpl"))

                                         ;; This wrapper insists on passing
                                         ;; no-canonical-prefixes, which makes it
                                         ;; impossible for GCC to find
                                         ;; architecture-specific headers like
                                         ;; bits/c++config.h.
                                         (substitute* (string-append bazel-out
                                                       "/external/xla/third_party/tsl/third_party/gpus/crosstool/cc_toolchain_config.bzl.tpl")
                                           (("\"-no-canonical-prefixes\",")
                                            "")))))
                                   ;; XXX: this should be a function of the features
                                   ;; supported by the given CUDA library version.  Our
                                   ;; CUDA 11 version does not support the virtual
                                   ;; architecture "compute_90", for example.
                                   (add-after 'configure 'set-cuda-capabilities
                                     (lambda _
                                       (setenv "TF_CUDA_COMPUTE_CAPABILITIES"
                                        "sm_52,sm_60,sm_70,sm_80,compute_90")))
                                   (add-after 'set-cuda-capabilities 'configure-with-cuda
                                     (lambda _
                                       ;; When building with CUDA, Bazel uses ldconfig and
                                       ;; complains that it can't open /etc/ld.so.cache.
                                       ;; So we fake ldconfig.
                                       (mkdir-p "/tmp/dummy-ldconfig")
                                       (symlink (which "true")
                                        "/tmp/dummy-ldconfig/ldconfig")
                                       (setenv "PATH"
                                               (string-append
                                                "/tmp/dummy-ldconfig:"
                                                (getenv "PATH")))

                                       ;; Bazel expects the GCC and CUDA toolchains to be
                                       ;; under the same prefix.
                                       (use-modules (guix build union))
                                       (let ((toolchain (string-append (getenv
                                                                        "NIX_BUILD_TOP")
                                                         "/toolchain")))
                                         (union-build toolchain
                                                      (cons #$(this-package-input
                                                               "cuda-toolkit")
                                                            (match '#$(standard-packages)
                                                              (((labels directories . rest)
                                                                ...)
                                                               directories))))
                                         (setenv "GCC_HOST_COMPILER_PREFIX"
                                                 (string-append toolchain
                                                                "/bin"))
                                         (setenv "GCC_HOST_COMPILER_PATH"
                                                 (string-append toolchain
                                                                "/bin/gcc")))
                                       (call-with-output-file ".jax_configure.bazelrc"
                                         (lambda (port)
                                           ;; Append at the end of this file
                                           (seek port 0 SEEK_END)
                                           (display (string-append
                                                     "build --config=cuda\n"
                                                     "build:cuda --action_env TF_CUDA_COMPUTE_CAPABILITIES="
                                                     (getenv
                                                      "TF_CUDA_COMPUTE_CAPABILITIES")
                                                     "\n"
                                                     "build --action_env CUDA_TOOLKIT_PATH="
                                                     #$(this-package-input
                                                        "cuda-toolkit")
                                                     "\n"
                                                     "build --action_env CUDNN_INSTALL_PATH="
                                                     #$(this-package-input
                                                        "cudnn")
                                                     "\n"
                                                     "build --action_env TF_CUDA_PATHS="
                                                     #$(this-package-input
                                                        "cuda-toolkit")
                                                     ","
                                                     #$(this-package-input
                                                        "cudnn")
                                                     "\n"
                                                     "build --action_env TF_CUDA_VERSION="
                                                     #$(version-major+minor (package-version
                                                                             (this-package-input
                                                                              "cuda-toolkit")))
                                                     "\n"
                                                     "build --action_env TF_CUDNN_VERSION="
                                                     #$(version-major (package-version
                                                                       (this-package-input
                                                                        "cudnn"))))
                                                    port))))))))))
    (inputs (modify-inputs (package-inputs python-jaxlib/wheel)
              (append cuda-toolkit-11.8 cudnn-8.9)))
    ;; For crosstool_wrapper_driver_is_not_gcc
    (native-inputs (modify-inputs (package-native-inputs python-jaxlib/wheel)
                     (append python-wrapper)))))

(define-public python-jaxlib
  (package
    (inherit python-jaxlib/wheel)
    (source
     #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "dist")
              (install-file (car (find-files (assoc-ref inputs "python-jaxlib")
                                             "\\.whl$")) "dist"))))))
    (native-inputs (list python-jaxlib/wheel))))

(define-public python-jaxlib-cuda
  (package
    (inherit python-jaxlib/wheel-cuda)
    (source
     #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "dist")
              (let ((wheel (car (find-files (assoc-ref inputs
                                                       "python-jaxlib-cuda")
                                            "jaxlib-.*\\.whl$"))))
                (install-file wheel "dist"))))
          ;; XXX: python-jaxlib/wheel-with-cuda11 builds libraries
          ;; without RUNPATH.
          (add-after 'install 'fix-rpath
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libdir (string-append #$output "/lib"))
                    (rpath (string-append #$output
                            "/lib/python3.10/site-packages/jaxlib/mlir/_mlir_libs/:"
                            (string-join (map (lambda (label+dir)
                                                (string-append (cdr label+dir)
                                                               "/lib")) inputs)
                                         ":"))))
                (for-each (lambda (file)
                            (invoke "patchelf" "--set-rpath" rpath file)
                            ;; .so files should be executable
                            (chmod file #o555))
                          (find-files libdir ".*\\.so$"))))))))
    ;; XXX: for fix-rpath phase only
    (inputs `(("gcc:lib" ,gcc "lib")
              ,@(package-inputs python-jaxlib/wheel-cuda)))
    (native-inputs (list patchelf python-jaxlib/wheel-cuda))))

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
    (native-inputs (list python-setuptools python-wheel))
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

(define-public python-jax-cuda
  (package
    (inherit python-jax)
    (name "python-jax-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-jax)
                         (replace "python-jaxlib" python-jaxlib-cuda)))))

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
    (native-inputs (list python-cloudpickle python-dm-tree python-pytest
                         python-setuptools python-wheel))
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

(define-public python-chex-cuda
  (package
    (inherit python-chex)
    (name "python-chex-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-chex)
                         (replace "python-jax" python-jax-cuda)
                         (replace "python-jaxlib" python-jaxlib-cuda)))))

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
    (native-inputs (list python-dm-tree python-pytest python-setuptools
                         python-wheel))
    (home-page "https://github.com/google-deepmind/optax/")
    (synopsis "Gradient processing and optimization library for JAX")
    (description "Optax is a gradient processing and optimization
library for JAX.")
    (license license:asl2.0)))

(define-public python-optax-cuda
  (package
    (inherit python-optax)
    (name "python-optax-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-optax)
                         (replace "python-chex" python-chex-cuda)
                         (replace "python-jax" python-jax-cuda)
                         (replace "python-jaxlib" python-jaxlib-cuda)))))

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
      #:phases
      #~(modify-phases %standard-phases
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
    (native-inputs (list python-flit-core python-pytest python-pytest-xdist
                         python-setuptools python-wheel))
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

(define-public python-flax-cuda
  (package
    (inherit python-flax)
    (name "python-optax-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-flax)
                         (replace "python-jax" python-jax-cuda)
                         (replace "python-optax" python-optax-cuda)))
    (native-inputs (modify-inputs (package-native-inputs python-flax)
                     (replace "python-pytorch" python-pytorch-cuda)))))

(define-public gloo-cuda
  (let ((version "0.0.0")
        (commit "c7b7b022c124d9643957d9bd55f57ac59fce8fa2")
        (revision "20250117"))
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
          (base32 "0xsp2m2if3g85l0c3cx9l0j3kz36j3kbmz9mai6kchdhrs13r7d5"))
         (patches (search-patches
                   "myguix/patches/gloo-cuda-cpp-standard.patch"))))
      (build-system cmake-build-system)
      (native-inputs (list googletest))
      (inputs (modify-inputs (package-inputs gloo)
                (append cuda-toolkit nccl)))
      (arguments
       (substitute-keyword-arguments (package-arguments gloo)
         ((#:configure-flags flags
           ''())
          #~(append (list "-DUSE_CUDA=ON" "-DCUDA_ARCH_NAME=All")
                    #$flags))))
      (synopsis "Collective communications library")
      (description
       "Gloo is a collective communications library.  It comes with a
number of collective algorithms useful for machine learning applications.
These include a barrier, broadcast, and allreduce.

Note: This package provides NVIDIA GPU support.")
      (home-page "https://github.com/facebookincubator/gloo")
      (license license:bsd-3))))

(define-public tensorpipe-cuda
  (let ((commit "af0118d13e52f5a08841464a768e01a0bf3e3075")
        (revision "20250815"))
    (package
      (name "tensorpipe-cuda")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pytorch/tensorpipe")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0p3zvyplvrfdci0qaxxldi9qpgs956w7y55gv0i6vi6ay1hiyrjz"))
         (modules '((guix build utils)))
         (snippet '(delete-file-recursively "third_party"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags ''("-DBUILD_SHARED_LIBS=ON" "-DTP_USE_CUDA=ON")
        ;; There are no tests
        #:tests? #f))
      (inputs (list cuda-toolkit libuv))
      (native-inputs (list googletest pkg-config pybind11 libnop))
      (home-page "https://github.com/pytorch/tensorpipe")
      (synopsis "Tensor-aware point-to-point communication primitive for
machine learning")
      (description
       "TensorPipe provides a tensor-aware channel to transfer
rich objects from one process to another while using the fastest transport for
the tensors contained therein.")
      (license license:bsd-3))))

(define-public dlpack
  (package
    (name "dlpack")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/dlpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vlp8gcf7s3snalj6xmvgqxxn96fki6gw9hzph30gmgdbaz730j6"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;no tests.
    (home-page "https://github.com/dmlc/dlpack")
    (synopsis "In Memory Tensor Structure")
    (description
     "This package provides an open in-memory tensor structure for
sharing tensors among frameworks.  DLPack enables
@itemize
@item Easier sharing of operators between deep learning frameworks.
@item Easier wrapping of vendor level operator implementations, allowing
collaboration when introducing new devices/ops.
@item Quick swapping of backend implementations, like different version of
BLAS
@item For final users,this could bring more operators, and possibility of
mixing usage between frameworks.
@end itemize

This package does not intend to implement Tensor and Ops, but instead use this
as common bridge to reuse tensor and ops across frameworks.")
    (license license:asl2.0)))

(define %python-pytorch-version
  "2.9.0")

(define %python-pytorch-cuda-src
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/pytorch/pytorch")
                        (commit (string-append "v" %python-pytorch-version))
                        (recursive? #t)))
    (file-name (git-file-name "python-pytorch" %python-pytorch-version))
    (sha256 (base32 "123wvixy0bbpaim2w89w8s6m8rci90si5m26ds9fidf5mrxy3k16"))
    (patches (map (lambda (patch)
                    (search-path (map (cut string-append <> "/myguix/patches")
                                      %load-path) patch))
                  '("python-pytorch-system-libraries.patch"
                    "python-pytorch-runpath.patch"
                    "python-pytorch-without-kineto.patch"
                    "python-pytorch-fix-codegen.patch")))
    (modules '((guix build utils)))
    (snippet '(begin
                ;; Bundled or unused code
                (for-each (lambda (dir)
                            (when (file-exists? dir)
                              (delete-file-recursively dir)))
                          '("android"
                            "aten/src/ATen/native/quantized/cpu/qnnpack"
                            "caffe2/mobile/contrib/libopencl-stub"
                            "caffe2/mobile/contrib/libvulkan-stub"))

                ;; Autogenerated files
                (for-each delete-file
                          '("aten/src/ATen/nnapi/nnapi_wrapper.cpp"
                            "aten/src/ATen/nnapi/nnapi_wrapper.h"
                            ;; These files contain just lists of floating point values and
                            ;; might be as well hand-written.
                            ;; "test/cpp/api/init_baseline.h"
                            ;; "test/cpp/api/optim_baseline.h"
                            "test/mobile/test_upgrader_bytecode_table_example.cpp"
                            "torch/csrc/jit/mobile/upgrader_mobile.cpp"
                            "torch/csrc/jit/runtime/decomposition_registry_util.cpp"
                            "torch/csrc/jit/runtime/serialized_shape_function_registry.cpp"
                            "torch/csrc/jit/tensorexpr/external_functions_codegen.cpp"
                            "torch/csrc/jit/serialization/mobile_bytecode_generated.h"))
                (delete-file-recursively ".github")))))

;; Just re-export $LIBRARY_PATH as $LD_LIBRARY_PATH and torch.cuda.is_available() will return True.
(define-public python-pytorch-cuda
  (package
    (inherit python-pytorch)
    (source
     %python-pytorch-cuda-src)
    (name "python-pytorch-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'cmake-patches 'cuda-cmake-patches
              (lambda _
                ;; XXX: Currently nvidia-cudnn-frontend doesn't install CMake
                ;; configuration files, we must add unbundled nlohmann-json.
                ;; Additionally, it won't work without CUDNN_INCLUDE_DIR.
                (substitute* "cmake/Dependencies.cmake"
                  (("set\\(CUDNN_FRONTEND_INCLUDE_DIR.*$")
                   (format #f
                    "set(CUDNN_FRONTEND_INCLUDE_DIR ~a/include)
  target_include_directories(torch::cudnn INTERFACE
      ${CUDNN_INCLUDE_DIR} ${~a/include}
  )~%"
                    #$(this-package-input "cudnn-frontend")
                    #$(this-package-input "nlohmann-json"))))
                (substitute* "aten/src/ATen/CMakeLists.txt"
                  ;; XXX: Link the right include dir for cutlass-headers.
                  (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/\\.\\./\\.\\./\\.\\./third_party/cutlass/include")
                   (string-append #$(this-package-input "cutlass-headers")
                                  "/include"))
                  ;; XXX: Link the right include dir for cutlass-tools.
                  (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/\\.\\./\\.\\./\\.\\./third_party/cutlass/tools/util/include")
                   (string-append #$(this-package-input "cutlass-tools")
                                  "/tools/util/include")))
                ;; Make _get_ld_library_path look for LIBRARY_PATH, which is the
                ;; environment variable used by Guix to manage library paths.
                (substitute* "torch/_inductor/compile_worker/subproc_pool.py"
                  (("os\\.environ\\.get\\(\"LD_LIBRARY_PATH\"")
                   "os.environ.get(\"LIBRARY_PATH\""))
                (substitute* "torch/utils/cpp_extension.py"
                  (("LD_LIBRARY_PATH")
                   "LIBRARY_PATH"))
                ;; XXX: Not linking gtest+gtest_main breaks compilation
                (substitute* '("c10/cuda/test/CMakeLists.txt"
                               "caffe2/CMakeLists.txt")
                  (("target_link_libraries\\((.* gtest_main)\\)" all content)
                   (format #f "target_link_libraries(~a gtest)" content)))))
            (add-after 'cuda-cmake-patches 'fix-fmt-compatibility
              (lambda _
                ;; XXX: PyTorch 2.8.0 is incompatible with fmt 9.1.0 due to
                ;; stricter constexpr requirements in FMT_COMPILE macro.
                ;; Remove FMT_COMPILE usage where it causes issues.
                (substitute* "torch/csrc/distributed/c10d/TraceUtils.h"
                  (("fmt::format\\(FMT_COMPILE\\(\"([^\"]+)\"\\)" _ fmt-string)
                   (string-append "fmt::format(\"" fmt-string "\""))
                  (("constexpr auto TB_FMT_CSTR = FMT_COMPILE\\(\"([^\"]+)\"\\)"
                    _ fmt-string)
                   (string-append "const char* TB_FMT_CSTR = \"" fmt-string
                                  "\"")))))
            (replace 'set-max-jobs
              (lambda _
                (setenv "MAX_JOBS"
                        (number->string (parallel-job-count)))))
            (add-after 'use-system-libraries 'use-cuda-libraries
              (lambda _
                (setenv "BUILD_TEST" "0")
                (setenv "PYTORCH_BUILD_VERSION" "2.7.0")
                (setenv "PYTORCH_BUILD_NUMBER" "1")
                (setenv "TORCH_NVCC_FLAGS" "-Xfatbin -compress-all")
                (setenv "USE_CUDA" "1")
                (setenv "USE_NCCL" "1")
                (setenv "USE_CUDNN" "1")
                (setenv "USE_CUFILE" "1")
                (setenv "USE_XPU" "0")
                (setenv "USE_CUSPARSELT" "1")
                (setenv "USE_CUPTI_SO" "1")
                (setenv "USE_TENSORPIPE" "1")
                (setenv "USE_GLOO" "0")
                (setenv "USE_SYSTEM_NCCL" "1")
                (setenv "USE_OPENMP" "1")
                ;; FlashAttention currently fails to compile with CUDA 13, so
                ;; keep it disabled until upstream gains support.
                (setenv "USE_FLASH_ATTENTION" "0")
                (setenv "USE_MEM_EFF_ATTENTION" "1")
                (setenv "TORCH_CUDA_ARCH_LIST" "8.6")
                (setenv "USE_ROCM" "0")
                (setenv "USE_NUMA" "0")
                (setenv "CUDA_HOME"
                        #$(this-package-input "cuda-toolkit"))
                (setenv "CUDA_NVCC_EXECUTABLE"
                        #$(file-append (this-package-input "cuda-toolkit")
                                       "/bin/nvcc"))
                (setenv "CUDNN_LIB_DIR"
                        #$(file-append (this-package-input "cudnn") "/lib"))
                (setenv "CUDNN_INCLUDE_DIR"
                        #$(file-append (this-package-input "cudnn") "/include"))
                (setenv "CUDSS_LIB_DIR"
                        #$(file-append (this-package-input "cudss") "/lib"))
                (setenv "CUDSS_INCLUDE_DIR"
                        #$(file-append (this-package-input "cudss") "/include"))
                (setenv "CUSPARSELT_LIBRARY_PATH"
                        #$(file-append (this-package-input "cusparselt")
                                       "/lib"))
                (setenv "CUSPARSELT_INCLUDE_PATH"
                        #$(file-append (this-package-input "cusparselt")
                                       "/include"))
                (setenv "NCCL_ROOT"
                        #$(file-append (this-package-input "nccl")))
                (setenv "NCCL_LIB_DIR"
                        #$(file-append (this-package-input "nccl") "/lib"))
                (setenv "NCCL_INCLUDE_DIR"
                        #$(file-append (this-package-input "nccl") "/include"))))
            (add-after 'install 'add-rpath
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (cuda (assoc-ref inputs "cuda-toolkit"))
                       (cudnn (assoc-ref inputs "cudnn"))
                       (nccl (assoc-ref inputs "nccl"))
                       (rpath (string-append "$ORIGIN:$ORIGIN/..:"
                                             cuda
                                             "/lib64:"
                                             cudnn
                                             "/lib:"
                                             nccl
                                             "/lib")))
                  (format #t "Embedding RPATH ~a into Torch shared objects~%"
                          rpath)
                  (for-each (lambda (file)
                              (invoke "patchelf" "--set-rpath" rpath file))
                            (find-files (string-append out "/lib/python")
                                        "\\.so$")))))))))

    (inputs (modify-inputs (package-inputs python-pytorch)
              (replace "tensorpipe" tensorpipe-cuda)
              (replace "gloo" gloo-cuda)
              (append cuda-toolkit
                      cudnn
                      cudss
                      cusparselt
                      cutlass-headers
                      cutlass-tools
                      cudnn-frontend
                      nlohmann-json
                      nccl
                      nvtx)))
    (native-inputs (package-native-inputs python-pytorch))
    (propagated-inputs (package-propagated-inputs python-pytorch))
    (home-page "https://pytorch.org/")
    (synopsis "Python library for tensor computation and deep neural networks")
    (description
     "PyTorch is a Python package that provides two high-level features:

@itemize
@item tensor computation (like NumPy) with strong GPU acceleration;
@item deep neural networks (DNNs) built on a tape-based autograd system.
@end itemize

You can reuse Python packages such as NumPy, SciPy, and Cython to extend
PyTorch when needed.

Note: This package provides NVIDIA GPU support.")
    (license license:bsd-3)))

(define-public python-torchvision-cuda
  (package
    (name "python-torchvision-cuda")
    (version "0.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pytorch/vision")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d09xwblldgzmzfdlrsyx6mgv939z4yi1hqanm9yx63cs2mr7w85"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (delete-file-recursively "android")
                   (delete-file-recursively "ios")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite is expensive and there is no easy way to subset it.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'setenv
            (lambda _
              (let ((jpegdir #$(this-package-input "libjpeg-turbo")))
                (setenv "TORCHVISION_INCLUDE"
                        (string-append jpegdir "/include/"))
                (setenv "TORCHVISION_LIBRARY"
                        (string-append jpegdir "/lib/"))))))))
    (inputs (list ffmpeg-nvidia libpng libjpeg-turbo))
    (propagated-inputs (list python-numpy
                             python-typing-extensions
                             python-requests
                             python-pillow
                             python-pillow-simd
                             python-pytorch-cuda))
    (native-inputs (list pybind11 which python-pytest python-setuptools
                         python-wheel))
    (home-page "https://pytorch.org/vision/stable/index.html")
    (synopsis "Datasets, transforms and models specific to computer vision")
    (description
     "The torchvision package consists of popular datasets, model architectures,
and common image transformations for computer vision.")
    (license license:bsd-3)))

(define-public python-torchdata-cuda
  (package
    (name "python-torchdata-cuda")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pytorch/data")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02dyakcvbwsdr504hb1p284qk93cnm7rfbbyp6f2c38npqn1jaad"))
       (modules '((guix build utils)))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-requests python-urllib3
                             python-pytorch-cuda))
    (native-inputs (list pybind11 which python-pytest python-setuptools
                         python-wheel))
    (home-page "https://pytorch.org/vision/stable/index.html")
    (synopsis "Datasets, transforms and models specific to computer vision")
    (description
     "The torchvision package consists of popular datasets, model architectures,
and common image transformations for computer vision.")
    (license license:bsd-3)))

(define-public python-lion-pytorch
  (package
    (name "python-lion-pytorch")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lion_pytorch" version))
       (sha256
        (base32 "127xr3jpwpmh6zqhm421wirgswanawpf49y7cxndvsapx1y13fj2"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Disable tests
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/lucidrains/lion-pytorch")
    (synopsis "Lion Optimizer - Pytorch")
    (description "Lion Optimizer - Pytorch.")
    (license license:expat)))

(define-public python-lion-pytorch-cuda
  (package
    (inherit python-lion-pytorch)
    (name "python-lion-pytorch-cuda")
    (propagated-inputs (list python-pytorch-cuda cuda-toolkit))))

(define-public python-torch-diffeq
  (package
    (name "python-torch-diffeq")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rtqichen/torchdiffeq")
             (commit "a88aac53cae738addee44251288ce5be9a018af3")))
       (sha256
        (base32 "0c2zqbdxqvd5abfpk0im6rcy1ij39xvrmixc6l9znb6bhcxk2jra"))))
    (build-system python-build-system)
    (arguments
     ;; Looks like the tests require network connection.
     '(#:tests? #f))
    (inputs (list python-pytorch python-pillow python-scipy))
    (home-page "https://github.com/rtqichen/torchdiffeq")
    (synopsis
     "Differentiable ODE solvers with full GPU support and O(1)-memory
backpropagation.")
    (description
     "This library provides ordinary differential equation (ODE) solvers
implemented in PyTorch. Backpropagation through ODE solutions is supported using
the adjoint method for constant memory cost. For usage of ODE solvers in deep
learning applications.

As the solvers are implemented in PyTorch, algorithms in this repository are
fully supported to run on the GPU.")
    (license license:expat)))

(define-public python-torch-diffeq-cuda
  (package
    (inherit python-torch-diffeq)
    (name "python-torch-diffeq-cuda")
    (propagated-inputs (list python-pytorch-cuda cuda-toolkit))))

(define-public python-torch-fidelity
  (package
    (name "python-torch-fidelity")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/toshas/torch-fidelity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cw126rlc4samgg5d1f13znklaykkv5x83xlys6cz3cdlfn0g44v"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ;No test command in setup.py
    (propagated-inputs (list python-numpy
                             python-pillow
                             python-scipy
                             python-pytorch
                             python-torchvision
                             python-tqdm))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.github.com/toshas/torch-fidelity")
    (synopsis
     "High-fidelity performance metrics for generative models in PyTorch")
    (description
     "High-fidelity performance metrics for generative models in @code{PyTorch}.")
    (license license:asl2.0)))

(define-public python-torch-fidelity-cuda
  (package
    (inherit python-torch-fidelity)
    (name "python-torch-fidelity-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torch-fidelity)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchvision" python-torchvision-cuda)))))

(define-public python-entmax
  (package
    (name "python-entmax")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deep-spin/entmax")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10dijylrpkfl66dkihqlhwgw9x05qrz3di7ybzdf1a4xqly875v8"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pytorch))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/deep-spin/entmax")
    (synopsis "Sparse probability mappings and losses (α-entmax) for PyTorch")
    (description
     "Entmax generalises @code{softmax} by producing sparse probability
distributions, which can make attention weights easier to interpret and
sometimes improve performance.  This library provides differentiable entmax
and sparsemax activations together with the corresponding loss functions,
implemented for PyTorch.")
    (license license:expat)))

(define-public python-entmax-cuda
  (package
    (inherit python-entmax)
    (name "python-entmax-cuda")
    (propagated-inputs (list python-pytorch-cuda cuda-toolkit))))

(define-public python-storchastic
  (package
    (name "python-storchastic")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "storchastic" version))
       (sha256
        (base32 "1c0hv37frpc7gh9f05c0fjgzsf83bxzs2d4nzbhgzlrj27dv8vi3"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-entmax python-packaging python-pyro-ppl
                             python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/HEmile/storchastic")
    (synopsis "Stochastic Deep Learning for PyTorch")
    (description "Stochastic Deep Learning for @code{PyTorch}.")
    (license license:gpl3+)))

(define-public python-storchastic-cuda
  (package
    (inherit python-storchastic)
    (name "python-storchastic-cuda")
    (propagated-inputs (list python-pytorch-cuda cuda-toolkit))))

(define-public python-triton
  (package
    (name "python-triton")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/triton-lang/triton")
             ;; v3.4.0 release tag
             (commit "c817b9b63d40ead1ed023b7663f5ea14f676f4bc")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fcs959s4fwnh037mqfi890fll3i352az0kwqzkv8mwlxs8kvjzg"))
       (patches (search-patches
                 "myguix/patches/triton-link-llvm-dynamically.patch"
                 "myguix/patches/triton-fix-ldconfig-path.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DLLVM_DIR="
                             (assoc-ref %build-inputs "llvm-for-triton")
                             "/lib/cmake/llvm") "-DBUILD_TESTING=OFF"
              (string-append "-DCMAKE_INSTALL_RPATH="
                             (assoc-ref %build-inputs "llvm-for-triton")
                             "/lib")
              "-DCMAKE_SHARED_LINKER_FLAGS=-Wl,--exclude-libs,ALL"
              "-DLLVM_LINK_LLVM_DYLIB=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              ;; 1. stub out googletest + drop test/ unittest/ bin/
              (with-output-to-file "unittest/googletest.cmake"
                (lambda ()
                  (display "return()  # stubbed for Guix\n")))
              (substitute* "CMakeLists.txt"
                (("include\\(unittest/googletest\\.cmake\\)")
                 "#")
                (("add_subdirectory\\((test|unittest|bin)\\)")
                 "#"))
              ;; 3. tiny shim for MLIRGPUOps
              (with-output-to-file "cmake/MLIRGPUCompat.cmake"
                (lambda ()
                  (display
                   "if(NOT TARGET MLIRGPUOps AND TARGET MLIRGPUDialect)
  add_library(MLIRGPUOps INTERFACE IMPORTED)
  set_target_properties(MLIRGPUOps PROPERTIES
                        INTERFACE_LINK_LIBRARIES MLIRGPUDialect)
endif()
")))
              (substitute* "CMakeLists.txt"
                (("(find_package\\(MLIR[^\n]*\\)\n)" all)
                 (string-append all
                  "include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/MLIRGPUCompat.cmake)
")))
              ;; 4. block pre-built LLVM download
              (for-each (lambda (file)
                          (substitute* file
                            (("download_prebuilt\\(")
                             "raise RuntimeError(\"offline\")  # ")))
                        (find-files "python" "check\\.py$"))
              (setenv "TRITON_DISABLE_LLVM_DOWNLOAD" "1")
              ;; 5. drop cmake / ninja wheels from pyproject
              (substitute* "pyproject.toml"
                ((" *\"cmake[^\"]+\",?")
                 "")
                ((" *\"ninja[^\"]+\",?")
                 ""))
              ;; 6. Remove AMD from CMakeLists to avoid building it
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(third_party/amd\\)")
                 "# add_subdirectory(third_party/amd)  # Disabled"))
              ;; 7. Disable Proton since it requires AMD roctracer
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(third_party/proton\\)")
                 "# add_subdirectory(third_party/proton)  # Disabled - requires AMD roctracer"))
              ;; 9. make setup.py entirely offline-safe
              (setenv "TRITON_OFFLINE_BUILD" "1")
              ;; 10. Disable Proton build
              (setenv "TRITON_BUILD_PROTON" "OFF")
              ;; 11. Limit parallel build jobs
              (setenv "MAX_JOBS" "4")
              ;; 12. use local CUDA / tool-chain
              (let* ((llvm (assoc-ref inputs "llvm-for-triton"))
                     (cuda (assoc-ref inputs "cuda-toolkit")))
                (setenv "TRITON_OFFLINE_BUILD" "1")
                (setenv "TRITON_LLVM_INSTALL_DIR" llvm)
                (setenv "LLVM_SYSPATH" llvm)
                (setenv "CUDAToolkit_ROOT" cuda)
                (setenv "CUDA_HOME" cuda)
                (setenv "CUDA_TOOLKIT_ROOT_DIR" cuda)
                (setenv "PATH"
                        (string-append cuda "/bin:"
                                       (getenv "PATH"))))
              ;; Add this new part to set LDFLAGS for proper RPATH
              (let* ((llvm (assoc-ref inputs "llvm-for-triton")))
                (setenv "LDFLAGS"
                        (string-append "-Wl,-rpath=" llvm "/lib "
                                       (or (getenv "LDFLAGS") ""))))))
          ;; ─────────────────────────────────────────────────────────
          ;; build & install the wheel after the C++ libs
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion "../source"
                (let* ((out (assoc-ref outputs "out"))
                       (llvm (assoc-ref inputs "llvm-for-triton"))
                       (rapidjson (assoc-ref inputs "rapidjson"))
                       (tmp-home (string-append (getenv "TMPDIR")
                                                "/triton-home")))
                  ;; Create a writable HOME directory for the build
                  (mkdir-p tmp-home)
                  (setenv "HOME" tmp-home)
                  (setenv "LLVM_SYSPATH" llvm)
                  ;; For offline builds, setup.py needs JSON_SYSPATH
                  (setenv "JSON_SYSPATH" rapidjson)
                  ;; Disable Proton
                  (setenv "TRITON_BUILD_PROTON" "OFF")
                  ;; Limit parallel jobs to prevent memory exhaustion
                  (setenv "MAX_JOBS" "4")
                  (let ((common-env (list (string-append "LLVM_SYSPATH=" llvm)
                                          (string-append "JSON_SYSPATH="
                                                         rapidjson)
                                          (string-append "HOME=" tmp-home)
                                          "TRITON_BUILD_PROTON=OFF"
                                          "MAX_JOBS=4"
                                          ;; Add LDFLAGS to the environment
                                          (string-append "LDFLAGS="
                                                         (getenv "LDFLAGS")))))
                    (apply invoke "env"
                           (append common-env
                                   (list "python"
                                         "-m"
                                         "build"
                                         "--wheel"
                                         "--no-isolation"
                                         ".")))
                    (apply invoke "env"
                           (append common-env
                                   (list "python" "-m" "installer"
                                         (string-append "--prefix=" out)
                                         (car (find-files "dist" "\\.whl$"))))))))))
          (delete 'check))))
    (inputs (list llvm-for-triton
                  libffi
                  rapidjson
                  python
                  python-numpy
                  python-packaging
                  python-filelock
                  python-psutil
                  python-sympy
                  linux-libre-headers))
    (native-inputs (list cmake
                         ninja
                         pkg-config
                         python-pypa-build
                         python-installer
                         python-wrapper
                         pybind11))
    (propagated-inputs (list cuda-toolkit))
    (native-search-paths
     (list (search-path-specification
            (variable "CUDA_HOME")
            (files '(".")))
           (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include")))))
    (home-page "https://github.com/triton-lang/triton")
    (synopsis "JIT-compile high-performance custom GPU kernels from Python")
    (description
     "Triton is an open-source language and compiler that lets you write
high-performance GPU code directly in Python.  This Guix package ships version
3.4.0, built against LLVM 20 and the NVIDIA CUDA toolkit, with all online
downloads disabled for reproducible, network-free builds.")
    (license license:asl2.0)
    (supported-systems (list "x86_64-linux"))))

(define-public python-eval-type-backport
  (package
    (name "python-eval-type-backport")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eval_type_backport" version))
       (sha256
        (base32 "1ha9v3kz12kr5ip4i1w3cw3mxbs6339i88yhb39mpfqyy166nmzh"))))
    (build-system pyproject-build-system)
    (native-inputs (list go
                         rust
                         python-pytest
                         python-setuptools
                         python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/alexmojaki/eval_type_backport")
    (synopsis
     "Like `typing._eval_type`, but lets older Python versions use newer typing features.")
    (description
     "Like `typing._eval_type`, but lets older Python versions use newer typing
features.")
    (license license:expat)))

(define-public python-wandb
  (package
    (name "python-wandb")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wandb" version))
       (sha256
        (base32 "1a9alrlcflbvy6gm9m63q0886whxl7pi78kb69hz16hy6575f7qz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;upstream tests hit the network
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'skip-gpu-stats
            (lambda _
              ;; For 0.18.x the flag is WANDB_BUILD_SKIP_NVIDIA
              (setenv "WANDB_BUILD_SKIP_NVIDIA" "true")))
          (add-before 'build 'prepare-go
            (lambda _
              (setenv "HOME"
                      (getcwd))
              (setenv "GOTOOLCHAIN" "local")
              (setenv "GOFLAGS" "-buildvcs=false"))))))
    ;; build-time tools only
    (native-inputs (list go-1.23 python-hatchling python-typing-extensions))
    ;; runtime Python deps
    (propagated-inputs (list python-click
                             python-docker-pycreds
                             python-eval-type-backport
                             python-gitpython
                             python-platformdirs
                             python-protobuf
                             python-psutil
                             python-pydantic
                             python-pyyaml
                             python-requests
                             python-sentry-sdk
                             python-setproctitle
                             python-setuptools
                             python-typing-extensions))
    (home-page "https://github.com/wandb/wandb")
    (synopsis
     "CLI and Python library for interacting with the Weights & Biases API")
    (description
     "Weights & Biases lets you track machine-learning experiments, version
datasets and models, and collaborate with your team.")
    (license license:expat)))

(define-public python-torchmetrics-cuda
  (package
    (inherit python-torchmetrics)
    (name "python-torchmetrics-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchmetrics)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-pytorch-lightning-cuda
  (let ((commit "1617f70428a791b2d81c392d6a0b8a078d8e7fb1")
        (revision "0"))
    (package
      (inherit python-pytorch-lightning)
      (name "python-pytorch-lightning-cuda")
      (version (git-version "2.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Lightning-AI/pytorch-lightning")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1i11n4094a8ysb7cj1lww23nh0mk3d9licw9c9pgzws2m0qy70yd"))))
      (build-system pyproject-build-system)
      (propagated-inputs (modify-inputs (package-propagated-inputs
                                         python-pytorch-lightning)
                           (replace "python-pytorch" python-pytorch-cuda)
                           (replace "python-torchvision"
                                    python-torchvision-cuda)
                           (replace "python-torchmetrics"
                                    python-torchmetrics-cuda))))))

(define-public python-xformers
  (package
    (name "python-xformers")
    (version "0.0.31.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xformers" version))
       (sha256
        (base32 "018m62gccrry9kggbyw5vz64n1hqicjp6fjlprpxbb8al0qj1886"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require CUDA device
    (propagated-inputs (list python-numpy python-pytorch))
    (native-inputs (list python-setuptools python-pytorch python-wheel))
    (home-page "https://facebookresearch.github.io/xformers/")
    (synopsis
     "XFormers: A collection of composable Transformer building blocks.")
    (description
     "XFormers: A collection of composable Transformer building blocks.")
    (license license:bsd-3)))

(define-public python-xformers-cuda
  (package
    (inherit python-xformers)
    (name "python-xformers-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-xformers)
                         (replace "python-pytorch" python-pytorch-cuda)))
    (native-inputs (modify-inputs (package-native-inputs python-xformers)
                     (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-kaldi-io
  (package
    (name "python-kaldi-io")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kaldi_io" version))
       (sha256
        (base32 "1kanbffd6clxf6dwx04lv8gkppps9sdssg9f8csvj8zxlwpjq0q5"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests included
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/vesis84/kaldi-io-for-python")
    (synopsis "Glue code connecting Kaldi data and Python")
    (description
     "This package provides I/O functions for reading and writing Kaldi ark/scp
files for use in Python.  It supports reading and writing various Kaldi data
types including matrices, vectors, and posterior probabilities.")
    (license license:asl2.0)))

(define-public python-torchaudio-cuda
  (package
    (inherit python-torchaudio)
    (name "python-torchaudio-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-torchaudio)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'configure-cuda
              (lambda _
                ;; Enable CUDA-specific features
                (setenv "USE_CUDA" "1")
                (setenv "USE_CUDNN" "1")
                (setenv "USE_CUSPARSELT" "1")
                (setenv "USE_CUDSS" "1")
                (setenv "USE_CUFILE" "1")
                ;; Set CUDA paths
                (setenv "CUDA_HOME"
                        #$(this-package-input "cuda-toolkit"))
                (setenv "CUDNN_LIB_DIR"
                        #$(file-append (this-package-input "cudnn") "/lib"))
                (setenv "CUDNN_INCLUDE_DIR"
                        #$(file-append (this-package-input "cudnn") "/include"))
                (setenv "CUDSS_LIB_DIR"
                        #$(file-append (this-package-input "cudss") "/lib"))
                (setenv "CUDSS_INCLUDE_DIR"
                        #$(file-append (this-package-input "cudss") "/include"))
                (setenv "CUSPARSELT_LIBRARY_PATH"
                        #$(file-append (this-package-input "cusparselt")
                                       "/lib"))
                (setenv "CUSPARSELT_INCLUDE_PATH"
                        #$(file-append (this-package-input "cusparselt")
                                       "/include"))
                ;; Set architecture list for RTX 3060
                (setenv "TORCH_CUDA_ARCH_LIST" "8.6")))))))
    (inputs (modify-inputs (package-inputs python-torchaudio)
              (append cuda-toolkit cudnn cudss cusparselt nccl)))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchaudio)
                         (replace "python-pytorch" python-pytorch-cuda)))
    (synopsis "Audio library for PyTorch with CUDA support")
    (description
     "TorchAudio is a library for audio and signal processing with PyTorch.
This variant is built with CUDA support for GPU acceleration, including
CUDNN, cuSPARSELt, cuDSS, and cuFile support.")))

(define-public python-ema-pytorch
  (package
    (name "python-ema-pytorch")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ema_pytorch" version))
       (sha256
        (base32 "1mwsqrsx6j83ycavvh6216mzq5d4mlanyg95bbwh0g7074s0xkpc"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;No tests included
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/lucidrains/ema-pytorch")
    (synopsis
     "Easy way to keep track of exponential moving average version of your pytorch module")
    (description
     "Easy way to keep track of exponential moving average version of your pytorch
module.")
    (license license:expat)))

(define-public python-ema-pytorch-cuda
  (package
    (inherit python-ema-pytorch)
    (name "python-ema-pytorch-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-ema-pytorch)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-torchcodec
  (package
    (name "python-torchcodec")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meta-pytorch/torchcodec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "191pn3ajbkkwvd6pzyykhbpyw5ngz1g4nnbn5j8djj2gqki15dmn"))))
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
          ;; Ensure setup.py allows wheel builds without fetching FFmpeg
          ;; binaries and avoid treating warnings as errors.
          (add-before 'build 'configure-environment
            (lambda _
              (setenv "I_CONFIRM_THIS_IS_NOT_A_LICENSE_VIOLATION" "1")
              (setenv "TORCHCODEC_DISABLE_COMPILE_WARNING_AS_ERROR" "ON")
              (setenv "CMAKE_BUILD_TYPE" "Release")
              ;; Help Torch's CMake find oneDNN (DNNL) provided by Guix.
              (let* ((prefix (getenv "CMAKE_PREFIX_PATH"))
                     (dnnl (string-append #$(this-package-input "onednn")))
                     (httplib (string-append #$(this-package-input
                                                "cpp-httplib")))
                     (openssl (string-append #$(this-package-input "openssl")))
                     (brotli (string-append #$(this-package-input "brotli")))
                     (numactl (string-append #$(this-package-input "numactl")))
                     (zlib (string-append #$(this-package-input "zlib")))
                     (merged (string-join (filter (lambda (s)
                                                    (and s
                                                         (not (string-null? s))))
                                                  (list dnnl
                                                        httplib
                                                        openssl
                                                        brotli
                                                        numactl
                                                        zlib
                                                        prefix)) ":")))
                (setenv "CMAKE_PREFIX_PATH" merged)
                (setenv "DNNL_DIR"
                        (string-append dnnl "/lib/cmake/dnnl"))
                ;; Some packages also honor *_ROOT
                (setenv "HTTPLIB_ROOT" httplib)
                (setenv "OPENSSL_ROOT_DIR" openssl)
                (setenv "BROTLI_ROOT" brotli)
                (setenv "NUMACTL_ROOT" numactl)
                (setenv "ZLIB_ROOT" zlib))))
          ;; Patch RPATH of built shared objects to include $ORIGIN and torch lib dir,
          ;; so the loader finds libtorchcodec_coreN.so and libtorch at runtime.
          (add-before 'validate-runpath 'fix-rpath
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (so-files (find-files out
                                           (lambda (file stat)
                                             (and (eq? 'regular
                                                       (stat:type stat))
                                                  (string-match
                                                   ".*/site-packages/torchcodec/libtorchcodec.*\\.so$"
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
                  (error
                   "torchcodec shared objects not found for RPATH patching"
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
                                   (base-rpath (string-append "$ORIGIN:"
                                                              torch-lib))
                                   (new-rpath (if (string=? existing "")
                                                  base-rpath
                                                  (string-append base-rpath
                                                                 ":" existing))))
                              (invoke "patchelf" "--set-rpath" new-rpath file)))
                          so-files)))))))
    (inputs (list ffmpeg
                  onednn
                  cpp-httplib
                  openssl
                  brotli
                  numactl
                  zlib
                  python-pytorch))
    (propagated-inputs (list python-pytorch))
    (native-inputs (list cmake
                         pkg-config
                         pybind11
                         python-setuptools
                         python-wheel
                         patchelf))
    (home-page "https://github.com/meta-pytorch/torchcodec")
    (synopsis "A video decoder for PyTorch")
    (description
     "TorchCodec provides high-performance video and audio decoding and encoding
primitives for PyTorch. It builds C++ extensions that link against FFmpeg and
optionally CUDA, and exposes Python APIs for efficient media processing.")
    (license license:bsd-3)))

(define-public python-pesq
  (package
    (name "python-pesq")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pesq" version))
       (sha256
        (base32 "02w9zjkpmc9k0939lqqhabsf4mq97j68xmibk0i8aqzvff7v495p"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require audio files
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-cython python-setuptools python-wheel))
    (home-page "https://github.com/ludlows/python-pesq")
    (synopsis "Python wrapper for PESQ Score (narrow band and wide band)")
    (description
     "This package provides a Python wrapper for the Perceptual Evaluation of
Speech Quality (PESQ) score calculation.  It supports both narrow band and
wide band modes for speech quality assessment according to ITU-T P.862
recommendations.")
    (license license:expat)))

(define-public python-torchcodec-cuda
  (package
    (inherit python-torchcodec)
    (name "python-torchcodec-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-torchcodec)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure-environment 'configure-cuda
              (lambda _
                (setenv "ENABLE_CUDA" "1")
                ;; Set minimal arch list; adjust as needed for target GPU(s).
                (setenv "TORCH_CUDA_ARCH_LIST" "8.6")
                (let* ((cuda #$(this-package-input "cuda-toolkit"))
                       (lib64 (string-append cuda "/lib64"))
                       (prefix (or (getenv "CMAKE_PREFIX_PATH") ""))
                       (merged (if (string-null? prefix) cuda
                                   (string-append cuda ":" prefix))))
                  ;; Help CMake discovery for CUDA/NPP
                  (setenv "CMAKE_PREFIX_PATH" merged)
                  (setenv "CMAKE_LIBRARY_PATH"
                          (let ((prev (or (getenv "CMAKE_LIBRARY_PATH") "")))
                            (if (string-null? prev) lib64
                                (string-append lib64 ":" prev))))
                  (setenv "CUDA_HOME" cuda)
                  (setenv "CUDA_TOOLKIT_ROOT_DIR" cuda)
                  (setenv "CUDA_PATH" cuda))))
            (add-after 'fix-rpath 'add-cuda-rpath
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (cuda (assoc-ref inputs "cuda-toolkit"))
                       (cuda-lib (string-append cuda "/lib64"))
                       (so-files (find-files out
                                             (lambda (file stat)
                                               (and (eq? 'regular
                                                         (stat:type stat))
                                                    (string-match
                                                     ".*/site-packages/torchcodec/libtorchcodec.*\\.so$"
                                                     file))))))
                  (for-each (lambda (file)
                              (let* ((p (open-pipe* OPEN_READ "patchelf"
                                                    "--print-rpath" file))
                                     (line (read-line p))
                                     (status (close-pipe p))
                                     (existing (if (and (zero? status) line)
                                                   line ""))
                                     (extra cuda-lib)
                                     (new-rpath (if (string=? existing "")
                                                    extra
                                                    (string-append existing
                                                                   ":" extra))))
                                (invoke "patchelf" "--set-rpath" new-rpath
                                        file))) so-files))))))))
    (inputs (modify-inputs (package-inputs python-torchcodec)
              (append cuda-toolkit)
              (replace "python-pytorch" python-pytorch-cuda)))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchcodec)
                         (replace "python-pytorch" python-pytorch-cuda)))
    (synopsis "A video decoder for PyTorch (CUDA-enabled)")
    (description
     "CUDA-enabled build of TorchCodec that links against NVIDIA's NPP libraries
from the CUDA toolkit and enables GPU acceleration for decoding/processing.")))

(define-public python-pycocotools
  (package
    (name "python-pycocotools")
    (version "2.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycocotools" version))
       (sha256
        (base32 "1qg2xkww0fqyc2dsjyrcshanxw0wls9psg1ia7hyb5gwvsf60ivs"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests require COCO dataset
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-cython python-numpy python-setuptools
                         python-wheel))
    (home-page "https://github.com/ppwwyyxx/cocoapi")
    (synopsis "Official APIs for the MS-COCO dataset")
    (description "Official APIs for the MS-COCO dataset.")
    (license license:bsd-2)))

(define-public python-pystoi
  (package
    (name "python-pystoi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pystoi" version))
       (sha256
        (base32 "1r76glpzq06nixwfl7581j1xja3jsbdqqi92r406pr7yzgb50vqw"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f)) ;Tests not included in PyPI release
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mpariente/pystoi")
    (synopsis "Computes Short Term Objective Intelligibility measure")
    (description
     "PySTOI provides a Python implementation of the Short Term Objective
Intelligibility (STOI) measure, which is used to predict the intelligibility
of speech signals.  STOI is commonly used in speech enhancement and audio
processing research to evaluate the quality of processed speech.")
    (license license:expat)))

(define-public libjpeg-turbo-3
  (package
    (inherit libjpeg-turbo)
    (name "libjpeg-turbo")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libjpeg-turbo/libjpeg-turbo"
                           "/releases/download/"
                           version
                           "/libjpeg-turbo-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "0sxpsp4pi5jcnbf3f1aap3ajhn1cj7psw3icbxlqsbnnwxcha4wr"))))))

(define-public python-hydra-core
  (package
    (name "python-hydra-core")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hydra-core" version))
       (sha256
        (base32 "0958j11rl9jb6filxyvsycdfhrs7gcpfga48klz7r68nfbb8x1wa"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Tests require pytest and other test dependencies
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check)))) ;Skip due to omegaconf also using different antlr
    (native-inputs (list python-setuptools python-wheel icedtea antlr4))
    (propagated-inputs (list python-antlr4-python3-runtime-4.9
                             python-omegaconf-2.2 python-packaging))
    (home-page "https://github.com/facebookresearch/hydra")
    (synopsis "Framework for elegantly configuring complex applications")
    (description
     "Hydra is a framework for elegantly configuring complex applications.
It enables you to compose your configuration dynamically, enabling you to
easily get the perfect configuration for each run.")
    (license license:expat)))

(define-public python-torchvggish
  (package
    (name "python-torchvggish")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "torchvggish" version))
       (sha256
        (base32 "0pz39ipdzng7dvr7swxyfxl0rz7hjy45yqlkyxxqcmxmgp0pqkgw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-resampy python-pytorch
                             python-soundfile))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/harritaylor/torchvggish")
    (synopsis "Pytorch port of Tensorflow's VGGish embedding model")
    (description
     "This package provides a Pytorch port of Tensorflow's VGGish embedding model.")
    (license license:asl2.0)))

(define-public python-torchvggish-cuda
  (package
    (inherit python-torchvggish)
    (name "python-torchvggish-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchvggish)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-scikit-base
  (package
    (name "python-scikit-base")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit_base" version))
       (sha256
        (base32 "1zm76iq11fpvdh47n2adjz33v9bs2g3b7x97yf0l956skla1m01g"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-coverage
                         python-numpy
                         python-pandas
                         pre-commit
                         python-pytest
                         python-pytest-cov
                         python-safety
                         python-scikit-learn
                         python-scipy
                         python-setuptools
                         python-toml
                         python-wheel))
    (home-page "https://github.com/sktime/base")
    (synopsis "Base classes for sklearn-like parametric objects")
    (description "Base classes for sklearn-like parametric objects.")
    (license license:bsd-3)))

(define-public python-signatory
  (package
    (name "python-signatory")
    (version "1.2.6.1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "signatory" version))
       (sha256
        (base32 "1n0753hidxhzdj3m39gw3mlff7qs0c3fkh0jgcf8qs46cbgigay9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pytorch))
    (native-inputs (list pybind11 python-setuptools python-wheel))
    (home-page "https://github.com/patrick-kidger/signatory")
    (synopsis
     "Differentiable computations of the signature and logsignature transforms, on both CPU and GPU.")
    (description
     "Differentiable computations of the signature and logsignature transforms, on
both CPU and GPU.")
    (license license:asl2.0)))

(define-public python-signatory-cuda
  (package
    (inherit python-signatory)
    (name "python-signatory-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-signatory)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-sktime
  (package
    (name "python-sktime")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sktime" version))
       (sha256
        (base32 "04wxbd085y42vxgx1zckyrixkqrvl78jhn6ijf7i2ajvlpx7df8s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-joblib
                             python-numpy
                             python-packaging
                             python-pandas
                             python-scikit-base
                             python-scikit-learn
                             python-scipy))
    (native-inputs (list python-backoff
                         python-boto3
                         python-botocore
                         python-httpx
                         python-moto
                         pre-commit
                         python-pytest
                         python-pytest-randomly
                         python-pytest-timeout
                         python-pytest-xdist
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/sktime/sktime")
    (synopsis "A unified framework for machine learning with time series")
    (description
     "This package provides a unified framework for machine learning with time series.")
    (license license:bsd-3)))

(define-public python-torchsde
  (package
    (name "python-torchsde")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "torchsde" version))
       (sha256
        (base32 "12vpnkjkk9k22bdqma23xq44dgmgjmim5ywl2q7ik7aga39p9l41"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-numpy python-scipy python-pytorch
                             python-trampoline))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/google-research/torchsde")
    (synopsis
     "SDE solvers and stochastic adjoint sensitivity analysis in PyTorch.")
    (description
     "SDE solvers and stochastic adjoint sensitivity analysis in @code{PyTorch}.")
    (license license:asl2.0)))

(define-public python-torchsde-cuda
  (package
    (inherit python-torchsde)
    (name "python-torchsde-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchsde)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-torchcde
  (package
    (name "python-torchcde")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "torchcde" version))
       (sha256
        (base32 "1rhd1zk1g03vvc37pfb8v0s739n6wracx5qdxca66iq05qcq2q3f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pytorch python-torchdiffeq python-torchsde))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/patrick-kidger/torchcde")
    (synopsis
     "Differentiable controlled differential equation solvers for PyTorch with GPU support and memory-efficient adjoint backpropagation.")
    (description
     "Differentiable controlled differential equation solvers for @code{PyTorch} with
GPU support and memory-efficient adjoint backpropagation.")
    (license license:asl2.0)))

(define-public python-torchcde-cuda
  (package
    (inherit python-torchcde)
    (name "python-torchcde-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchcde)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-iisignature
  (package
    (name "python-iisignature")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iisignature" version))
       (sha256
        (base32 "1imrjnv3ja9v1b7nf33bx0k45bc6b6riy693ygx8f15qx33194qb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/bottler/iisignature")
    (synopsis "Iterated integral signature calculations")
    (description "Iterated integral signature calculations.")
    (license license:expat)))

(define-public python-torchsignature
  (package
    (name "python-torchsignature")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/torchsignature")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18hdii0g39jiwc9fga7sfzrckf2d361c67hhv7hlmg97fmfnd4yd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-numpy python-pytorch
                             python-typing-extensions))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/b-vitamins/torchsignature")
    (synopsis "PyTorch signatures and logsignatures.")
    (description "PyTorch signatures and logsignatures.")
    (license license:expat)))

(define-public python-torchsignature-cuda
  (package
    (inherit python-torchsignature)
    (name "python-torchsignature-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchsignature)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public mujoco
  (package
    (name "mujoco")
    (version "3.3.7")
    (home-page "https://mujoco.org")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/deepmind/mujoco.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ahjk9kk3kw525wr056rgfz1wjr436lksgqap30a1d8p71061sx9"))
       (patches (search-patches
                 "myguix/patches/mujoco-skip-tmd-header-rewrite.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              ;; Avoid network and preseed third-party sources.
              "-DFETCHCONTENT_FULLY_DISCONNECTED=ON"
              ;; Build GUI examples and simulate using system GLFW.
              "-DMUJOCO_BUILD_EXAMPLES=ON"
              "-DMUJOCO_BUILD_SIMULATE=ON"
              "-DMUJOCO_SAMPLES_USE_SYSTEM_GLFW=ON"
              "-DMUJOCO_SIMULATE_USE_SYSTEM_GLFW=ON"
              (string-append "-Dglfw3_DIR="
                             #$(this-package-input "glfw") "/lib/cmake/glfw3")
              "-DMUJOCO_BUILD_TESTS=OFF"
              "-DMUJOCO_TEST_PYTHON_UTIL=OFF"
              ;; No USD by default.
              "-DMUJOCO_WITH_USD=OFF"
              (string-append "-DFETCHCONTENT_SOURCE_DIR_LODEPNG="
                             #$(this-package-native-input "lodepng-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_TINYXML2="
                             #$(this-package-native-input "tinyxml2-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_TINYOBJLOADER="
                             #$(this-package-native-input "tinyobjloader-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_CCD="
                             #$(this-package-native-input "libccd-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_QHULL="
                             #$(this-package-native-input "qhull-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_TRIANGLEMESHDISTANCE="
               #$(this-package-native-input "trianglemeshdistance-src"))
              (string-append "-DFETCHCONTENT_SOURCE_DIR_MARCHINGCUBECPP="
                             #$(this-package-native-input
                                "marchingcubecpp-src")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'alias-system-glfw-target
            (lambda _
              (define (inject-glfw-alias file)
                (substitute* file
                  (("EXCLUDE_FROM_ALL[[:space:]]*\\)" all)
                   (string-append "EXCLUDE_FROM_ALL)\n\n"
                    "# Guix: System GLFW may not export a `glfw` CMake target. Create a
"
                    "# compatible alias if alternative targets or variables are available.
"
                    "if(NOT TARGET glfw)\n"
                    "  if(TARGET glfw3::glfw)\n"
                    "    add_library(glfw INTERFACE IMPORTED)
"
                    "    set_target_properties(glfw PROPERTIES INTERFACE_LINK_LIBRARIES glfw3::glfw)
"
                    "  elseif(TARGET glfw3)\n"
                    "    add_library(glfw INTERFACE IMPORTED)
"
                    "    set_target_properties(glfw PROPERTIES INTERFACE_LINK_LIBRARIES glfw3)
"
                    "  else()\n"
                    "    # Try legacy FindGLFW3 variables.\n"
                    "    if(NOT DEFINED GLFW3_LIBRARY)\n"
                    "      find_package(glfw3 REQUIRED)\n"
                    "    endif()\n"
                    "    if(TARGET glfw)\n"
                    "      # nothing to do\n"
                    "    elseif(DEFINED GLFW3_LIBRARY OR DEFINED GLFW3_LIBRARIES)
"
                    "      add_library(glfw INTERFACE IMPORTED)
"
                    "      set(_GLFW_LINK_LIBS \"${GLFW3_LIBRARY}\")
"
                    "      if(DEFINED GLFW3_LIBRARIES)\n"
                    "        list(APPEND _GLFW_LINK_LIBS ${GLFW3_LIBRARIES})
"
                    "      endif()\n"
                    "      set_target_properties(glfw PROPERTIES INTERFACE_LINK_LIBRARIES \"${_GLFW_LINK_LIBS}\")
"
                    "      if(DEFINED GLFW3_INCLUDE_DIR)\n"
                    "        target_include_directories(glfw INTERFACE \"${GLFW3_INCLUDE_DIR}\")
"
                    "      endif()\n"
                    "    else()\n"
                    "      message(FATAL_ERROR \"GLFW found but no CMake target or GLFW3_LIBRARY provided.\")
"
                    "    endif()\n"
                    "  endif()\n"
                    "endif()\n"))))
              (inject-glfw-alias "sample/cmake/SampleDependencies.cmake")
              (inject-glfw-alias "simulate/cmake/SimulateDependencies.cmake")
              #t))
          (add-after 'install 'install-plugins
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/lib"))
                     (gcc-lib (assoc-ref inputs "gcc:lib"))
                     (libc (assoc-ref inputs "libc"))
                     (rpaths (cons libdir
                                   (append (if libc
                                               (list (string-append libc
                                                                    "/lib"))
                                               '())
                                           (if gcc-lib
                                               (list (string-append gcc-lib
                                                                    "/lib"))
                                               '()))))
                     (rpath (string-join rpaths ":"))
                     (names '("elasticity" "sensor" "sdf_plugin" "actuator"))
                     (patterns (map (lambda (n)
                                      (string-append "lib" n "\\.so$")) names))
                     (copied '()))
                (for-each (lambda (rx)
                            (let ((matches (find-files "." rx)))
                              (when (pair? matches)
                                (let* ((src (car matches))
                                       (dst libdir)
                                       (out-so (string-append dst "/"
                                                              (basename src))))
                                  (install-file src dst)
                                  ;; Set RPATH so plugins find libmujoco and toolchains.
                                  (make-file-writable out-so)
                                  (invoke "patchelf" "--set-rpath" rpath
                                          out-so)
                                  (set! copied
                                        (cons out-so copied)))))) patterns)
                (format #t "Installed ~a MuJoCo plugin libraries into ~a~%"
                        (length copied) libdir) #t)))
          (add-after 'install-plugins 'wrap-binaries
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (plugin-files (filter file-exists?
                                           (map (lambda (n)
                                                  (string-append lib "/" n))
                                                '("libactuator.so"
                                                  "libelasticity.so"
                                                  "libsdf_plugin.so"
                                                  "libsensor.so")))))
                (define (wrap-one name)
                  (let ((p (string-append bin "/" name)))
                    (when (file-exists? p)
                      (let ((envs (append `(("MUJOCO_PLUGIN_PATH" ":" prefix
                                             (,lib)))
                                          (if (null? plugin-files)
                                              '()
                                              `(("LD_PRELOAD" " " prefix
                                                 (,@plugin-files)))))))
                        (apply wrap-program p envs)))))
                (for-each wrap-one
                          '("simulate" "basic" "record" "compile"
                            "dependencies" "testspeed")) #t)))
          (add-after 'wrap-binaries 'install-desktop-files
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (apps (string-append out "/share/applications"))
                     (bin (string-append out "/bin")))
                (mkdir-p apps)
                (define (write-desktop name exec comment)
                  (with-output-to-file (string-append apps "/" name ".desktop")
                    (lambda ()
                      (display (string-append "[Desktop Entry]\n"
                                "Type=Application\n"
                                "Name="
                                comment
                                "\n"
                                "Exec="
                                exec
                                " %f\n"
                                "Icon="
                                name
                                "\n"
                                "Terminal=false\n"
                                "Categories=Science;Education;Graphics;Utility;
"
                                "Keywords=physics;simulator;MuJoCo;\n")))))
                (write-desktop "org.mujoco.Simulate"
                               (string-append bin "/simulate")
                               "MuJoCo Simulate")
                (write-desktop "org.mujoco.Basic"
                               (string-append bin "/basic")
                               "MuJoCo Basic Sample")
                (write-desktop "org.mujoco.Record"
                               (string-append bin "/record")
                               "MuJoCo Record Sample")
                #t)))
          (add-after 'install-desktop-files 'install-icons
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (icons (string-append out "/share/icons/hicolor"))
                     (apps (string-append icons "/256x256/apps"))
                     (ico #$(file-append (package-source mujoco)
                                         "/dist/mujoco.ico"))
                     (dest (string-append apps "/org.mujoco.Simulate.png")))
                (mkdir-p apps)
                ;; Try first frame of ICO explicitly, resize to 256x256.
                (invoke "convert"
                        (string-append ico "[0]") "-resize" "256x256" dest)
                (unless (file-exists? dest)
                  (invoke "convert" ico "-resize" "256x256" dest))
                (when (file-exists? dest)
                  (let ((basic (string-append apps "/org.mujoco.Basic.png"))
                        (record (string-append apps "/org.mujoco.Record.png")))
                    (false-if-exception (delete-file basic))
                    (false-if-exception (delete-file record))
                    (symlink dest basic)
                    (symlink dest record)))
                #t))))))
    (inputs (list (list "glfw" glfw)
                  (list "gcc:lib" gcc "lib")))
    (native-inputs (append (list (list "cmake" cmake-minimal)
                                 (list "pkg-config" pkg-config)
                                 (list "ninja" ninja)
                                 (list "patchelf" patchelf)
                                 (list "imagemagick" imagemagick))
                           ;; Vendored third-party sources for FetchContent.
                           (list (list "lodepng-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/lvandeve/lodepng.git")
                                                             (commit
                                                              "17d08dd26cac4d63f43af217ebd70318bfb8189c")))
                                         (file-name (git-file-name "lodepng"
                                                                   "git"))
                                         (sha256 (base32
                                                  "1703khw3j5v6qk989c6in96rpvvkayriywspxwwayqr5dpc3jz5y"))))
                                 (list "tinyxml2-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/leethomason/tinyxml2.git")
                                                             (commit
                                                              "e6caeae85799003f4ca74ff26ee16a789bc2af48")))
                                         (file-name (git-file-name "tinyxml2"
                                                                   "git"))
                                         (sha256 (base32
                                                  "0s4r3ysc8zbahhpl6sd8rqlzid6v52qm5c9kpmsp3mgzbrd4b48s"))))
                                 (list "tinyobjloader-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/tinyobjloader/tinyobjloader.git")
                                                             (commit
                                                              "1421a10d6ed9742f5b2c1766d22faa6cfbc56248")))
                                         (file-name (git-file-name
                                                     "tinyobjloader" "git"))
                                         (sha256 (base32
                                                  "03gbssdx9wd08fj7yq55031988d5zzlhmz4l8bj2a2lgymxqsggp"))))
                                 (list "libccd-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/danfis/libccd.git")
                                                             (commit
                                                              "7931e764a19ef6b21b443376c699bbc9c6d4fba8")))
                                         (file-name (git-file-name "libccd"
                                                                   "git"))
                                         (sha256 (base32
                                                  "0sfmn5pd7k5kyhbxnd689xmsa5v843r7sska96dlysqpljd691jc"))))
                                 (list "qhull-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/qhull/qhull.git")
                                                             (commit
                                                              "62ccc56af071eaa478bef6ed41fd7a55d3bb2d80")))
                                         (file-name (git-file-name "qhull"
                                                                   "git"))
                                         (sha256 (base32
                                                  "1z205prkzx63z3cps8s0h6hb2j9phly479v2ymasrz8b9ns4g34h"))))
                                 (list "trianglemeshdistance-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/InteractiveComputerGraphics/TriangleMeshDistance.git")
                                                             (commit
                                                              "2cb643de1436e1ba8e2be49b07ec5491ac604457")))
                                         (file-name (git-file-name
                                                     "TriangleMeshDistance"
                                                     "git"))
                                         (sha256 (base32
                                                  "15ys0d2c08d57q4qk8dxpx9fg0hnhczjwrwx81a4m7afm90gqvx8"))))
                                 (list "marchingcubecpp-src"
                                       (origin
                                         (method git-fetch)
                                         (uri (git-reference (url
                                                              "https://github.com/aparis69/MarchingCubeCpp.git")
                                                             (commit
                                                              "f03a1b3ec29b1d7d865691ca8aea4f1eb2c2873d")))
                                         (file-name (git-file-name
                                                     "MarchingCubeCpp" "git"))
                                         (sha256 (base32
                                                  "077hgjd1w0nmxladjv9zmyv11pcryxkaqd32akpca0s9bb9a4izp")))))))
    (synopsis "MuJoCo physics simulator (C/C++ library)")
    (description
     "MuJoCo (Multi-Joint dynamics with Contact) is a physics engine for
research and development in robotics, biomechanics, graphics and animation.
This package builds the core C/C++ shared library and installs headers and
models; examples and Python bindings are not built.")
    (license license:asl2.0)))

(define-public python-gymnasium
  (package
    (name "python-gymnasium")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gymnasium" version))
       (sha256
        (base32 "0v5njl27fxdnqj4dwrh8z8akzc8ic7xvx6fvnf83lli86lkq0r2f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-cloudpickle python-farama-notifications
                             python-numpy python-typing-extensions))
    (native-inputs (list python-array-api-extra
                         python-dill
                         python-pytest
                         python-scipy
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/Farama-Foundation/Gymnasium")
    (synopsis
     "A standard API for reinforcement learning and a diverse set of reference environments (formerly Gym).")
    (description
     "This package provides a standard API for reinforcement learning and a diverse
set of reference environments (formerly Gym).")
    (license license:expat)))

(define-public python-tianshou
  (package
    (name "python-tianshou")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tianshou" version))
       (sha256
        (base32 "0cl7yiv4fjhn19w8faa9z40jm4c80x0hbz1w8v2hljn0k724b7m7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/thu-ml/tianshou")
    (synopsis "A Library for Deep Reinforcement Learning")
    (description
     "This package provides a Library for Deep Reinforcement Learning.")
    (license license:expat)))

(define-public python-thop
  (package
    (name "python-thop")
    (version "2.0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ultralytics/thop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ghdcgfnnjskl9z7c5q5k3q9062nhcfspwind1n5q3mknr4p1czd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'rename-dist-to-thop
            (lambda _
              ;; Upstream publishes as "ultralytics-thop" on PyPI, but
              ;; RecBole requires the distribution name "thop".
              ;; Rename the project for packaging so pkg_resources can
              ;; resolve the requirement "thop>=0.1.1" during sanity-check.
              (when (file-exists? "pyproject.toml")
                (substitute* "pyproject.toml"
                  (("name = \"ultralytics-thop\"")
                   "name = \"thop\""))))))))
    (propagated-inputs (list python-numpy python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ultralytics/thop")
    (synopsis "Compute FLOPs and parameter counts of PyTorch models")
    (description
     "THOP provides utilities to profile PyTorch models, including fast
computation of floating-point operations (FLOPs) and parameter counts.  This
package tracks the upstream Ultralytics fork used widely by Ultralytics
<projects.")
    (license license:agpl3+)))

(define-public python-thop-cuda
  (package
    (inherit python-thop)
    (name "python-thop-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs python-thop)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-recbole
  (package
    (name "python-recbole")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "recbole" version))
       (sha256
        (base32 "1a6hm6lzy3qi5xjmvq5p3sdljnimqvbvslmcnmy2vipigfz982ph"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-dependencies
            (lambda _
              (when (file-exists? "setup.py")
                (substitute* "setup.py"
                  ;; Relax Ray upper bound to allow newer Ray.
                  (("ray>=1\\.13\\.0, <=2\\.6\\.3")
                   "ray>=1.13.0")
                  (("colorama==0\\.4\\.4")
                   "colorama>=0.4.4")
                  (("colorlog==4\\.7\\.2")
                   "colorlog>=4.7.2"))))))))
    (propagated-inputs (list python-colorama
                             python-colorlog
                             python-numpy
                             python-pandas
                             python-plotly
                             python-psutil
                             python-pyyaml
                             python-ray
                             python-scikit-learn
                             python-scipy
                             python-tabulate
                             python-tensorboard
                             python-texttable
                             python-thop
                             python-pytorch
                             python-tqdm))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/RUCAIBox/RecBole")
    (synopsis "A unified, comprehensive and efficient recommendation library")
    (description
     "This package provides a unified, comprehensive and efficient recommendation
library.")
    (license license:expat)))

(define-public python-recbole-cuda
  (package
    (inherit python-recbole)
    (name "python-recbole-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-recbole)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-setuptools-git-versioning
  (package
    (name "python-setuptools-git-versioning")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools_git_versioning" version))
       (sha256
        (base32 "08cr0lm0949mvphc48zflbr6rnb1qnzprlj3nfv57ffgn65mpvva"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-packaging python-setuptools))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://setuptools-git-versioning.readthedocs.io")
    (synopsis
     "Use git repo data for building a version number according PEP-440")
    (description
     "Use git repo data for building a version number according PEP-440.")
    (license license:expat)))

(define-public python-logistro
  (package
    (name "python-logistro")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "logistro" version))
       (sha256
        (base32 "058z3qvnwss9vq3ahb3kwq8jl516imdm3iv70yg69kj2b269mv4a"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-setuptools-git-versioning
                         python-wheel))
    (home-page #f)
    (synopsis "Simple wrapper over logging for a couple basic features")
    (description "Simple wrapper over logging for a couple basic features.")
    (license #f)))

(define-public python-choreographer
  (package
    (name "python-choreographer")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "choreographer" version))
       (sha256
        (base32 "0inkg58d3269wn81kbpwj172y1wykvk3jqa7w5l1vj6vndpaykn9"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-logistro python-simplejson))
    (native-inputs (list python-setuptools python-setuptools-git-versioning
                         python-wheel))
    (home-page #f)
    (synopsis "Devtools Protocol implementation for chrome.")
    (description "Devtools Protocol implementation for chrome.")
    (license #f)))

(define-public python-kaleido
  (package
    (name "python-kaleido")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kaleido" version))
       (sha256
        (base32 "1djjmfqb6raw93nf0j58dd8yzs5zqals9glslvpk9h6laqx70isp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)
          (add-after 'unpack 'relax-dependencies
            (lambda _
              (when (file-exists? "pyproject.toml")
                (substitute* "pyproject.toml"
                  (("pytest-timeout>=2\\.4\\.0")
                   "pytest-timeout>=2.3.1")
                  (("orjson>=3\\.10\\.15")
                   "orjson>=3.9.7"))))))))
    (propagated-inputs (list python-choreographer python-logistro
                             python-orjson python-packaging
                             python-pytest-timeout))
    (native-inputs (list python-setuptools
                         python-setuptools-git-versioning
                         python-wheel
                         python-pytest
                         python-pytest-asyncio
                         python-plotly))
    (home-page #f)
    (synopsis "Plotly graph export library")
    (description "Plotly graph export library.")
    (license #f)))

(define-public python-optuna
  (package
    (name "python-optuna")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "optuna" version))
       (sha256
        (base32 "1fj2k7wxj0d26lndarvmqhln2r0qqa5ps1cmlbg49mys2vd48j16"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-alembic
                             python-colorlog
                             python-numpy
                             python-packaging
                             python-pyyaml
                             python-sqlalchemy
                             python-tqdm))
    (native-inputs (list python-coverage
                         python-fakeredis
                         python-grpcio
                         python-kaleido
                         python-moto
                         python-protobuf
                         python-pytest
                         python-pytest-xdist
                         python-scipy
                         python-setuptools
                         python-pytorch
                         python-wheel))
    (home-page #f)
    (synopsis "A hyperparameter optimization framework")
    (description
     "This package provides a hyperparameter optimization framework.")
    (license #f)))

(define-public python-webdataset
  (package
    (name "python-webdataset")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webdataset" version))
       (sha256
        (base32 "1d5azlddclwghghip9kn0r0nlb2fl9l8g99hak64dykwhaz9h13z"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-braceexpand python-numpy python-pyyaml))
    (native-inputs (list python-autoflake
                         python-bandit
                         python-black
                         python-bump2version
                         python-diffusers
                         python-flake8
                         python-icecream
                         python-imageio
                         python-isort
                         python-jupyter
                         python-jupyterlab
                         python-lmdb
                         python-matplotlib
                         python-mkdocs-next
                         python-mkdocs-autorefs
                         python-mkdocs-jupyter
                         python-mkdocs-material
                         python-mkdocs-material-extensions-next
                         python-mkdocs-minify-plugin
                         python-mkdocstrings
                         python-mkdocstrings
                         python-msgpack
                         python-mypy
                         python-nbconvert
                         python-notebook
                         python-papermill
                         python-pdm
                         python-pillow
                         pre-commit
                         python-pydocstyle
                         python-pytest
                         python-pytest-cov
                         python-pytorch-lightning
                         python-ray
                         python-ruff
                         python-scipy
                         python-setuptools
                         python-pytorch
                         python-torchvision
                         python-transformers
                         python-twine
                         python-typer
                         python-types-pyyaml
                         python-wheel))
    (home-page #f)
    (synopsis
     "High performance storage and I/O for deep learning and data processing.")
    (description
     "High performance storage and I/O for deep learning and data processing.")
    (license #f)))

(define-public python-webdataset-cuda
  (package
    (inherit python-webdataset)
    (name "python-webdataset-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-webdataset)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchvision" python-torchvision-cuda)))))

(define-public python-torchlibrosa
  (package
    (name "python-torchlibrosa")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "torchlibrosa" version))
       (sha256
        (base32 "0a200kpw97j80bpcqy3nws3bmxr90bqz6k930qd19d69z7nvxa32"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-librosa python-numpy python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/qiuqiangkong/torchlibrosa")
    (synopsis "PyTorch implemention of part of librosa functions.")
    (description "@code{PyTorch} implemention of part of librosa functions.")
    (license license:expat)))

(define-public python-torchlibrosa-cuda
  (package
    (inherit python-torchlibrosa)
    (name "python-torchlibrosa-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchlibrosa)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-laion-clap
  (package
    (name "python-laion-clap")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "laion_clap" version))
       (sha256
        (base32 "15s3c1bbpa87zv3n5n602fylx08lkhmfz1b6gpv482cydciwbfcs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Avoid network access during import triggered by data.py creating a
      ;; BertTokenizer at module import time. This breaks Guix's offline build
      ;; sanity-check phase. Patch it to try local cache only and fall back to
      ;; a harmless None when unavailable.
      #:phases '(modify-phases %standard-phases
                  (add-after 'unpack 'avoid-online-downloads-on-import
                    (lambda _
                      (let* ((data-target "src/laion_clap/training/data.py")
                             (init-target "src/laion_clap/__init__.py"))
                        (when (file-exists? data-target)
                          (substitute* data-target
                            (("bert_tokenizer\\s*=\\s*BertTokenizer\\.from_pretrained\\(\\\"bert-base-uncased\\\"\\)"
                              all)
                             (string-append "import warnings\n"
                              "try:\n"
                              "    bert_tokenizer = BertTokenizer.from_pretrained(\"bert-base-uncased\", local_files_only=True)
"
                              "except Exception as e:\n"
                              "    warnings.warn(f\"Skipping online pretrained model load during import: {e}\")
"
                              "    bert_tokenizer = None\n"))
                            (("roberta_tokenizer\\s*=\\s*RobertaTokenizer\\.from_pretrained\\(\\\"roberta-base\\\"\\)"
                              all)
                             (string-append "import warnings\n"
                              "try:\n"
                              "    roberta_tokenizer = RobertaTokenizer.from_pretrained(\"roberta-base\", local_files_only=True)
"
                              "except Exception as e:\n"
                              "    warnings.warn(f\"Skipping online pretrained model load during import: {e}\")
"
                              "    roberta_tokenizer = None\n"))))
                        ;; Inject lazy loader and remove warnings in data.py
                        (when (file-exists? data-target)
                          (substitute* data-target
                            (("^[[:space:]]*warnings\\.warn\\(.*\\)$" all)
                             "")))
                        (when (file-exists? data-target)
                          (substitute* data-target
                            (("^bert_tokenizer = None$" all)
                             (string-append "bert_tokenizer = None\n"
                              "try:\n"
                              "    _LazyLoad\n"
                              "except NameError:\n"
                              "    class _LazyLoad:\n"
                              "        def __init__(self, local_loader, remote_loader=None):
"
                              "            self._local_loader = local_loader
"
                              "            self._remote_loader = remote_loader
"
                              "            self._obj = None\n"
                              "        def _load(self):\n"
                              "            if self._obj is None:\n"
                              "                try:\n"
                              "                    self._obj = self._local_loader()
"
                              "                except Exception:\n"
                              "                    if self._remote_loader is None:
"
                              "                        raise\n"
                              "                    self._obj = self._remote_loader()
"
                              "            return self._obj\n"
                              "        def __getattr__(self, name):\n"
                              "            return getattr(self._load(), name)
"
                              "\n"
                              "bert_tokenizer = _LazyLoad(\n"
                              "    lambda: BertTokenizer.from_pretrained(\"bert-base-uncased\", local_files_only=True),
"
                              "    lambda: BertTokenizer.from_pretrained(\"bert-base-uncased\")
"
                              ")\n"))
                            (("^roberta_tokenizer = None$" all)
                             (string-append "roberta_tokenizer = None\n"
                              "try:\n"
                              "    _LazyLoad\n"
                              "except NameError:\n"
                              "    class _LazyLoad:\n"
                              "        def __init__(self, local_loader, remote_loader=None):
"
                              "            self._local_loader = local_loader
"
                              "            self._remote_loader = remote_loader
"
                              "            self._obj = None\n"
                              "        def _load(self):\n"
                              "            if self._obj is None:\n"
                              "                try:\n"
                              "                    self._obj = self._local_loader()
"
                              "                except Exception:\n"
                              "                    if self._remote_loader is None:
"
                              "                        raise\n"
                              "                    self._obj = self._remote_loader()
"
                              "            return self._obj\n"
                              "        def __getattr__(self, name):\n"
                              "            return getattr(self._load(), name)
"
                              "\n"
                              "roberta_tokenizer = _LazyLoad(\n"
                              "    lambda: RobertaTokenizer.from_pretrained(\"roberta-base\", local_files_only=True),
"
                              "    lambda: RobertaTokenizer.from_pretrained(\"roberta-base\")
"
                              ")\n"))))
                        (when (file-exists? init-target)
                          (substitute* init-target
                            (("^from \\.hook import CLAP_Module" all)
                             (string-append "import warnings\n"
                              "try:\n"
                              "    from .hook import CLAP_Module\n"
                              "except Exception as e:\n"
                              "    warnings.warn(f\"Skipping optional import during init: {e}\")
"
                              "    CLAP_Module = None\n"))))
                        ;; Remove warnings from __init__.py too.
                        (when (file-exists? init-target)
                          (substitute* init-target
                            (("^[[:space:]]*warnings\\.warn\\(.*\\)$" all)
                             "")))))))))
    (propagated-inputs (list python-braceexpand
                             python-ftfy
                             python-h5py
                             python-librosa
                             python-llvmlite
                             python-numpy
                             python-pandas
                             python-progressbar
                             python-regex
                             python-scikit-learn
                             python-scipy
                             python-soundfile
                             python-torchlibrosa
                             python-torchvision
                             python-tqdm
                             python-transformers
                             python-wandb
                             python-webdataset
                             python-wget))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/LAION-AI/CLAP")
    (synopsis "Contrastive Language-Audio Pretraining Model from LAION")
    (description "Contrastive Language-Audio Pretraining Model from LAION.")
    (license license:cc0)))

(define-public python-laion-clap-cuda
  (package
    (inherit python-laion-clap)
    (name "python-laion-clap-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-laion-clap)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchlibrosa"
                                  python-torchlibrosa-cuda)
                         (replace "python-torchvision" python-torchvision-cuda)
                         (replace "python-webdataset" python-webdataset-cuda)))))

(define-public python-open-clip-torch
  (package
    (name "python-open-clip-torch")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "open_clip_torch" version))
       (sha256
        (base32 "01i59jbw4n5yp8rwfnjqfkfi62ms5xvgl6a8qsvhzi6c28q79dv2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-ftfy
                             python-huggingface-hub
                             python-regex
                             python-safetensors
                             python-timm
                             python-pytorch
                             python-torchvision
                             python-tqdm))
    (native-inputs (list python-pdm-backend python-pytest))
    (home-page "https://github.com/mlfoundations/open_clip")
    (synopsis
     "Open reproduction of consastive language-image pretraining (CLIP) and related.")
    (description
     "Open reproduction of consastive language-image pretraining (CLIP) and related.")
    (license license:expat)))

(define-public python-open-clip-torch-cuda
  (package
    (inherit python-open-clip-torch)
    (name "python-open-clip-torch-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-open-clip-torch)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchvision" python-torchvision-cuda)))))

(define-public python-axial-positional-embedding
  (package
    (name "python-axial-positional-embedding")
    (version "0.3.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "axial_positional_embedding" version))
       (sha256
        (base32 "05xw61hrvg07bqbbj0l274fkzihgzckysdhbsb9x984g5ws8zn8h"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-einops python-pytorch))
    (native-inputs (list python-hatchling))
    (home-page "https://pypi.org/project/axial_positional_embedding/")
    (synopsis "Axial Positional Embedding")
    (description "Axial Positional Embedding.")
    (license license:expat)))

(define-public python-axial-positional-embedding-cuda
  (package
    (inherit python-axial-positional-embedding)
    (name "python-axial-positional-embedding-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-axial-positional-embedding)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-hyper-connections
  (package
    (name "python-hyper-connections")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hyper_connections" version))
       (sha256
        (base32 "081fmidcwc0mml6lkp7mah083wg2wn85fal5p14xclkdxjh50y79"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-einops python-pytorch))
    (native-inputs (list python-hatchling))
    (home-page "https://pypi.org/project/hyper-connections/")
    (synopsis "Hyper-Connections")
    (description "Hyper-Connections.")
    (license license:expat)))

(define-public python-hyper-connections-cuda
  (package
    (inherit python-hyper-connections)
    (name "python-hyper-connections-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-hyper-connections)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-local-attention
  (package
    (name "python-local-attention")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucidrains/local-attention")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ib2dd46w7mvf2kk6ahfy9rcg1y3rl4yf2wgxpgm4zj0n804y06s"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-einops python-hyper-connections
                             python-pytorch))
    (native-inputs (list python-setuptools python-wheel python-pytest))
    (home-page "https://github.com/lucidrains/local-attention")
    (synopsis "Local attention, window with lookback, for language modeling")
    (description
     "Local attention, window with lookback, for language modeling.")
    (license license:expat)))

(define-public python-local-attention-cuda
  (package
    (inherit python-local-attention)
    (name "python-local-attention-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-local-attention)
                         (replace "python-hyper-connections"
                                  python-hyper-connections-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-linformer
  (package
    (name "python-linformer")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucidrains/linformer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c94zssa97jsv1sirrh6qg7wsk7plaxlhmsphb8fy50z0hzxbpnw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/lucidrains/linformer")
    (synopsis "Linformer implementation in Pytorch")
    (description "Linformer implementation in Pytorch.")
    (license license:expat)))

(define-public python-linformer-cuda
  (package
    (inherit python-linformer)
    (name "python-linformer-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-linformer)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-colt5-attention
  (package
    (name "python-colt5-attention")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucidrains/CoLT5-attention")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sfi7xlqysl1yk2hl4fq4jsz7j074k2v204ipz0wmfwqklynng41"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-einops python-local-attention
                             python-packaging python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/lucidrains/CoLT5-attention")
    (synopsis "Conditionally Routed Attention")
    (description "Conditionally Routed Attention.")
    (license license:expat)))

(define-public python-colt5-attention-cuda
  (package
    (inherit python-colt5-attention)
    (name "python-colt5-attention-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-colt5-attention)
                         (replace "python-local-attention"
                                  python-local-attention-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-product-key-memory
  (package
    (name "python-product-key-memory")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "product_key_memory" version))
       (sha256
        (base32 "0ajs0vppvhl4hqy26j8h7r8g887gzs9sdz0bfkk2aam63pkix5p4"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-colt5-attention python-einops
                             python-pytorch))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/lucidrains/product-key-memory")
    (synopsis "Product Key Memory")
    (description "Product Key Memory.")
    (license license:expat)))

(define-public python-product-key-memory-cuda
  (package
    (inherit python-product-key-memory)
    (name "python-product-key-memory-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-product-key-memory)
                         (replace "python-colt5-attention"
                                  python-colt5-attention-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-linear-attention-transformer
  (package
    (name "python-linear-attention-transformer")
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lucidrains/linear-attention-transformer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07llz3613lkm03zk891cs6vx0884jf46ns7mv99cblhrmamv4zg8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-axial-positional-embedding
                             python-einops
                             python-linformer
                             python-local-attention
                             python-product-key-memory
                             python-pytorch))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/lucidrains/linear-attention-transformer")
    (synopsis "Linear Attention Transformer")
    (description "Linear Attention Transformer.")
    (license license:expat)))

(define-public python-linear-attention-transformer-cuda
  (package
    (inherit python-linear-attention-transformer)
    (name "python-linear-attention-transformer-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-linear-attention-transformer)
                         (replace "python-axial-positional-embedding"
                                  python-axial-positional-embedding-cuda)
                         (replace "python-linformer" python-linformer-cuda)
                         (replace "python-local-attention"
                                  python-local-attention-cuda)
                         (replace "python-product-key-memory"
                                  python-product-key-memory-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)))))

(define-public python-skorch
  (package
    (name "python-skorch")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skorch" version))
       (sha256
        (base32 "001yv7hzyfk5h1fn33idhlq3d5fhpncmknid31gn0h1xzmgy9ylq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-numpy python-scikit-learn python-scipy
                             python-tabulate python-tqdm))
    (native-inputs (list python-flake8
                         python-flaky
                         python-future
                         python-pylint
                         python-pytest
                         python-pytest-cov
                         python-pytorch
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/skorch-dev/skorch")
    (synopsis "scikit-learn compatible neural network library for pytorch")
    (description "scikit-learn compatible neural network library for pytorch.")
    (license license:bsd-3)))

(define-public python-pytorch-msssim
  (package
    (name "python-pytorch-msssim")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VainF/pytorch-msssim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zqlj9lsmqs3c15r7rw5s9wpmysa4vr0x6qsxj11r0110jbn023f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pytorch))
    (native-inputs (list python-setuptools python-wheel python-pytest))
    (home-page "https://github.com/VainF/pytorch-msssim")
    (synopsis "Fast and differentiable MS-SSIM and SSIM for pytorch.")
    (description "Fast and differentiable MS-SSIM and SSIM for pytorch.")
    (license license:expat)))

(define-public python-compressai
  (package
    (name "python-compressai")
    (version "1.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/InterDigitalInc/CompressAI")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mc2lkn1l32vmbzdnlbaapap1797l4szsgasafdhfl371n5apbim"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-einops
                             python-matplotlib
                             python-numpy
                             python-pandas
                             python-pybind11
                             python-pytorch-msssim
                             python-scipy
                             python-setuptools
                             python-tomli
                             python-pytorch
                             python-pytorch-geometric
                             python-torchvision
                             python-tqdm
                             python-typing-extensions
                             python-wheel))
    (native-inputs (list python-black
                         python-flake8
                         python-flake8-bugbear
                         python-flake8-comprehensions
                         python-isort
                         python-mypy
                         python-plotly
                         python-pybind11
                         python-pytest
                         python-pytest-cov
                         python-ruff
                         python-setuptools
                         python-tomli
                         python-wheel))
    (home-page "https://github.com/InterDigitalInc/CompressAI")
    (synopsis
     "A PyTorch library and evaluation platform for end-to-end compression research")
    (description
     "This package provides a @code{PyTorch} library and evaluation platform for
end-to-end compression research.")
    (license #f)))

(define-public python-torchinfo
  (package
    (name "python-torchinfo")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tyleryep/torchinfo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m77krkzw4i7x6xfzmpljmnsib64f4rj6slbdywcqkqsvziy1y54"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-setuptools
                         python-wheel
                         python-pytest
                         python-numpy
                         python-pytorch
                         python-torchvision
                         python-compressai))
    (home-page "https://github.com/tyleryep/torchinfo")
    (synopsis
     "Model summary in PyTorch, based off of the original torchsummary.")
    (description
     "Model summary in @code{PyTorch}, based off of the original torchsummary.")
    (license license:expat)))

(define-public python-braindecode
  (package
    (name "python-braindecode")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "braindecode" version))
       (sha256
        (base32 "0jpq5axp96yk4wmfsc2750jimv45zsxdka06rv05s4h43za62ldj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-docstring-inheritance
                             python-einops
                             python-h5py
                             python-joblib
                             python-linear-attention-transformer
                             python-matplotlib
                             python-mne
                             python-mne-bids
                             python-numpy
                             python-pandas
                             python-scipy-next
                             python-skorch
                             python-pytorch
                             python-torchaudio
                             python-torchinfo
                             python-wfdb))
    (native-inputs (list python-codecov
                         python-mypy
                         python-pytest
                         python-pytest-cov
                         python-pytest-cases
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/braindecode/braindecode")
    (synopsis "Deep learning software to decode EEG, ECG or MEG signals")
    (description "Deep learning software to decode EEG, ECG or MEG signals.")
    (license license:bsd-3)))

(define-public python-braindecode-cuda
  (package
    (inherit python-braindecode)
    (name "python-braindecode-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-braindecode)
                         (replace "python-linear-attention-transformer"
                                  python-linear-attention-transformer-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)
                         (replace "python-torchaudio" python-torchaudio-cuda)))))

(define-public python-fvcore
  (package
    (name "python-fvcore")
    (version "0.1.5.post20221221")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fvcore" version))
       (sha256
        (base32 "0q3q0gfpps3d5nxlnzhaam0256yi7r4j13n724f6bbkj0nwhpyzj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-iopath
                             python-numpy
                             python-pillow
                             python-pyyaml
                             python-tabulate
                             python-termcolor
                             python-tqdm
                             python-yacs))
    (native-inputs (list python-setuptools python-wheel python-pytest))
    (home-page "https://github.com/facebookresearch/fvcore")
    (synopsis
     "Collection of common code shared among different research projects in FAIR computer vision team")
    (description
     "Collection of common code shared among different research projects in FAIR
computer vision team.")
    (license license:asl2.0)))

(define-public python-pytorchvideo
  (package
    (name "python-pytorchvideo")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytorchvideo" version))
       (sha256
        (base32 "06rlys6352nhr9w5v3dss15rhmv6nlbyq34xllnl4w7bgx6458v9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-av python-fvcore python-iopath
                             python-networkx python-parameterized))
    (native-inputs (list python-autoflake
                         python-black
                         python-bs4
                         python-coverage
                         python-decord
                         python-flake8
                         python-flake8-bugbear
                         python-flake8-comprehensions
                         python-isort
                         python-nbconvert
                         pre-commit
                         python-pytest
                         python-setuptools
                         python-sphinx
                         python-wheel))
    (home-page "https://github.com/facebookresearch/pytorchvideo")
    (synopsis "A video understanding deep learning library.")
    (description
     "This package provides a video understanding deep learning library.")
    (license license:asl2.0)))

(define-public python-pyriemann
  (package
    (name "python-pyriemann")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyriemann" version))
       (sha256
        (base32 "00wqb6av50p4010hbvdmxmzcx00v606q8nzfgp5ppz0gyvsm4z4l"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check)))) ;Skip because matplotlib requires numpy<2,>=1.21
    (propagated-inputs (list python-joblib python-matplotlib python-numpy-2
                             python-scikit-learn python-scipy))
    (native-inputs (list python-flake8 python-pytest python-seaborn
                         python-setuptools python-wheel))
    (home-page "https://pyriemann.readthedocs.io")
    (synopsis
     "Machine learning for multivariate data with Riemannian geometry")
    (description
     "Machine learning for multivariate data with Riemannian geometry.")
    (license license:bsd-3)))

(define-public python-edflib
  (package
    (name "python-edflib")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "EDFlib-Python" version))
       (sha256
        (base32 "069f7ypzm7qzvlsi7a1pp5lpn8x36z4dsg2fqky3g7w0h1wkppj2"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.teuniz.net/edflib_python/")
    (synopsis
     "Library to read/write EDF+/BDF+ files written in pure Python by the same author as the original EDFlib.")
    (description
     "Library to read/write EDF+/BDF+ files written in pure Python by the same author
as the original EDFlib.")
    (license license:bsd-3)))

(define-public python-edfio
  (package
    (name "python-edfio")
    (version "0.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "edfio" version))
       (sha256
        (base32 "1g3lgf71nx1l01jqjggxadpjv8k7svhsdb2sr6pzx8jq60fs1bqf"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-typing-extensions))
    (native-inputs (list python-hatch-vcs python-hatchling))
    (home-page "https://github.com/the-siesta-group/edfio")
    (synopsis "Read and write EDF/EDF+C/BDF/BDF+C files.")
    (description "Read and write EDF/EDF+C/BDF/BDF+C files.")
    (license license:asl2.0)))

(define-public python-moabb
  (package
    (name "python-moabb")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "moabb" version))
       (sha256
        (base32 "1n1qwpiq0anbmmcvygg0kidcps9gds8hpd7y48lhbgvh08xxfhdp"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check)))) ;Skip because matplotlib requires numpy<2,>=1.21
    (propagated-inputs (list python-coverage
                             python-edfio
                             python-edflib
                             python-h5py
                             python-matplotlib
                             python-memory-profiler
                             python-mne
                             python-mne-bids
                             python-numpy-2
                             python-pandas
                             python-pooch
                             python-pyriemann
                             python-pytest
                             python-pyyaml
                             python-requests
                             python-scikit-learn
                             python-scipy
                             python-seaborn
                             python-tqdm
                             python-urllib3))
    (native-inputs (list python-codecov
                         python-pytest
                         python-pytest-cov
                         python-pytest-xdist
                         python-pytest-cases
                         python-setuptools
                         python-wheel
                         python-filelock
                         python-optuna))
    (home-page "https://github.com/NeuroTechX/moabb")
    (synopsis "Mother of All BCI Benchmarks")
    (description "Mother of All BCI Benchmarks.")
    (license license:bsd-3)))

(define-public python-torchdyn
  (package
    (name "python-torchdyn")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "torchdyn" version))
       (sha256
        (base32 "1gyd910k3jbpy0r3v9qca0v21yrr3pqil1nwclma9j6rwf2xfx1v"))))
    (build-system pyproject-build-system)
    (native-inputs (list poetry
                         python-setuptools-cpp
                         python-wheel
                         python-pytorch
                         python-torchcde
                         python-pytorch-lightning
                         python-scikit-learn))
    (home-page "https://github.com/DiffEqML/torchdyn")
    (synopsis
     "A PyTorch library entirely dedicated to neural differential equations, implicit models and related numerical methods.")
    (description
     "This package provides a @code{PyTorch} library entirely dedicated to neural
differential equations, implicit models and related numerical methods.")
    (license license:asl2.0)))

(define-public python-poethepoet
  (package
    (name "python-poethepoet")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poethepoet" version))
       (sha256
        (base32 "17713bbnm51mn6zkhjmqhbgwsippws2kwybh9i3dqy5rjjqprfbh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pastel python-pyyaml python-tomli))
    (native-inputs (list poetry python-setuptools python-pytest))
    (home-page "https://poethepoet.natn.io/")
    (synopsis "A task runner that works well with poetry and uv.")
    (description
     "This package provides a task runner that works well with poetry and uv.")
    (license license:expat)))

(define-public python-torchcfm
  (package
    (name "python-torchcfm")
    (version "1.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atong01/conditional-flow-matching")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yz1s4imi1hp9qmf8ly574mz3siihjwf1r2hjy07blha92zwnml5"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-absl-py
                             python-ipywidgets
                             python-ipykernel
                             python-matplotlib
                             python-numpy
                             python-pandas
                             python-poethepoet
                             python-pot
                             python-scikit-learn
                             python-scipy
                             python-pytorch
                             python-tomlkit
                             python-torchsde
                             python-torchcde
                             python-torchdiffeq
                             python-torchdyn))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/atong01/conditional-flow-matching")
    (synopsis
     "Conditional Flow Matching for Fast Continuous Normalizing Flow Training.")
    (description
     "Conditional Flow Matching for Fast Continuous Normalizing Flow Training.")
    (license license:expat)))

(define-public python-torchcfm-cuda
  (package
    (inherit python-torchdyn)
    (name "python-torchcfm-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       python-torchcfm)
                         (replace "python-torchcde" python-torchcde-cuda)
                         (replace "python-torchsde" python-torchsde-cuda)
                         (replace "python-torch-diffeq"
                                  python-torch-diffeq-cuda)
                         (replace "python-pytorch" python-pytorch-cuda)))))

;; TODO: Lab-level R&D essential packages to add:
;; - python-pytorch3d: 3D deep learning with differentiable rendering
;; - python-detectron2: Facebook's detection/segmentation platform
;; - python-monai: Medical imaging deep learning framework
;; - python-imgaug: Advanced image augmentation library
;; - python-pytorch-ignite: High-level training abstractions
;; - python-fiftyone: Dataset visualization and debugging tool
;; - python-dvc: Data version control for ML
;; - python-aim: Lightweight experiment tracking
;; - python-lightning-bolts: Pre-built PyTorch Lightning models
