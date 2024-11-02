(define-module (myguix packages machine-learning)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix build-system bazel)
  #:use-module (myguix packages rust-pqrs)
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

(define jaxlib-system-libs
  (list "absl_py"
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
        "zlib"))

(define python-jaxlib/wheel
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
                             python-protobuf-for-tensorflow-2
                             python-scipy))
    (native-inputs `(("python-pypa-build" ,python-pypa-build)
                     ("python-setuptools" ,python-setuptools)
                     ("python-wheel" ,python-wheel)
                     ("bazel-platforms" ,(origin
                                           (method git-fetch)
                                           (uri (git-reference (url
                                                                "https://github.com/bazelbuild/platforms")
                                                               (commit "0.0.8")))
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
    (license license:asl2.0)))

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
