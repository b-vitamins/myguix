(define-module (myguix packages python-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages image)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages check)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix base32)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (myguix packages bazel)
  #:use-module (ice-9 match))

(define-public python-pele
  (package
    (name "python-pele")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pele-python/pele")
             (commit "7da72ba")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8s182w3i6dzjgwzgj9pb927fd3yqypma9hys86r4c7ij4kbmpb"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'build
                    (lambda _
                      (invoke "python" "setup.py" "build")))
                  (replace 'install
                    (lambda _
                      (invoke "python" "setup.py" "install"))))))
    (inputs (list python
                  gfortran-toolchain
                  python-numpy
                  python-scipy
                  python-cython
                  python-networkx
                  python-matplotlib
                  python-pyro4
                  python-sqlalchemy))
    (propagated-inputs (list python-decorator))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/pele-python/")
    (synopsis "Energy landscapes.")
    (description "TBA.")
    (license license:bsd-3)))

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

;; TODO: turn this into a proper bazel build system.
(define* (bazel-vendored-inputs #:key name
                                version
                                source
                                hash
                                search-paths
                                inputs
                                bazel-targets
                                (bazel-arguments '())
                                extra-configuration)
  (let ((name* (string-append name "-vendored-inputs-" version ".tar.xz")))
    (computed-file name*
                   (with-imported-modules (source-module-closure '((guix build
                                                                    utils)
                                                                   (guix build
                                                                    gnu-build-system)))
                                          #~(begin
                                              (use-modules (guix build utils)
                                                           (ice-9 match)
                                                           (ice-9 string-fun))
                                              (define input-directories
                                                '#$(map cadr inputs))
                                              (define %build-directory
                                                (getcwd))
                                              (define %bazel-out
                                                (string-append
                                                 %build-directory "/output"))
                                              (define %bazel-user-root
                                                (string-append
                                                 %build-directory "/tmp"))
                                              (setvbuf (current-output-port)
                                                       'line)
                                              (setvbuf (current-error-port)
                                                       'line)
                                              (set-path-environment-variable
                                               "PATH"
                                               '("bin" "sbin")
                                               (cons* #+openjdk11:jdk
                                                      #$(canonical-package
                                                         which)
                                                      input-directories))
                                              (for-each (match-lambda
                                                          ((env-var (files ...)
                                                                    separator
                                                                    type
                                                                    pattern)
                                                           (set-path-environment-variable
                                                            env-var
                                                            files
                                                            input-directories
                                                            #:separator
                                                            separator
                                                            #:type type
                                                            #:pattern pattern)))
                                                        '#$search-paths)

                                              ;; TODO: only works for directories
                                              (chdir #$source)
                                              (setenv "SOURCE_DATE_EPOCH" "1")
                                              (setenv "HOME" %build-directory)
                                              (setenv "USER"
                                                      "homeless-shelter")
                                              (setenv "GIT_SSL_CAINFO"
                                                      (string-append #+nss-certs
                                                       "/etc/ssl/certs/ca-bundle.crt"))
                                              (setenv "SSL_CERT_FILE"
                                                      (string-append #+nss-certs
                                                       "/etc/ssl/certs/ca-bundle.crt"))

                                              (mkdir-p %bazel-out)
                                              #$extra-configuration
                                              (apply invoke
                                               "bazel"
                                               "--batch"
                                               (string-append "--output_base="
                                                %bazel-out)
                                               (string-append
                                                "--output_user_root="
                                                %bazel-user-root)
                                               "build"
                                               "--nobuild"
                                               "--curses=no"
                                               "--loading_phase_threads=1"
                                               "--strategy=Genrule=standalone"
                                               "--verbose_failures"
                                               "--subcommands"
                                               "--action_env=PATH"
                                               "--action_env=LIBRARY_PATH"
                                               "--action_env=C_INCLUDE_PATH"
                                               "--action_env=CPLUS_INCLUDE_PATH"
                                               "--action_env=GUIX_LOCPATH"
                                               "--host_action_env=PATH"
                                               "--host_action_env=LIBRARY_PATH"
                                               "--host_action_env=C_INCLUDE_PATH"
                                               "--host_action_env=CPLUS_INCLUDE_PATH"
                                               "--host_action_env=GUIX_LOCPATH"

                                               ;; Extra arguments
                                               (append #$bazel-arguments
                                                       (list #$@bazel-targets)))

                                              (with-directory-excursion %bazel-out
                                                (for-each
                                                 delete-file-recursively
                                                 (append (find-files
                                                          "external"
                                                          "^\\.(git|svn|hg)$")))
                                                ;; Erase markers
                                                (for-each (lambda (file)
                                                            (truncate-file
                                                             file 0))
                                                          (find-files
                                                           "external"
                                                           "@.*\\.marker"))
                                                ;; Remove symlink references to the build directory.  These
                                                ;; will be rewritten to the current build directory by
                                                ;; users of this archive.
                                                (for-each (lambda (file)
                                                            (let ((new-target (string-replace-substring
                                                                               (readlink
                                                                                file)
                                                                               %build-directory
                                                                               "GUIX_BUILD_TOP")))
                                                              (delete-file
                                                               file)
                                                              (symlink
                                                               new-target file)))
                                                          (find-files
                                                           "external"
                                                           (lambda (file-name
                                                                    stat)
                                                             (and (eq? (stat:type
                                                                        stat)
                                                                       'symlink)
                                                                  (string-contains
                                                                   (readlink
                                                                    file-name)
                                                                   %build-directory)))
                                                           #:stat lstat))
                                                (invoke "du" "-s" "external")
                                                (invoke "tar"
                                                        "cfa"
                                                        #$output
                                                        "--mtime=@1"
                                                        "--owner=0"
                                                        "--group=0"
                                                        "--numeric-owner"
                                                        "--sort=name"
                                                        "external"))))
                   #:options `(#:hash-algo sha256
                               #:hash ,(nix-base32-string->bytevector hash)))))

(define jaxlib-system-libs
  (list "absl_py"
        ;; "boringssl"
        "com_github_grpc_grpc"
        ;; "com_google_protobuf"
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
        "lmdb"
        "zlib"))

(define-public static-protobuf
  (package
    (inherit protobuf)
    (name "protobuf-static")
    (outputs (list "out"))
    (arguments
     (substitute-keyword-arguments (package-arguments protobuf)
       ((#:configure-flags _ #f)
        #~(list "-DBUILD_SHARED_LIBS=OFF" "-Dprotobuf_USE_EXTERNAL_GTEST=ON"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'move-static-libraries)))))))

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

(define python-jaxlib/wheel
  (package
    (name "python-jaxlib")
    (version "0.4.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/jax")
             (commit (string-append "jaxlib-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pfk7z3kkair6xi92yn0pvs3zlaxajhmk6r2yq020q13mwfxcfxc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 string-fun))
      #:tests? #f ;there are none
      #:phases #~(modify-phases %standard-phases
                   (replace 'configure
                     (lambda* (#:key inputs #:allow-other-keys)
                       (define bazel-out
                         (string-append (getenv "NIX_BUILD_TOP") "/output"))
                       (mkdir-p bazel-out)
                       (with-directory-excursion bazel-out
                         (invoke "tar" "xf"
                                 #$(bazel-vendored-inputs #:source (package-source
                                                                    this-package)
                                    #:name "jaxlib"
                                    #:version version
                                    #:inputs (append (standard-packages)
                                                     (package-inputs
                                                      this-package)
                                                     (package-propagated-inputs
                                                      this-package)
                                                     (package-native-inputs
                                                      this-package))
                                    #:search-paths (map
                                                    search-path-specification->sexp
                                                    (package-transitive-native-search-paths
                                                     this-package))
                                    #:extra-configuration #~(begin
                                                              (setenv
                                                               "TF_SYSTEM_LIBS"
                                                               (string-join '#$jaxlib-system-libs
                                                                ",")))
                                    #:bazel-targets (list
                                                     "//jaxlib/tools:build_wheel"
                                                     "@mkl_dnn_v1//:mkl_dnn")
                                    #:bazel-arguments #~(list "-c" "opt"
                                                         "--config=avx_posix"
                                                         "--config=mkl_open_source_only"
                                                         (string-append
                                                          "--define="
                                                          "PROTOBUF_INCLUDE_PATH="
                                                          #$static-protobuf
                                                          "/include"))
                                    #:hash
                                    "0xxs5vvfhryrpw9j85p1c5gm9rlny2q139wl75wby3zd6m6vrml2")))

                       ;; Rewrite dangling links to current build directory
                       (for-each (lambda (file)
                                   (let ((new-target (string-replace-substring
                                                      (readlink file)
                                                      "GUIX_BUILD_TOP"
                                                      (getenv "NIX_BUILD_TOP"))))
                                     (delete-file file)
                                     (symlink new-target file)))
                                 (find-files bazel-out
                                             (lambda (file-name stat)
                                               (and (eq? (stat:type stat)
                                                         'symlink)
                                                    (string-contains (readlink
                                                                      file-name)
                                                     "GUIX_BUILD_TOP")))
                                             #:stat lstat))
                       (setenv "HOME"
                               (getenv "NIX_BUILD_TOP"))

                       (invoke "python" "build/build.py" "--configure_only")

                       ;; Bazel aborts when a source file includes a header
                       ;; that isn't declared.  It prints something like this:
                       ;;
                       ;; "this rule is missing dependency declarations for
                       ;; the following files included by..."
                       ;;
                       ;; Since we pass through C_INCLUDE_PATH and
                       ;; CPLUS_INCLUDE_PATH there are many headers that are
                       ;; visible to the toolchain, but that Bazel refuses to
                       ;; allow.
                       ;;
                       ;; The biggest problem is that the kernel headers are
                       ;; never declared as dependencies anywhere, so we need
                       ;; to modify the toolchain declaration to allow headers
                       ;; from this location.
                       ;;
                       ;; There are other headers that cause trouble, though,
                       ;; such as those for zlib in the
                       ;; @com_google_protobuf//:protobuf target.  There must
                       ;; be some other mechanism to fix this (e.g. in the
                       ;; protobuf target itself), but I find it easier to just
                       ;; allow all locations that appear on these INCLUDE_PATH
                       ;; variables.
                       (substitute* (string-append bazel-out
                                     "/external/local_config_cc/BUILD")
                         (("cxx_builtin_include_directories = \\[" m)
                          (string-append m
                                         (string-join (map (lambda (dir)
                                                             (string-append
                                                              "\"" dir "\""))
                                                           (append (parse-path
                                                                    (getenv
                                                                     "C_INCLUDE_PATH")
                                                                    '())
                                                                   (parse-path
                                                                    (getenv
                                                                     "CPLUS_INCLUDE_PATH")
                                                                    '()))) ","
                                                      'suffix))))

                       ;; XXX: Our version of protobuf leads to "File already
                       ;; exists in database" when loading jax in Python.
                       ;; Using the static library is what Nix does, but it
                       ;; doesn't help us.
                       (substitute* (string-append bazel-out
                                     "/external/xla/third_party/systemlibs/protobuf.BUILD")
                         (("-lprotobuf")
                          "-l:libprotobuf.a")
                         (("-lprotoc")
                          "-l:libprotoc.a"))))
                   (replace 'build
                     (lambda* (#:key parallel-build? #:allow-other-keys)
                       (define %build-directory
                         (getenv "NIX_BUILD_TOP"))
                       (define %bazel-out
                         (string-append %build-directory "/output"))
                       (define %bazel-user-root
                         (string-append %build-directory "/tmp"))
                       ;; The version is automatically set to ".dev" if this
                       ;; variable is not set.  See
                       ;; https://github.com/google/jax/commit/e01f2617b85c5bdffc5ffb60b3d8d8ca9519a1f3
                       (setenv "JAXLIB_RELEASE" "1")
                       (setenv "BAZEL_USE_CPP_ONLY_TOOLCHAIN" "1")

                       (call-with-output-file ".jax_configure.bazelrc"
                         (lambda (port)
                           ;; build --define PROTOBUF_INCLUDE_PATH=" #$(this-package-input "protobuf") "/include
                           (display (string-append
                                     "
build --strategy=Genrule=local
build --repo_env PYTHON_BIN_PATH="
                                     #$(this-package-input "python-wrapper")
                                     "/bin/python
build --action_env=PYENV_ROOT
build --python_path="
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
                                    port)))
                       (setenv "USER" "homeless-shelter")
                       (setenv "TF_SYSTEM_LIBS"
                               (string-join '#$jaxlib-system-libs ","))
                       (apply invoke
                              "bazel"
                              "--batch"
                              (string-append "--output_base=" %bazel-out)
                              (string-append "--output_user_root="
                                             %bazel-user-root)
                              "run"
                              "--nofetch"
                              "--verbose_explanations"
                              "--curses=no"
                              "--verbose_failures"
                              "--subcommands"
                              "--action_env=PATH"
                              "--action_env=LIBRARY_PATH"
                              "--action_env=C_INCLUDE_PATH"
                              "--action_env=CPLUS_INCLUDE_PATH"
                              "--action_env=GUIX_LOCPATH"
                              "--action_env=TF_SYSTEM_LIBS"
                              "--host_action_env=TF_SYSTEM_LIBS"
                              "--host_action_env=PATH"
                              "--host_action_env=LIBRARY_PATH"
                              "--host_action_env=C_INCLUDE_PATH"
                              "--host_action_env=CPLUS_INCLUDE_PATH"
                              "--host_action_env=GUIX_LOCPATH"
                              "-c"
                              "opt"
                              "--jobs"
                              (if parallel-build?
                                  (number->string (parallel-job-count)) "1")
                              (list "//jaxlib/tools:build_wheel" "--"
                                    (string-append "--output_path="
                                                   #$output)
                                    (string-append "--cpu="
                                                   #$(match (or (%current-target-system)
                                                                (%current-system))
                                                       ("x86_64-linux"
                                                        "x86_64")
                                                       ("i686-linux" "i686")
                                                       ("mips64el-linux"
                                                        "mips64")
                                                       ("aarch64-linux"
                                                        "aarch64")
                                                       ;; Prevent errors when querying
                                                       ;; this package on unsupported
                                                       ;; platforms, e.g. when running
                                                       ;; "guix package --search="
                                                       (_ "UNSUPPORTED")))))))
                   (delete 'install))))
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
    (native-inputs (list python-pypa-build
                         python-setuptools
                         python-wheel
                         bazel-6
                         which
                         `(,openjdk11 "jdk"))) ;for bazel
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

;; Keep in sync with jaxlib above
(define-public python-jax
  (package
    (name "python-jax")
    (version "0.4.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jax" version))
       (sha256
        (base32 "0cl1j8y7664i0rn7ckixk7372wkjm88azya5izlh620hj0wg6v3p"))))
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

(define-public python-grobid-client-python
  (package
    (name "python-grobid-client-python")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grobid_client_python" version))
       (sha256
        (base32 "1qxwkp0brqrfxwm853f48jzraff8934b89nm0h0vb7mpsgv1p0xw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests))
    (home-page "https://github.com/kermitt2/grobid_client_python")
    (synopsis "Simple python client for GROBID REST services")
    (description "Simple python client for GROBID REST services")
    (license license:asl2.0)))

(define-public python-grobid-client-python
  (package
    (name "python-grobid-client-python")
    (version "0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grobid_client_python" version))
       (sha256
        (base32 "1qxwkp0brqrfxwm853f48jzraff8934b89nm0h0vb7mpsgv1p0xw"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-requests))
    (home-page "https://github.com/kermitt2/grobid_client_python")
    (synopsis "Simple python client for GROBID REST services")
    (description "Simple python client for GROBID REST services")
    (license license:asl2.0)))

(define-public python-openalexapi
  (package
    (name "python-openalexapi")
    (version "0.0.1a0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openalexapi" version))
       (sha256
        (base32 "02bwnqr0kbjgjqcaknwnzm92nczxvc03av93ngmkin40cb99bxk0"))))
    (arguments (list #:tests? #f))
    (build-system pyproject-build-system)
    (inputs (list python-requests python-pydantic python-purl))
    (home-page "https://github.com/dpriskorn/OpenAlexAPI")
    (synopsis "Libray for accessing the OpenAlex API")
    (description "Libray for accessing the @code{OpenAlex} API")
    (license license:gpl3+)))

(define-public python-pyalex
  (package
   (name "python-pyalex")
   (version "0.13")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pyalex" version))
     (sha256
      (base32 "0n29hl0m137jyx65lry7mh50lwcnba1j8zpap252rckli0gbfffh"))))
    (arguments
     '(#:tests? #f)) ;; Requires network access
   (build-system pyproject-build-system)
   (propagated-inputs (list python-requests python-urllib3))
   (native-inputs (list python-pytest python-pytest-xdist))
   (home-page "")
   (synopsis "Python interface to the OpenAlex database")
   (description "Python interface to the @code{OpenAlex} database")
   (license license:expat)))

(define-public python-decord
  (package
    (name "python-decord")
    (version "0.6.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/dmlc/decord")
            (commit (string-append "v" version))
            (recursive? #t)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1wh9bg3m1bnxqzqmm1pg3hlfhlvb3xgcxyf0qw8i9j7mv6z4xqqr"))))
    (arguments
     '(#:tests? #f ;; Requires MXNet
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'libdecord
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libdecord (string-append (assoc-ref inputs "decord")
                                             "/lib/libdecord.so")))
               (install-file libdecord "python/decord")
               (chdir "python")))))))
    (build-system python-build-system)
    (inputs (list python-numpy))
    (native-inputs (list decord))
    (home-page "https://github.com/dmlc/decord")
    (synopsis "@code{Decord} is a reverse procedure of @code{Record}. It provides convenient video slicing methods based on a thin wrapper on top of hardware accelerated video decoders, e.g. 1) FFMPEG/LibAV, 2) NVIDEA Codecs, 3) Intel Codecs")
    (description "@code{Decord} was designed to handle awkward video shuffling experience in order to provide smooth experiences similar to random image loader for deep learning.

@code{Decord} is also able to decode audio from both video and audio files. One can slice video and audio together to get a synchronized result; hence providing a one-stop solution for both video and audio decoding.")
    (license license:asl2.0)))

(define-public python-bristol
  (package
   (name "python-bristol")
   (version "0.2.14")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "bristol" version))
     (sha256
      (base32 "1l8k6ndv9hssxkl0f4ylzidm9s59r1h8d3fiaa8dqwjnx7c8pa01"))))
   (build-system pyproject-build-system)
   (propagated-inputs (list python-numpy python-pytorch python-torchvision))
   (home-page "https://github.com/msuzen/bristol")
   (synopsis
    "Parallel random matrix tools and random matrix theory deep learning applications. Generate matrices from Circular Unitary Ensemble (CUE), Circular Ortogonal Ensemble (COE) and Circular Symplectic Ensemble (CSE). Additional spectral analysis utilities are also implemented, such as computation of spectral density and spectral ergodicity for complexity of deep learning architectures.")
   (description
    "Parallel random matrix tools and random matrix theory deep learning
applications.  Generate matrices from Circular Unitary Ensemble (CUE), Circular
Ortogonal Ensemble (COE) and Circular Symplectic Ensemble (CSE).  Additional
spectral analysis utilities are also implemented, such as computation of
spectral density and spectral ergodicity for complexity of deep learning
architectures.")
   (license license:asl2.0)))

(define-public python-transformers
  (package
    (name "python-transformers")
    (version "4.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "transformers" version))
       (sha256
        (base32 "1mbxhmh5kglxc59h1l5xn6nnfmfyl975vh54n940m9dqhbb7mz65"))))
    (build-system pyproject-build-system)
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
    (native-inputs (list python-accelerate
                         python-av
                         python-beautifulsoup4
                         python-codecarbon
                         python-cookiecutter
                         python-datasets
                         python-decord
                         python-deepspeed
                         python-dill
                         python-evaluate
                         python-faiss-cpu
                         python-flax
                         python-fugashi
                         python-gitpython
                         python-hf-doc-builder
                         python-ipadic
                         python-isort
                         python-jax
                         python-jaxlib
                         python-kenlm
                         python-keras-nlp
                         python-librosa
                         python-nltk
                         python-onnxconverter-common
                         python-onnxruntime
                         python-onnxruntime-tools
                         python-optax
                         python-optuna
                         python-parameterized
                         python-phonemizer
                         python-pillow
                         python-protobuf
                         python-psutil
                         python-pyctcdecode
                         python-pydantic
                         python-pytest
                         python-pytest-timeout
                         python-pytest-xdist
                         python-ray
                         python-rhoknp
                         python-rjieba
                         python-rouge-score
                         python-ruff
                         python-sacrebleu
                         python-sacremoses
                         python-scikit-learn
                         python-sentencepiece
                         python-sigopt
                         python-sudachidict-core
                         python-sudachipy
                         python-tensorboard
                         python-tensorflow
                         python-tensorflow-text
                         python-tf2onnx
                         python-timeout-decorator
                         python-timm
                         python-tokenizers
                         python-torch
                         python-torchaudio
                         python-torchvision
                         python-unidic
                         python-unidic-lite
                         python-urllib3))
    (home-page "https://github.com/huggingface/transformers")
    (synopsis
     "State-of-the-art Machine Learning for JAX, PyTorch and TensorFlow")
    (description
     "State-of-the-art Machine Learning for JAX, @code{PyTorch} and @code{TensorFlow}")
   (license license:asl2.0)))