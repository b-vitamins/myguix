(define-module (myguix packages nvidia-containers)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (myguix packages nvidia)
  #:use-module ((myguix packages rust-crates-pqrs)
                #:select (lookup-myguix-cargo-inputs)))

(define-public pathrs
  (package
    (name "pathrs")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cyphar/libpathrs/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j8qj1dgl460ir9g73ncf4ka72ckmix744ph84bhnv3wxmwz4yq1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f                  ;Upstream tests require privileged access.
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-workspace-members
            (lambda _
              (invoke "sed" "-i" "/^\\[workspace\\]/,$d" "Cargo.toml")))
          (add-after 'drop-workspace-members 'disable-broken-symbol-version-script
            (lambda _
              (substitute* "build.rs"
                ((".*cargo:rustc-cdylib-link-arg=-Wl,--version-script=.*")
                 ""))))
          (replace 'build
            (lambda _
              (invoke "cargo" "rustc" "--offline" "--release"
                      "--crate-type=cdylib"
                      "--features=capi"
                      "--" "-C" "panic=abort")
              (invoke "cargo" "rustc" "--offline" "--release"
                      "--crate-type=staticlib"
                      "--features=capi"
                      "--" "-C" "panic=abort")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "bash" "./install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--exec-prefix=" out)
                        (string-append "--includedir=" out "/include")
                        (string-append "--libdir=" out "/lib")
                        (string-append "--pkgconfigdir=" out "/lib/pkgconfig"))))))))
    (native-inputs (list util-linux))
    (inputs (lookup-myguix-cargo-inputs 'pathrs))
    (home-page "https://github.com/cyphar/libpathrs")
    (synopsis "Safe path resolution library for Linux")
    (description
     "pathrs provides a Rust library and C-compatible API for performing
safer path resolution operations on Linux.")
    (license (list license:mpl2.0 license:lgpl3+))))

(define-public libnvidia-container
  (package
    (name "libnvidia-container")
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/NVIDIA/libnvidia-container"
                           "/archive/refs/tags/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "060p6kvq46pjbsdsihkz6hff5d4z9dngz3s4s6f7w17mpk9hmd11"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                  ;Upstream does not ship a standalone test suite.
      #:make-flags
      #~(let ((out #$output))
          (list (string-append "LIB_VERSION=" #$version)
                (string-append "REVISION=v" #$version)
                "WITH_LIBELF=yes"
                "WITH_TIRPC=yes"
                "LDCONFIG=true"
                (string-append "prefix=" out)
                (string-append "exec_prefix=" out)
                (string-append "bindir=" out "/bin")
                (string-append "libdir=" out "/lib")
                (string-append "docdir=" out "/share/doc")
                (string-append "includedir=" out "/include")
                (string-append "pkgconfdir=" out "/lib/pkgconfig")
                (string-append "libdbgdir=" out "/lib/debug")
                "CC=gcc"
                (string-append
                 "CPPFLAGS=-D_GNU_SOURCE -D_FORTIFY_SOURCE=2 -isystem "
                 (assoc-ref %build-inputs "nvidia-modprobe") "/include"
                 " -isystem "
                 (assoc-ref %build-inputs "libtirpc") "/include/tirpc")
                (string-append
                 "GO=" (search-input-file %build-inputs "/bin/go"))))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-guix-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (glibc (assoc-ref inputs "glibc"))
                    (nvidia-modprobe (assoc-ref inputs "nvidia-modprobe")))
                (setenv "HOME" (getcwd))
                (mkdir-p ".cache/go-build")
                (setenv "GOCACHE" (string-append (getcwd) "/.cache/go-build"))
                (setenv "GOFLAGS" "-mod=vendor")
                (substitute* "mk/common.mk"
                  (("which \\$\\(CC\\)")
                   "command -v $(CC)"))
                (substitute* "mk/nvcgo.mk"
                  (("\\$\\(RM\\) -rf \\$\\(SRCS_DIR\\)\n")
                   "$(RM) -rf $(SRCS_DIR)\n\t$(MKDIR) -p $(dir $(SRCS_DIR))\n"))
                (substitute* "Makefile"
                  (("all: shared static tools")
                   "all: shared tools")
                  (("debug: shared static tools")
                   "debug: shared tools")
                  (("LIB_LDFLAGS        = -L\\$\\(DEPS_DIR\\)\\$\\(libdir\\) -shared -Wl,-soname=\\$\\(LIB_SONAME\\)")
                   "LIB_LDFLAGS        = -shared -Wl,-soname=$(LIB_SONAME)")
                  (("BIN_LDFLAGS        = -L\\. -pie \\$\\(LDFLAGS\\) -Wl,-rpath='\\$\\$ORIGIN/../\\$\\$LIB'")
                   "BIN_LDFLAGS        = -L. -pie $(LDFLAGS) -Wl,-rpath=$(libdir)")
                  (("LIB_LDLIBS_STATIC  = -l:libnvidia-modprobe-utils\\.a")
                   (string-append
                    "LIB_LDLIBS_STATIC  = "
                    nvidia-modprobe "/lib/libnvidia-modprobe-utils.a"))
                  (("LIB_LDLIBS_STATIC  \\+= -l:libtirpc\\.a")
                   "LIB_LDLIBS_STATIC  += -ltirpc")
                  (("\\$\\(MAKE\\) -f \\$\\(MAKE_DIR\\)/libtirpc\\.mk DESTDIR=\\$\\(DEPS_DIR\\) install")
                   "true")
                  (("\t\\$\\(MAKE\\) -f \\$\\(MAKE_DIR\\)/nvidia-modprobe\\.mk DESTDIR=\\$\\(DEPS_DIR\\) install\n")
                   "")
                  (("ifeq \\(\\$\\(WITH_LIBELF\\), no\\)\n\t\\$\\(MAKE\\) -f \\$\\(MAKE_DIR\\)/elftoolchain\\.mk DESTDIR=\\$\\(DEPS_DIR\\) install\nendif\n")
                   "")
                  (("\t\\$\\(INSTALL\\) -m 644 \\$\\(LIB_STATIC\\) \\$\\(DESTDIR\\)\\$\\(libdir\\)\n")
                   "")
                  (("ifeq \\(\\$\\(WITH_TIRPC\\), yes\\)\n\t\\$\\(MAKE\\) -f \\$\\(MAKE_DIR\\)/libtirpc\\.mk DESTDIR=\\$\\(DEPS_DIR\\) install\nendif\n")
                   ""))
                (substitute* "src/common.h"
                  (("#define LIB_DIR\\s+\"/lib64\"")
                   (string-append
                    "#define LIB_DIR                   \""
                    glibc "/lib\"")))
                (substitute* "src/nvc_info.c"
                  (("#include \"xfuncs.h\"\n")
                   (string-append
                    "#include \"xfuncs.h\"\n\n"
                    "int nvidia_cap_get_device_file_attrs(const char *, int *, int *, char *);\n")))
                (substitute* "src/nvc_internal.h"
                  (("#define SONAME_LIBNVCGO \"libnvidia-container-go.so.1\"")
                   (string-append
                    "#define SONAME_LIBNVCGO \""
                    out "/lib/libnvidia-container-go.so.1\""))))))
          (add-after 'install 'fix-installed-soname-links
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((libdir (string-append (assoc-ref outputs "out") "/lib")))
                (with-directory-excursion libdir
                  (symlink (string-append "libnvidia-container.so." #$version)
                           "libnvidia-container.so.1")
                  (delete-file "libnvidia-container.so")
                  (symlink "libnvidia-container.so.1"
                           "libnvidia-container.so")
                  (symlink (string-append
                            "libnvidia-container-go.so." #$version)
                           "libnvidia-container-go.so.1")
                  (delete-file "libnvidia-container-go.so")
                  (symlink "libnvidia-container-go.so.1"
                           "libnvidia-container-go.so")))))
          (add-after 'fix-installed-soname-links 'remove-debug-artifacts
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((debugdir (string-append
                               (assoc-ref outputs "out") "/lib/debug")))
                (when (file-exists? debugdir)
                  (delete-file-recursively debugdir))))))))
    (native-inputs
     (list go
           pkg-config
           rpcsvc-proto))
    (inputs
     (list elfutils
           glibc
           libcap
           libseccomp
           libtirpc
           nvidia-modprobe))
    (home-page "https://github.com/NVIDIA/libnvidia-container")
    (synopsis "NVIDIA container runtime library and CLI")
    (description
     "libnvidia-container provides the core library and command line tools
used to configure Linux containers for access to NVIDIA GPUs.")
    (license (list license:asl2.0
                   license:bsd-3))))

(define nvidia-container-toolkit-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/NVIDIA/nvidia-container-toolkit"
                        "/archive/refs/tags/v1.19.0.tar.gz"))
    (file-name "nvidia-container-toolkit-1.19.0.tar.gz")
    (sha256
     (base32 "0bp4bxv2ajx6m80iywc9gszn4jw67qmsgz7813rgyrqicxkqapwc"))))

(define-public nvidia-container-toolkit-base
  (package
    (name "nvidia-container-toolkit-base")
    (version "1.19.0")
    (source nvidia-container-toolkit-source)
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.25
      #:import-path "github.com/NVIDIA/nvidia-container-toolkit"
      #:install-source? #f
      #:tests? #f                  ;Integration tests require a live container runtime.
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key import-path outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (ldflags
                     (string-append
                      "-s -w "
                      "-X github.com/NVIDIA/nvidia-container-toolkit/internal/info.version="
                      #$version)))
                (setenv "CGO_ENABLED" "1")
                (mkdir-p (string-append out "/bin"))
                (with-directory-excursion (string-append "src/" import-path)
                  (for-each
                   (lambda (command)
                     (invoke "go" "build"
                             "-trimpath"
                             "-ldflags" ldflags
                             "-o" (string-append out "/bin/" command)
                             (string-append "./cmd/" command)))
                   '("nvidia-container-runtime"
                     "nvidia-ctk"
                     "nvidia-cdi-hook"))))))
          (add-after 'install 'wrap-executables
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bindir (string-append (assoc-ref outputs "out") "/bin")))
                (wrap-program (string-append bindir "/nvidia-container-runtime")
                  `("PATH" ":" prefix (,bindir)))
                (wrap-program (string-append bindir "/nvidia-ctk")
                  `("PATH" ":" prefix (,bindir)))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pathrs))
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (synopsis "NVIDIA container runtime and CLI tools")
    (description
     "nvidia-container-toolkit-base provides the NVIDIA container runtime,
the @command{nvidia-ctk} management CLI, and the @command{nvidia-cdi-hook}
helper used to enable GPU support in containers.")
    (license license:asl2.0)))
