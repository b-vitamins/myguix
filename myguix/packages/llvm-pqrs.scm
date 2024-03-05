(define-module (myguix packages llvm-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python))

(define %llvm-release-monitoring-url
  "https://github.com/llvm/llvm-project/releases")

(define %llvm-monorepo-hashes
  '(("14.0.6" . "14f8nlvnmdkp9a9a79wv67jbmafvabczhah8rwnqrgd5g3hfxxxx")
    ("15.0.7" . "12sggw15sxq1krh1mfk3c1f07h895jlxbcifpwk3pznh4m1rjfy2")
    ("16.0.6" . "0jxmapg7shwkl88m4mqgfjv4ziqdmnppxhjz6vz51ycp2x4nmjky")
    ("17.0.5" . "149flpr96vcn7a1ckya6mm93m9yp85l47w156fjd0r99ydxrw5kv")))

(define %llvm-patches
  '(("14.0.6" "clang-14.0-libc-search-path.patch")
    ("15.0.7" "clang-15.0-libc-search-path.patch")
    ("16.0.6" "clang-16.0-libc-search-path.patch")
    ("17.0.5" "clang-17.0-libc-search-path.patch")))

(define (llvm-monorepo version)
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/llvm/llvm-project")
                        (commit (string-append "llvmorg-" version))))
    (file-name (git-file-name "llvm-project" version))
    (sha256 (base32 (assoc-ref %llvm-monorepo-hashes version)))
    (patches (map search-patch
                  (assoc-ref %llvm-patches version)))))

(define-public llvm-with-bolt-17
  (package
    (name "llvm-with-bolt")
    (version "17.0.5")
    (source
     (llvm-monorepo version))
    (build-system cmake-build-system)
    (outputs '("out" "opt-viewer"))
    (arguments
     (list
      #:configure-flags #~(list
                           ;; These options are required for cross-compiling LLVM according
                           ;; to <https://llvm.org/docs/HowToCrossCompileLLVM.html>.
                           #$@(if (%current-target-system)
                                  #~((string-append "-DLLVM_TABLEGEN="
                                                    #+(file-append
                                                       this-package
                                                       "/bin/llvm-tblgen"))
                                     #$(string-append
                                        "-DLLVM_DEFAULT_TARGET_TRIPLE="
                                        (%current-target-system))
                                     #$(string-append "-DLLVM_TARGET_ARCH="
                                                      (system->llvm-target-arch))
                                     #$(string-append
                                        "-DLLVM_TARGETS_TO_BUILD="
                                        (system->llvm-target)))
                                  '())
                           ;; Note: sadly, the build system refuses the use of
                           ;; -DBUILD_SHARED_LIBS=ON and the large static archives are needed to
                           ;; build clang-runtime, so we cannot delete them.
                           "-DLLVM_BUILD_LLVM_DYLIB=ON"
                           "-DLLVM_LINK_LLVM_DYLIB=ON"
                           "-DLLVM_ENABLE_FFI=ON"
                           "-DLLVM_ENABLE_ASSERTIONS=ON"
                           "-DLLVM_ENABLE_PROJECTS='bolt'"
                           "-DLLVM_ENABLE_RTTI=ON" ;for some third-party utilities
                           "-DLLVM_INSTALL_UTILS=ON" ;needed for rustc
                           "-DLLVM_PARALLEL_LINK_JOBS=1") ;cater to smaller build machines
      ;; Don't use '-g' during the build, to save space.
      #:build-type "Release"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'change-directory
                     (lambda _
                       (chdir "llvm")))
                   (add-after 'install 'install-opt-viewer
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((opt-viewer-share (string-append #$output:opt-viewer
                                                               "/share")))
                         (mkdir-p opt-viewer-share)
                         (rename-file (string-append #$output
                                                     "/share/opt-viewer")
                                      opt-viewer-share)))))))
    (native-inputs (list python-wrapper perl))
    (inputs (list libffi))
    (propagated-inputs (list zlib)) ;to use output from llvm-config
    (home-page "https://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license license:asl2.0)
    (properties `((release-monitoring-url unquote %llvm-release-monitoring-url)))))
