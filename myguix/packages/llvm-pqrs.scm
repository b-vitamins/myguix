(define-module (myguix packages llvm-pqrs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (myguix packages nvidia)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public llvm-for-triton
  (package
    (inherit llvm-20)
    (name "llvm-for-triton")
    (version "a66376b0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project")
             (commit "a66376b0dc3b2ea8a84fda26faca287980986f78")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ib206nzixnhkm239k4nix1lx3314h7w92a7y5vipbsy6jihy5gg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-20)
       ((#:configure-flags flags)
        #~(list
           ;; Triton's required LLVM configuration
           "-DLLVM_ENABLE_PROJECTS=clang;lld;mlir"
           "-DLLVM_TARGETS_TO_BUILD=X86;NVPTX;AMDGPU"
           "-DLLVM_ENABLE_RTTI=ON"
           "-DLLVM_ENABLE_ASSERTIONS=ON"
           ;; Keep dynamic linking as Triton expects it
           "-DLLVM_BUILD_LLVM_DYLIB=ON"
           "-DLLVM_LINK_LLVM_DYLIB=ON"
           "-DBUILD_SHARED_LIBS=OFF" ;But not all libs as shared
           "-DLLVM_ENABLE_FFI=ON"
           "-DLLVM_INSTALL_UTILS=ON"
           "-DLLVM_PARALLEL_LINK_JOBS=1"))))
    (native-inputs (modify-inputs (package-native-inputs llvm-20)
                     (prepend python-wrapper perl cmake-minimal ninja)))
    (inputs (modify-inputs (package-inputs llvm-20)
              (prepend libffi zlib)))))
