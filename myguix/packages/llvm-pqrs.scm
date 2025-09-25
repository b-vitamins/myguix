(define-module (myguix packages llvm-pqrs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages version-control)
  #:use-module (myguix packages nvidia)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public llvm-for-triton
  (let ((commit "8957e64a20fc7f4277565c6cfe3e555c119783ce")
        (revision "1"))
    (package
      (inherit llvm-20)
      (name "llvm-for-triton")
      (version (git-version "20.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/llvm/llvm-project")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1540p24cdjg115frhn3yq375r5pfxjd87h8k8kvgwrn6y8f70dwn"))))
      (arguments
       (substitute-keyword-arguments (package-arguments llvm-20)
         ((#:configure-flags flags)
          #~(append #$flags
                    ;; ───────── extra sub-projects ─────────
                    (list "-DLLVM_ENABLE_PROJECTS=clang;lld;mlir"
                          "-DLLVM_TARGETS_TO_BUILD=X86;NVPTX;AMDGPU"
                          "-DLLVM_ENABLE_RTTI=ON")))))
      (native-inputs (modify-inputs (package-native-inputs llvm-20)
                       (prepend isl z3 zstd git-minimal cuda-toolkit)))
      (inputs (modify-inputs (package-inputs llvm-20)
                (prepend zstd))))))
