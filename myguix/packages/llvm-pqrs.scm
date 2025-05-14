(define-module (myguix packages llvm-pqrs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages version-control)
  #:use-module (myguix packages nvidia)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public llvm-for-triton
  (package
    (inherit llvm-20)
    (name "llvm")
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
              (prepend zstd)))))
