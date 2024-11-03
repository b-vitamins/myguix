(define-module (myguix packages llvm-pqrs)
  #:use-module (gnu packages llvm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public llvm-with-bolt
  llvm-with-bolt-18)

(define-public llvm-with-bolt-17
  (package
    (inherit llvm-17)
    (name "llvm-with-bolt")
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-17)
       ((#:configure-flags flags)
        #~(append #$flags
                  ;; Add BOLT support
                  (list "-DLLVM_ENABLE_ASSERTIONS=ON"
                        "-DLLVM_ENABLE_PROJECTS=bolt")))))))

(define-public llvm-with-bolt-18
  (package
    (inherit llvm-18)
    (name "llvm-with-bolt")
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-18)
       ((#:configure-flags flags)
        #~(append #$flags
                  ;; Add BOLT support
                  (list "-DLLVM_ENABLE_ASSERTIONS=ON"
                        "-DLLVM_ENABLE_PROJECTS=bolt")))))))
