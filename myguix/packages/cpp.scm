(define-module (myguix packages cpp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages cpp))

(define-public cpp-httplib-3b6597
  (let ((version "1.6.1")
        (revision "0")
        (commit "3b6597bba913d51161383657829b7e644e59c006"))
    (package
      (inherit cpp-httplib)
      (name "cpp-httplib")
      (home-page "https://github.com/yhirose/cpp-httplib")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qfr5xp901kx7089cjk4bpbgj8s37gnjqx957d86i63m85r4vpvm")))))))
