(define-module (myguix packages parallel)
  #:use-module (gnu packages parallel)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public cpuinfo-3c8b15
  (let ((version "0.0")
        (revision "4")
        (commit "3c8b1533ac03dd6531ab6e7b9245d488f13a82a5"))
    (package
			(inherit cpuinfo)
      (name "cpuinfo")
      (version (git-version version revision commit))
      (home-page "https://github.com/pytorch/cpuinfo")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c3hqrncha4q956x43yqpy7w3szckwjxbh2gbhs9kfx2c4g6ij3s")))))))
