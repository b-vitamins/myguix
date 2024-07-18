(define-module (myguix packages parallel)
  #:use-module (myguix packages maths)
  #:use-module (myguix packages check)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages check))

(define-public cpuinfo-3c8b15
  (let ((version "0.0")
        (revision "4")
        (commit "3c8b1533ac03dd6531ab6e7b9245d488f13a82a5"))
    (package
      (inherit cpuinfo)
      (name "cpuinfo")
      (version (git-version version revision commit))
      (home-page "https://github.com/pytorch/cpuinfo")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1c3hqrncha4q956x43yqpy7w3szckwjxbh2gbhs9kfx2c4g6ij3s"))))
      (inputs (list googletest-e2239e googlebenchmark)))))

(define-public pthreadpool-4fe0e1
  (let ((commit "4fe0e1e183925bf8cfa6aae24237e724a96479b8")
        (version "0.1")
        (revision "3"))
    (package
      (inherit pthreadpool)
      (name "pthreadpool")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/pthreadpool")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qcz27w1wgzsag5shwkh686rf6jpilwplgqb0fjb84446lvjd1j7"))
         (patches (search-patches "pthreadpool-system-libraries-rev3.patch"))))
      (inputs (list googletest-e2239e googlebenchmark fxdiv-b408327)))))
