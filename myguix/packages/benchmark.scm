(define-module (myguix packages benchmark)
  #:use-module (myguix packages check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages benchmark))

(define-public benchmark-0d98db
  (let ((version "1.6.1")
        (revision "0")
        (commit "0d98dba29d66e93259db7daa53a9327df767a415"))
		(package
			(inherit benchmark)
    (name "benchmark")
    (home-page "https://github.com/google/benchmark")
		(source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rj5ybcf37gjw848xpm02xq2mnkzrjj9lalq9mk6059l5z38aj69"))))
    (native-inputs `(("googletest-source" ,(package-source googletest-e2239e))
                     ("googletest" ,googletest-e2239e))))))
