(define-module (myguix packages check)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages check))

(define-public googletest-e2239e
  (let ((version "1.11")
        (revision "0")
        (commit "e2239ee6043f73722e7aa812a459f54a28552929"))
  (package
		(inherit googletest)
    (name "googletest")
		(version (string-append version "." revision))
    (home-page "https://github.com/google/googletest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pd4y1gpx1z8fiyarkvqlmk6hbv0lc8fr00ivnsvqzi1xg34jfaa")))))))

