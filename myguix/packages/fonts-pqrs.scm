(define-module (myguix packages fonts-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system font))

(define-public font-et-bembo
  (let ((revision "1")
        (commit "b1824ac5bee3f54ef1ce88c9d6c7850f6c869818"))
    (package
      (name "font-et-bembo")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DavidBarts/ET_Bembo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0207anlcvma00qq1v5ddsh2whyabrydr6x38z8qlqxmrrfci8vgl"))))
      (build-system font-build-system)
      (home-page "https://github.com/edwardtufte/et-book")
      (synopsis "ET Book typeface")
      (description
       "A webfont version of the typeface used in Edward Tufte’s books. ET Book was designed by Dmitry Krasny, Bonnie Scranton, and Edward Tufte. It was converted from the original suit files by Adam Schwartz.")
      (license license:expat))))
