(define-module (myguix packages pretty-print)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression))

(define-public fmt-b50e68
  (let ((version "11.0.1")
        (revision "0")
        (commit "b50e685db996c167e6c831dcef582aba6e14276a")
        (url "https://github.com/fmtlib/fmt"))
    (package
      (name "fmt")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url url)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yzw3xf015h4mfz2iqmj38vj530f96xcs05f5wmjpgs2kinrvy0h"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
      (native-inputs (list unzip))
      (home-page "https://fmt.dev")
      (synopsis "Small and fast C++ formatting library")
      (description
       "@code{fmt} (formerly @code{cppformat}) is a formatting
library for C++.  It can be used as a safe alternative to @code{printf} or as
a fast alternative to @code{IOStreams}.")
      (license (list license:bsd-2 license:bsd-3 license:psfl)))))
