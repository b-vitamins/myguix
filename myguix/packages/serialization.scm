(define-module (myguix packages serialization)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages serialization))

(define-public flatbuffers-01834d
  (let ((version "23.3.3")
        (revision "0")
        (commit "01834de25e4bf3975a9a00e816292b1ad0fe184b"))
    (package
      (inherit flatbuffers)
      (name "flatbuffers")
      (version version)
      (home-page "https://github.com/google/flatbuffers")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0swl6hzk8rasn2bzbzhppvfiknnhqcp0lm58r9akaw7m6zp4ajc7")))))))
