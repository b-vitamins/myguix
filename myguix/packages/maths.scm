(define-module (myguix packages maths)
  #:use-module (myguix packages)
  #:use-module (myguix packages check)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (guix build-system cmake))

(define-public fp16-4dfe081
  (let ((commit "4dfe081cf6bcd15db339cf2680b9281b8451eeb3")
        (version "0.0")
        (revision "2"))
    (package
      (name "fp16")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/FP16")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06a8dfl3a29r93nxpp6hpywsajz5d555n3sqd3i6krybb6swnvh7"))
         (patches (search-patches "fp16-implicit-double.patch"
                                  "fp16-system-libraries-rev2.patch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:imported-modules ((guix build python-build-system)
                             ,@%cmake-build-system-modules)
         #:modules (((guix build python-build-system)
                     #:select (site-packages))
                    (guix build cmake-build-system)
                    (guix build utils))
         #:phases (modify-phases %standard-phases
                    (add-after 'install 'move-python-files
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (include (string-append out "/include"))
                               (site (site-packages inputs outputs))
                               (target (string-append site "/fp16")))
                          (mkdir-p target)
                          (for-each (lambda (file)
                                      (rename-file file
                                                   (string-append target "/"
                                                                  (basename
                                                                   file))))
                                    (find-files include "\\.py$"))))))))
      (native-inputs (list python-wrapper))
      (inputs (list psimd googletest-e2239e googlebenchmark))
      (synopsis "C++ library for half-precision floating point formats")
      (description
       "This header-only C++ library implements conversion to and from
half-precision floating point formats.")
      (license license:expat))))

(define-public fxdiv-b408327
  (let ((commit "b408327ac2a15ec3e43352421954f5b1967701d1")
        (version "0.0")
        (revision "2"))
    (package
      (inherit fxdiv)
      (name "fxdiv")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/FXdiv")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nsrwghyy2rn9la9q6vwba19nzsg8cg6l9ihqi8mhqd3qxrfqj04"))
         (patches (search-patches "fxdiv-system-libraries-rev2.patch"))))
      (inputs (list googletest-e2239e googlebenchmark)))))
