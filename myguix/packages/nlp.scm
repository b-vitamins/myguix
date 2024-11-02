(define-module (myguix packages nlp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages cuda)
  #:use-module (myguix packages nvidia))

(define-public whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/whisper.cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01q4j602wkvsf9vw0nsazzgvjppf4fhpy90vqnm9affynyxhi0c4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #t
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:validate-runpath? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'configure 'fix-runpath
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (setenv "LDFLAGS"
                              (string-append "-Wl,-rpath="
                                             (assoc-ref outputs "out") "/lib"))))
                  (add-after 'install 'install-main
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (build-dir (string-append (getcwd) "/bin/")))
                        (for-each (lambda (file)
                                    (let ((orig-file (string-append build-dir
                                                                    file))
                                          (new-file (string-append build-dir
                                                                   (string-append
                                                                    "whisper-"
                                                                    file)))
                                          (dest-file (string-append bin
                                                      "/whisper-" file)))
                                      (invoke "mv" orig-file new-file)
                                      (install-file new-file dest-file)))
                                  '("main" "bench" "server" "quantize"))))))))
    (outputs '("out"))
    (inputs (list python))
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (synopsis "Port of OpenAI's Whisper model in C/C++")
    (description
     "This package provides a port to OpenAI's Whisper Automatic Speech Recognition Models. It requires model parameters to be downloaded independently and to be able to run a Whisper model.")
    (license license:expat)))

(define-public python-morfessor
  (package
    (name "python-morfessor")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Morfessor" version))
       (sha256
        (base32 "1cmsxyd7ymlqlgam9a6va0x3fqhz0w1mixj0yv2j85rl6k1flfxv"))))
    (build-system python-build-system)
    (home-page "http://morpho.aalto.fi")
    (synopsis "Morfessor")
    (description "Morfessor")
    (license license:bsd-3)))

(define-public python-nmslib
  (let ((pybind11-for-nmslib (package
                               (inherit pybind11)
                               (name "pybind11")
                               (version "2.6.1")
                               (source
                                (origin
                                  (method git-fetch)
                                  (uri (git-reference
                                        (url
                                         "https://github.com/pybind/pybind11")
                                        (commit (string-append "v" version))))
                                  (sha256
                                   (base32
                                    "1wh5b1xnywzxwxkyac2wvyqwzmy1qxs341jjk820r7b825wn6yad"))
                                  (file-name (git-file-name name version))))
                               (arguments
                                (substitute-keyword-arguments (package-arguments
                                                               pybind11)
                                  ((#:phases phases
                                    #~%standard-phases)
                                   #~(modify-phases #$phases
                                       (add-after 'unpack 'skip-failing-test
                                         (lambda _
                                           (substitute* "tests/test_exceptions.py"
                                             ;; This test fails with Python 3.10; skip it.
                                             (("^def test_python_alreadyset_in_destructor(.*)"
                                               _ rest)
                                              (string-append
                                               "def test_python_alreadyset_in_destructor"
                                               rest "\n" "    return\n"))))))))))))
    (package
      (name "python-nmslib")
      (version "2.1.1")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "nmslib" version))
         (sha256
          (base32 "084wl5kl2grr2yi3bibc6i2ak5s7vanwi21wssbwd4bgfskr84lp"))))
      (build-system python-build-system)
      (propagated-inputs (list python-numpy python-psutil pybind11-for-nmslib))
      (home-page "https://github.com/nmslib/nmslib")
      (synopsis "Non-Metric Space Library (NMSLIB)")
      (description "Non-Metric Space Library (NMSLIB)")
      (license license:asl2.0))))
