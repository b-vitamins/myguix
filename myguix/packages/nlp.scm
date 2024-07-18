(define-module (myguix packages nlp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (myguix packages cuda)
  #:use-module (nonguix packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages base)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python))

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
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON" "-DWHISPER_CUDA=ON"
                           "-DWHISPER_OPENMP=ON")
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
    (inputs (list python nvidia-driver cuda-toolkit-12.1 libomp coreutils))
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (synopsis "Port of OpenAI's Whisper model in C/C++")
    (description
     "This package provides a port to OpenAI's Whisper Automatic Speech Recognition Models. It requires model parameters to be downloaded independently and to be able to run a Whisper model.")
    (license license:expat)))
