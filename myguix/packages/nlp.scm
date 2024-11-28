(define-module (myguix packages nlp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs))

(define-public whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/whisper.cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fbxf43dfz0wgc2qx57gm7a56nqpbmvkzgk68jfj1pa5r9qijzfb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;No tests.
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON" "-DGGML_BLAS=ON"
                           "-DGGML_BLAS_VENDOR=OpenBLAS" "-DGGML_OPENMP=ON")
       #:modules ((guix build utils)
                  (guix build cmake-build-system)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-binaries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (build-dir (string-append (getcwd) "/bin/")))
                        (for-each (lambda (file)
                                    (let ((orig-file (string-append build-dir
                                                                    file))
                                          (new-file (if (string=? file "main")
                                                        (string-append
                                                         build-dir "/whisper")
                                                        (string-append
                                                         build-dir "/whisper-"
                                                         file))))
                                      (invoke "mv" orig-file new-file)
                                      (install-file new-file bin)))
                                  '("main" "bench" "server" "quantize")))))
                  (add-after 'install-binaries 'fix-rpath
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((libdir (string-append (assoc-ref outputs "out")
                                                   "/lib"))
                            (bindir (string-append (assoc-ref outputs "out")
                                                   "/bin")))
                        (for-each (lambda (file)
                                    (let* ((pipe (open-pipe* OPEN_READ
                                                             "patchelf"
                                                             "--print-rpath"
                                                             file))
                                           (line (read-line pipe)))
                                      (and (zero? (close-pipe pipe))
                                           (invoke "patchelf" "--set-rpath"
                                                   (string-append libdir ":"
                                                                  line) file))))
                                  (find-files bindir))))))))
    (inputs (list openblas))
    (native-inputs (list git-minimal patchelf-0.16 python pkg-config))
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (synopsis "Port of OpenAI's Whisper model in C/C++")
    (description
     "This package provides a port to OpenAI's Whisper Automatic Speech Recognition Models. It requires model parameters to be downloaded independently and to be able to run a Whisper model.")
    (license license:expat)))

(define-public whisper-cpp-cuda
  (package
    (inherit whisper-cpp)
    (name "whisper-cpp-cuda")
    (inputs (list cuda-toolkit-12.4 openblas))
    (propagated-inputs (list nvidia-driver))
    (arguments
     (substitute-keyword-arguments (package-arguments whisper-cpp)
       ((#:configure-flags flags
         ''())
        #~(append '("-DGGML_CUDA=ON" "-DCMAKE_CUDA_ARCHITECTURES=75;80;86"
                    "-DGGML_CUDA_FA_ALL_QUANTS=true")
                  #$flags))))))

(define-public llama-cpp-cuda
  (package
    (inherit llama-cpp)
    (name "llama-cpp-cuda")
    (inputs (list python))
    (propagated-inputs (list nvidia-driver
                             cuda-toolkit-12.4
                             python-numpy
                             python-pytorch
                             python-sentencepiece
                             openblas))
    (native-inputs (list git-minimal pkg-config))
    (arguments
     (substitute-keyword-arguments (package-arguments llama-cpp)
       ((#:configure-flags flags
         ''())
        #~(list "-DBUILD_SHARED_LIBS=ON"
                "-DGGML_NATIVE=OFF"
                "-DGGML_BLAS=ON"
                "-DGGML_BLAS_VENDOR=OpenBLAS"
                "-DGGML_OPENMP=ON"
                "-DGGML_CUDA=ON"
                "-DCMAKE_CUDA_ARCHITECTURES=75;80;86"
                "-DGGML_CUDA_FA_ALL_QUANTS=true"))))))

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
