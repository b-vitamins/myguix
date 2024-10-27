(define-module (myguix packages video)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (myguix packages cuda)
  #:use-module (myguix packages nvidia)
  #:use-module (ice-9 match))

(define-public decord
  (package
    (name "decord")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/decord")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wh9bg3m1bnxqzqmm1pg3hlfhlvb3xgcxyf0qw8i9j7mv6z4xqqr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DUSE_CUDA=0" "-DCMAKE_BUILD_TYPE=Release")))
    (native-inputs (list ffmpeg-4 python pkg-config))
    (home-page "https://github.com/dmlc/decord")
    (synopsis
     "@code{Decord} is a reverse procedure of @code{Record}. It provides convenient video slicing methods based on a thin wrapper on top of hardware accelerated video decoders, e.g. 1) FFMPEG/LibAV, 2) NVIDEA Codecs, 3) Intel Codecs")
    (description
     "@code{Decord} was designed to handle awkward video shuffling experience in order to provide smooth experiences similar to random image loader for deep learning.

@code{Decord} is also able to decode audio from both video and audio files. One can slice video and audio together to get a synchronized result; hence providing a one-stop solution for both video and audio decoding.")
    (license license:asl2.0)))

(define-public nv-codec-headers
  (package
    (name "nv-codec-headers")
    (version "12.2.72.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FFmpeg/nv-codec-headers")
             (commit (string-append "n" version))))
       (sha256
        (base32 "1dk13wjg56ddb9g0653fwx3n0h64xs7n8m5ys696adrhhgx77pym"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "PREFIX=" "LIBDIR=lib")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda* (#:key make-flags outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (apply invoke "make" "install"
                               (string-append "DESTDIR=" out) make-flags))))
                  (delete 'check))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/FFmpeg/nv-codec-headers")
    (synopsis
     "FFmpeg version of headers required to interface with NVIDIA codec APIs")
    (description
     "NV Codec headers are required for FFmpeg and other multimedia frameworks to interface with NVIDIA's hardware-accelerated video encoding and decoding.")
    (license license:expat)))
