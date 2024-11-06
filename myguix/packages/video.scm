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

(define-public ffmpeg-cuda
  (package
    (inherit ffmpeg-7)
    (name "ffmpeg-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg-7)
       ((#:configure-flags flags)
        #~(append #$flags
                  (list "--enable-nonfree"
                        "--enable-cuda-nvcc"
                        "--enable-libnpp"
                        "--enable-nvdec"
                        "--enable-nvenc"
                        "--enable-cuvid"
                        "--enable-encoder=h264_nvenc"
                        "--enable-decoder=h264_cuvid"
                        "--enable-decoder=aac"
                        "--enable-decoder=h264"
                        "--enable-decoder=rawvideo"
                        "--enable-indev=lavfi"
                        "--enable-demuxer=mov"
                        "--enable-muxer=mp4"
                        "--enable-filter=scale"
                        "--enable-filter=testsrc2"
                        "--enable-protocol=file"
                        "--enable-protocol=https"
                        "--nvccflags=-gencode arch=compute_80,code=sm_80 -O2")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key outputs inputs configure-flags
                        #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (cuda-bin (string-append (assoc-ref inputs
                                                          "cuda-toolkit")
                                               "/bin"))
                      (cuda-lib (string-append (assoc-ref inputs
                                                          "cuda-toolkit")
                                               "/lib"))
                      (cuda-include (string-append (assoc-ref inputs
                                                              "cuda-toolkit")
                                                   "/include")))
                  (substitute* "configure"
                    (("#! /bin/sh")
                     (string-append "#!"
                                    (which "sh"))))
                  (setenv "SHELL"
                          (which "bash"))
                  (setenv "CONFIG_SHELL"
                          (which "bash"))
                  (apply invoke
                         "./configure"
                         (string-append "--prefix=" out)
                         ;; Add $libdir to the RUNPATH of all the binaries.
                         (string-append "--extra-ldflags=-Wl,-rpath=" out
                                        "/lib")
                         (string-append "--extra-cflags=-I" cuda-include)
                         (string-append "--extra-cflags=-I" cuda-bin)
                         (string-append "--extra-ldflags=-L" cuda-lib)
                         configure-flags))))))))
    (inputs (modify-inputs (package-inputs ffmpeg)
              (replace "mesa" nvda)
              (append cuda-toolkit nv-codec-headers)))
    (description
     "FFmpeg with NVIDIA hardware acceleration support using NVDEC and CUDA libraries.")
    (synopsis "FFmpeg with NVIDIA GPU hardware decoding (NVDEC) support")))

(define-public mpv-cuda
  (package
    (inherit mpv)
    (name "mpv-cuda")
    (propagated-inputs (modify-inputs (package-propagated-inputs mpv)
                         (append nvda cuda-toolkit)
                         (replace "ffmpeg" ffmpeg-cuda)
                         (append nv-codec-headers)))))
