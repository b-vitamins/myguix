(define-module (myguix packages video)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix build-system python)
                #:select (pypi-uri))
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs)
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
    (version "13.0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FFmpeg/nv-codec-headers")
             (commit (string-append "n" version))))
       (sha256
        (base32 "01p6bjbgm6hfc1snf0hw63b7f7hif40v7bb1xn84ic3cww2m2fcw"))))
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
    (inherit ffmpeg)
    (name "ffmpeg-cuda")
    (version "6.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0f2fr8ywchhlkdff88lr4d4vscqzsi1ndjh3r5jwbkayf94lcqiv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        #~(append #$flags
                  (list "--enable-nonfree"
                        "--enable-cuda-nvcc"
                        "--enable-libnpp"
                        "--enable-nvdec"
                        "--enable-nvenc"
                        "--enable-cuvid"
                        "--enable-ffnvcodec"
                        "--enable-encoder=hevc_nvenc"
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
                        "--enable-protocol=https")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key outputs inputs configure-flags
                        #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (cuda-bin (string-append (assoc-ref inputs
                                                          "cuda-toolkit")
                                               "/bin"))
                      (cuda-nvcc (string-append (assoc-ref inputs
                                                           "cuda-toolkit")
                                                "/bin/nvcc"))
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
                         (string-append "--nvcc=" cuda-nvcc)
                         configure-flags))))))))
    (inputs (modify-inputs (package-inputs ffmpeg)
              (append cuda-toolkit nv-codec-headers)))
    (description
     "FFmpeg with NVIDIA hardware acceleration support using NVDEC and CUDA libraries.")
    (synopsis "FFmpeg with NVIDIA GPU hardware decoding (NVDEC) support")))

(define-public mpv-cuda
  (package
    (inherit mpv)
    (name "mpv-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments mpv)
       ((#:configure-flags flags)
        #~(append #$flags
                  ;; Disable OpenGL and GLX which are provided by Mesa
                  (list "-Dgl=disabled"
                        "-Degl-drm=disabled"
                        "-Degl-x11=disabled"
                        ;; Enable NVIDIA-specific support
                        "-Degl=enabled"
                        "-Dvdpau=enabled"
                        "-Dvaapi=enabled"
                        "-Dvulkan=enabled")))))
    (propagated-inputs (modify-inputs (package-propagated-inputs mpv)
                         (replace "ffmpeg" ffmpeg-cuda)
                         (append cuda-toolkit nv-codec-headers)))))

(define-public gmmlib
  (package
    (name "gmmlib")
    (version "22.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/gmmlib")
             (commit (string-append "intel-gmmlib-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ijvcmg33mmhc4sr76qgwbiacpnzbja7lh9fnm0scf8vysydlnjd"))))
    (build-system cmake-build-system)
    (arguments
     ;; Tests are run as part of the normal build step
     '(#:tests? #f))
    (home-page "https://github.com/intel/gmmlib")
    (synopsis "Intel Graphics Memory Management Library")
    (description
     "This package provides device specific and buffer management for the
Intel Graphics Compute Runtime for OpenCL and the Intel Media Driver
for VAAPI.")
    (license license:expat)))

(define-public intel-media-driver
  (package
    (name "intel-media-driver")
    (version "24.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/media-driver")
             (commit (string-append "intel-media-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mc50vls3q5b7cyaa3aljvp2gsrwhqbh9jggbqk47b81yqgid4mx"))))
    (build-system cmake-build-system)
    (inputs (list libva gmmlib))
    (native-inputs (list pkg-config))
    (arguments
     (list
      #:tests? #f ;Tests are run as part of the normal build step
      #:configure-flags
      #~(list "-DENABLE_NONFREE_KERNELS=OFF"
              (string-append "-DLIBVA_DRIVERS_PATH="
                             #$output "/lib/dri"))))
    ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
    ;; this to all VA-API back ends instead of once to libva.
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://github.com/intel/media-driver")
    (synopsis "Intel Media Driver for VAAPI")
    (description
     "This package provides a VA-API user mode driver supporting hardware
accelerated decoding, encoding, and video post processing for the GEN based
graphics hardware.")
    (license (list license:expat license:bsd-3))))

(define-public intel-media-driver/nonfree
  (package
    (inherit intel-media-driver)
    (name "intel-media-driver-nonfree")
    (arguments
     (substitute-keyword-arguments (package-arguments intel-media-driver)
       ((#:configure-flags flags
         #~'())
        #~(cons "-DENABLE_NONFREE_KERNELS=ON"
                (delete "-DENABLE_NONFREE_KERNELS=OFF"
                        #$flags)))))
    (synopsis (string-append (package-synopsis intel-media-driver)
                             " with nonfree kernels"))
    (description (string-append (package-description intel-media-driver)
                  "  This build of intel-media-driver includes nonfree blobs to fully enable the
video decode capabilities of supported Intel GPUs."))))

(define-public nvidia-vaapi-driver
  (package
    (name "nvidia-vaapi-driver")
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elFarto/nvidia-vaapi-driver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycrik4sdiy14miqvin5vg79776p7p2pazm0s8la4kngbgss1qr9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-install-path
            (lambda _
              (substitute* "meson.build"
                (("(nvidia_install_dir = ).*" _ prefix)
                 (format #f "~a'~a/lib/dri'" prefix
                         #$output))))))))
    (native-inputs (list pkg-config))
    (inputs (list libva mesa nv-codec-headers))
    ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
    ;; this to all VA-API back ends instead of once to libva.
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (home-page "https://github.com/elFarto/nvidia-vaapi-driver")
    (synopsis "VA-API implemention using NVIDIA's NVDEC.")
    (description
     "This is an VA-API implementation that uses NVDEC as a backend,
specifically designed to be used by Firefox for accelerated decoding of web
content.")
    (license license:expat)))

(define-public python-manim
  (package
    (name "python-manim")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "manim" version))
       (sha256
        (base32 "01a59ydddip5szsw3svl30zdrkhcmvf3wykdzm04k8nyq7zib0bl"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f ;Tests require various resources and display
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-skia-pathops-requirement
                    (lambda _
                      ;; Remove skia-pathops as it requires complex Skia library
                      (substitute* "pyproject.toml"
                        (("\"skia-pathops.*\",")
                         ""))
                      ;; Make boolean_ops optional by wrapping the whole module
                      (substitute* "manim/__init__.py"
                        (("from \\.mobject\\.geometry\\.boolean_ops import \\*")
                         "try:
    from .mobject.geometry.boolean_ops import *
except ImportError:
    pass  # boolean_ops requires skia-pathops")))))))
    (propagated-inputs (list python-av
                             python-beautifulsoup4
                             python-click
                             python-cloup
                             python-decorator
                             python-isosurfaces
                             python-manimpango
                             python-mapbox-earcut
                             python-moderngl
                             python-moderngl-window
                             python-networkx
                             python-numpy
                             python-pillow
                             python-pycairo
                             python-pydub
                             python-pygments
                             python-rich
                             python-scipy
                             python-screeninfo
                             python-srt
                             python-svgelements
                             python-tqdm
                             python-typing-extensions
                             python-watchdog))
    (inputs (list ffmpeg))
    (native-inputs (list python-poetry-core))
    (home-page "https://www.manim.community/")
    (synopsis "Mathematical animation engine")
    (description
     "Manim is an animation engine for explanatory math videos. It's used to create precise programmatic animations, designed for creating explanatory math videos.")
    (license license:expat)))
