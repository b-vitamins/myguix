(define-module (myguix packages video)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module ((gnu packages fontutils)
                #:hide (python-skia-pathops))
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-xyz)
                #:hide (python-manimpango))
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
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
  #:use-module (myguix packages)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages chrome)
  #:use-module (myguix build-system binary)
  #:use-module ((myguix licenses)
                #:prefix myguix-license:)
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

(define-public ffmpeg-nvidia
  (package
    (inherit ffmpeg)
    (name "ffmpeg-nvidia")
    (version "6.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0f2fr8ywchhlkdff88lr4d4vscqzsi1ndjh3r5jwbkayf94lcqiv"))))
    (inputs
     (modify-inputs
         (package-inputs ffmpeg)
       (prepend nv-codec-headers)))
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        ;; All the codecs from the original plus NVENC support
        #~(append #$flags
                  (list "--enable-nonfree"
                        "--enable-nvdec"
                        "--enable-nvenc"
                        "--enable-cuvid"
                        "--enable-ffnvcodec"
                        "--enable-encoder=hevc_nvenc"
                        "--enable-encoder=h264_nvenc"
                        "--enable-decoder=hevc_cuvid"
                        "--enable-decoder=h264_cuvid"
                        "--enable-decoder=mjpeg_cuvid"
                        "--enable-decoder=mpeg1_cuvid"
                        "--enable-decoder=mpeg2_cuvid"
                        "--enable-decoder=mpeg4_cuvid"
                        "--enable-decoder=vc1_cuvid"
                        "--enable-decoder=vp8_cuvid"
                        "--enable-decoder=vp9_cuvid"
                        "--enable-decoder=av1_cuvid"
                        "--enable-decoder=aac"
                        "--enable-decoder=h264"
                        "--enable-decoder=rawvideo"
                        "--enable-indev=lavfi"
                        "--enable-demuxer=mov"
                        "--enable-muxer=mp4"
                        "--enable-filter=scale"
                        "--enable-filter=testsrc2"
                        "--enable-protocol=file"
                        "--enable-protocol=https")))))
    (description
     (string-append
      (package-description ffmpeg)
      "  This build of FFmpeg includes nonfree NVIDIA encoders and decoders for
hardware acceleration including h264_nvenc, hevc_nvenc encoders and various
cuvid decoders."))
    (properties '((upstream-name . "ffmpeg")))))

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
                         (replace "ffmpeg" ffmpeg-nvidia)
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
    (version "25.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/media-driver")
             (commit (string-append "intel-media-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rp4s9a4x26p07w36ywql7qz7gyk15mgp9yrdx9j2b9qbmr1w1zs"))))
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
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "manim" version))
       (sha256
        (base32 "0sz1j8yijdz5zjkjc5w1c55wx4qp2iyxgjwvs25fbgf15zxlg5qy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-ffmpeg
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((ffmpeg-bin (string-append (assoc-ref inputs "ffmpeg")
                                               "/bin")))
                (for-each
                 (lambda (program)
                   (when (file-exists? program)
                     (wrap-program program
                       `("PATH" ":" prefix (,ffmpeg-bin)))))
                 (list (string-append #$output "/bin/manim")
                       (string-append #$output "/bin/manimce")))))))))
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
                             python-skia-pathops
                             python-srt
                             python-svgelements
                             python-tqdm
                             python-typing-extensions
                             python-watchdog))
    (inputs (list ffmpeg))
    (native-inputs (list python-hatchling))
    (home-page "https://www.manim.community/")
    (synopsis "Mathematical animation engine")
    (description
     "Manim is an animation engine for explanatory math videos. It's used to create precise programmatic animations, designed for creating explanatory math videos.")
    (license license:expat)))

(define-public grayjay
  (package
    (name "grayjay")
    (version "9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://updater.grayjay.app/Apps/Grayjay.Desktop/"
                           version "/Grayjay.Desktop-linux-x64-v"
                           version ".zip"))
       (file-name (string-append name "-" version "-x86_64.zip"))
       (sha256
        (base32 "1k27rlqbmhfc3v4x4vfl7kvshmmp0dvwra94bi0jr2anfyrzvh0j"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f         ; prevent corruption of .NET programs
           #:patchelf-plan
           #~(let ((libs '("alsa-lib"
                           "at-spi2-core"
                           "cairo"
                           "cups"
                           "dbus"
                           "eudev"
                           "expat"
                           "fontconfig-minimal"
                           "gcc"
                           "glib"
                           "glibc"
                           "gtk+"
                           "icu4c"
                           "libdrm"
                           "libnotify"
                           "librsvg"
                           "libsecret"
                           "libx11"
                           "libxcb"
                           "libxcomposite"
                           "libxcursor"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxi"
                           "libxkbcommon"
                           "libxkbfile"
                           "libxrandr"
                           "libxrender"
                           "libxshmfence"
                           "libxtst"
                           "mesa"
                           "mit-krb5"
                           "nspr"
                           ("nss" "/lib/nss")
                           ("out" "/lib/grayjay/cef")
                           "openssl"
                           "pango"
                           "pulseaudio"
                           "sqlcipher"
                           "xcb-util"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "xcb-util-renderutil"
                           "xcb-util-wm"
                           "xdg-utils"
                           "zlib")))
               `(("ClearScriptV8.linux-x64.so" ,libs)
                 ("Grayjay" ,libs)
                 ("cef/chrome-sandbox" ,libs)
                 ("cef/dotcefnative" ,libs)
                 ;; Some of these likely are not directly used after
                 ;; patchelf-ing the main binaries, other than libcef.so.
                 ;; This allows validate-runpath to pass though.
                 ("cef/libEGL.so" ,libs)
                 ("cef/libGLESv2.so" ,libs)
                 ;; XXX: Can replace with chromium-embedded-framework?
                 ("cef/libcef.so" ,libs)
                 ("cef/libvk_swiftshader.so" ,libs)
                 ("cef/libvulkan.so.1" ,libs)
                 ("libe_sqlite3.so" ,libs)
                 ("libsodium.so" ,libs)))
           #:install-plan ''(("." "lib/grayjay"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'remove-files
                 (lambda _
                   ;; Disable automatic updates, unbundle ffmpeg, and remove
                   ;; "Portable" which makes Grayjay try to (unsuccessfully)
                   ;; run from its installed path.  (Grayjay doesn't find the
                   ;; updater or ffmpeg when run outside of lib/grayjay.)
                   (delete-file "FUTO.Updater.Client")
                   (delete-file "ffmpeg")
                   (delete-file "Portable")))
               (add-before 'install 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/lib/grayjay/Grayjay")
                              (string-append bin "/Grayjay")))))
               (add-before 'install 'install-icon
                 (lambda _
                   (let ((dir (string-append
                               #$output
                               "/share/icons/hicolor/scalable/apps")))
                     (mkdir-p dir)
                     (copy-file "grayjay.png"
                                (string-append dir
                                               "/app.grayjay.Grayjay.png")))))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (string-append #$output "/lib/grayjay/Grayjay")
                     `("PATH" prefix
                       (,(string-append #$(this-package-input "ffmpeg")
                                        "/bin"))))))
               (add-after 'install 'create-desktop-file
                 (lambda _
                   (make-desktop-entry-file
                    (string-append #$output "/share/applications/Grayjay.desktop")
                    #:name "Grayjay"
                    #:type "Application"
                    #:exec (string-append #$output "/bin/Grayjay")
                    #:icon "app.grayjay.Grayjay"
                    #:categories '("AudioVideo" "Player")
                    #:startup-w-m-class "Grayjay"
                    #:comment "Universal media aggregator"))))))
    (native-inputs (list unzip))
    (inputs (list alsa-lib
                  at-spi2-core
                  bash-minimal
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  ffmpeg
                  fontconfig
                  freetype
                  `(,gcc "lib")
                  glib
                  glibc
                  gtk+
                  icu4c-76
                  libdrm
                  libnotify
                  librsvg
                  libsecret
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxrender
                  libxshmfence
                  libxtst
                  mesa
                  mit-krb5
                  nspr
                  nss
                  openssl
                  pango
                  pulseaudio
                  sqlcipher
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  xdg-utils
                  zlib))
    (home-page "https://grayjay.app/")
    (synopsis "Universal media aggregator")
    (description "Grayjay is a media aggregator application that enables users
to stream and download multimedia content from various online sources, most
prominently YouTube.  It also offers an extensible plugin API to create and
import new integrations.")
    (license
     ;; "Source First License 1.1" which allows distribution, modification,
     ;; etc. but with a non-commercial prohibition.
     (myguix-license:nonfree
      "https://gitlab.futo.org/videostreaming/grayjay/-/blob/master/LICENSE.md"))))

(define-public makemkv
  (package
    (name "makemkv")
    ;; This is not the last version, but newer ones like 1.18.3 have a bug
    ;; where the 'makemkvcon' process hang at 100% CPU when attempting to read
    ;; a Blu-ray disc, as reported by multiple users (see for example:
    ;; <https://forum.makemkv.com/forum/viewtopic.php?t=35897> and
    ;; <https://forum.makemkv.com/forum/viewtopic.php?p=178014>).
    (version "1.17.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.makemkv.com/download/old/"
                           name "-oss-" version ".tar.gz"))
       (sha256
        (base32
         "1vx0sf8y5kl0l3szc3hd28anm7pxq2bpvjrdqskpbv7r8qnmabkn"))
       (patches (myguix-patches "makemkv-app-id.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules (cons '(guix build qt-utils)
                               %default-gnu-imported-modules)
      #:modules (cons '(guix build qt-utils)
                      %default-gnu-modules)
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-ldconfig-invocation
            (lambda _
              (substitute* "Makefile.in"
                (("\tldconfig.*") ""))))
          (add-after 'install 'install-makemkv-bin
            ;; This is the closed-source binary component of makemkv, which
            ;; contains e.g. the 'makemkvcon' executable for retrieving keys
            ;; from their server.
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (invoke "tar" "xf"
                      #$(this-package-native-input
                         (format #f "makemkv-bin-~a.tar.gz"
                                 (package-version this-package))))
              (with-directory-excursion #$(string-append "makemkv-bin-" version)
                (substitute* "Makefile"
                  ;; Automatically accept the EULA non-interactively.
                  (("@/bin/bash src/ask_eula.sh") "true"))
                (apply (assoc-ref %standard-phases 'install)
                       `(,@args #:make-flags
                                (,(string-append "PREFIX="
                                                 #$output))))
                (install-file "src/eula_en_linux.txt"
                              (string-append #$output "/share/MakeMKV")))
              ;; Fix the RUNPATH of the makemkvcon binary.
              (let ((makemkvcon (string-append #$output "/bin/makemkvcon")))
                (invoke "patchelf" "--set-rpath"
                        ;; libcurl is dlopen'ed from makemkvcon
                        (string-append #$output "/lib:"
                                       (dirname (search-input-file
                                                 inputs "lib/libcurl.so")))
                        makemkvcon)
                (invoke "patchelf" "--set-interpreter"
                        (search-input-file inputs #$(glibc-dynamic-linker))
                        makemkvcon))))
          (add-after 'install 'wrap-qt
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-qt-program "makemkv"
                               #:output #$output
                               #:inputs inputs))))))
    (native-inputs
     (list patchelf
           pkg-config
           (origin
             (method url-fetch)
             (uri (string-append "https://www.makemkv.com/download/old/"
                                 name "-bin-" version ".tar.gz"))
             (sha256
              (base32
               "1l2ii5k6bjgzy20d29mng4j0pnwjwdj0qif87j3iyawmphqwhnwc")))))
    (inputs (list curl ffmpeg-6 expat openssl qtbase-5 qtwayland-5 zlib))
    (home-page "https://www.makemkv.com")
    (synopsis "Video converter with support for Blu-ray and DVD encryption")
    (description "MakeMKV allows converting the video clips from
proprietary (and usually encrypted) discs into a set of MKV files, preserving
most information but not changing it in any way.  The MKV format can store
multiple video/audio tracks with all meta-information and preserve chapters.
There are many players that can play MKV files nearly on all platforms, and
there are tools to convert MKV files to many formats, including DVD and
Blu-ray discs.

Additionally, MakeMKV can instantly stream decrypted video without
intermediate conversion to wide range of players, so you may watch Blu-ray and
DVD discs with your favorite player.  This is made possible via its
@code{libmmdb} library, which can act as a replacement for the @code{libaacs}
library.  To use it with VLC for example, you can force its use instead of the
regular @code{libaacs} library by setting the following (@code{libbluray},
used by VLC) environment variable:

@example
guix install makemkv vlc
export MAKEMKVCON=$(which makemkvcon)
export LIBAACS_PATH=$HOME/.guix-profile/lib/libmmbd
export LIBBDPLUS_PATH=$HOME/.guix-profile/lib/libmmbd
vlc /dev/sr0
@end example

Among its features are:
@itemize
@item Reads DVD and Blu-ray discs
@item Reads Blu-ray discs protected with latest versions of AACS and BD+
@item Preserves all video and audio tracks, including HD audio
@item Preserves chapters information
@item Preserves all meta-information (track language, audio type)
@item Fast conversion -- converts as fast as your drive can read data
@item No additional software required for conversion or decryption.
@end itemize

IMPORTANT:
@itemize
@item
By installing this package, you agree to its end user license
agreement, which you can read at @file{share/MakeMKV/eula_en_linux.txt}.
@item
Purchasing a license key is required to use this older version.
@item
UHD (4K) Blu-ray playback requires LibreDrive compatibility.  Do your research
before buying a Blu-ray drive!
@end itemize")
    ;; Redistributable, with a proprietary license (shareware).
    (license (myguix-license:nonfree "file://License.txt"))
    (supported-systems (list "x86_64-linux" "i686-linux"
                             "aarch64-linux" "armhf-linux"))))
