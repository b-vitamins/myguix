;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (myguix packages remote-desktop)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (myguix build-system binary)
  #:use-module ((myguix licenses)
                #:prefix license:))

(define-public anydesk
  (package
    (name "anydesk")
    (version "8.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (let* ((system (or (%current-target-system)
                               (%current-system)))
                   (arch (match system
                           ("x86_64-linux" "amd64")
                           ("aarch64-linux" "arm64")
                           (_ "unsupported"))))
              (string-append
               "https://deb.anydesk.com/pool/main/a/anydesk/anydesk_"
               version
               "_"
               arch
               ".deb")))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "07ick86npvja00js59jm2ak15lfd1d67c7zj1z7wbcx0yv1hr1kp")
                  ("aarch64-linux"
                   "1hld31yqsngvyvjbx16kz91rr1fw2g045bilvsvj9261gb8xr3ms")
                  (_ "0000000000000000000000000000000000000000000000000000"))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (build-system binary-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan
      #~'(("usr/bin/anydesk"
           ("at-spi2-core"
            "cairo"
            "dbus"
            "eudev"
            "fontconfig-minimal"
            "gcc"
            "gdk-pixbuf"
            "glib"
            "gtk+"
            "libepoxy"
            "libevdev"
            "libglvnd"
            "libx11"
            "libxcb"
            "libxdamage"
            "libxext"
            "libxfixes"
            "libxi"
            "libxkbfile"
            "libxrandr"
            "libxrender"
            "libxtst"
            "pango"
            "polkit"
            "wayland"
            "zlib")))
      #:install-plan
      #~'(("usr/bin/" "/bin")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'patch-install-paths
            (lambda _
              (substitute* "usr/share/applications/anydesk.desktop"
                (("^Exec=/usr/bin/anydesk %u")
                 (string-append "Exec=" #$output "/bin/anydesk %u"))
                (("^TryExec=anydesk")
                 (string-append "TryExec=" #$output "/bin/anydesk")))
              (substitute*
                  "usr/share/polkit-1/actions/com.anydesk.anydesk.policy"
                (("/usr/bin/anydesk")
                 (string-append #$output "/bin/anydesk")))
              (substitute* "usr/share/anydesk/files/systemd/anydesk.service"
                (("/usr/bin/anydesk")
                 (string-append #$output "/bin/anydesk")))
              (substitute* "usr/bin/anydesk-global-settings"
                (("^pkexec /usr/bin/anydesk --admin-settings")
                 (string-append #$(file-append polkit "/bin/pkexec")
                                " "
                                #$output
                                "/bin/anydesk --admin-settings")))
              #t)))))
    (inputs (list at-spi2-core
                  cairo
                  dbus
                  eudev
                  fontconfig
                  `(,gcc "lib")
                  gdk-pixbuf
                  glib
                  gtk+
                  libepoxy
                  libevdev
                  libglvnd
                  libx11
                  libxcb
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxkbfile
                  libxrandr
                  libxrender
                  libxtst
                  pango
                  polkit
                  wayland
                  zlib))
    (home-page "https://anydesk.com/")
    (synopsis "Remote desktop application")
    (description
     "AnyDesk is a remote desktop application for desktop sharing, remote
control, file transfer, and remote support.")
    (license (license:nonfree "https://anydesk.com/en/terms"))))
