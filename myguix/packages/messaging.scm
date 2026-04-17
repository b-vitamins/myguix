;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021, 2022 PantherX OS Team <team@pantherx.org>
;;; Copyright © 2022, 2023, 2024, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.org>

(define-module (myguix packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                :prefix license:)
  #:use-module (myguix build-system binary)
  #:use-module (myguix build-system chromium-binary)
  #:use-module ((myguix licenses)
                :prefix license:)
  #:use-module (ice-9 match))

(define-public element-desktop
  (package
    (name "element-desktop")
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://packages.element.io/debian/pool/main/e/"
                           name
                           "/"
                           name
                           "_"
                           version
                           "_amd64.deb"))
       (sha256
        (base32 "0q0hk1mkh3s7wdmzjdp5w35g2pdx24pivx2h9zxfdilg78nzdpxw"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~'(("lib/Element/element-desktop" (("out" "/lib/Element"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'setup-cwd
            (lambda _
              (copy-recursively "usr/" ".")
              ;; Use the more standard lib directory for everything.
              (rename-file "opt/" "lib")
              ;; Remove unneeded files.
              (delete-file-recursively "usr")
              ;; Fix the .desktop file binary location.
              (substitute* '("share/applications/element-desktop.desktop")
                (("/opt/Element/")
                 (string-append #$output "/bin/")))))
          (add-after 'install 'symlink-binary-file
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (symlink (string-append #$output "/lib/Element/element-desktop")
                       (string-append #$output "/bin/element-desktop")))))))
    (home-page "https://element.io/")
    (synopsis "Matrix collaboration client for desktop")
    (description
     "Element Desktop is a Matrix client for desktop with Element Web at
its core.")
    ;; not working?
    (properties '((release-monitoring-url . "https://github.com/element-hq/element-desktop/releases")))
    (license license:asl2.0)))

(define-public signal-desktop
  (package
    (name "signal-desktop")
    (version "8.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://updates.signal.org/desktop/apt/pool/s/"
                           name
                           "/"
                           name
                           "_"
                           version
                           "_amd64.deb"))
       (sha256
        (base32 "1lw6bl0xsp2hngiah3i13yh3qkzq16ky3vr4n6cf7rg85iyj30zc"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~'(("lib/Signal/signal-desktop" (("out" "/lib/Signal"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'setup-cwd
            (lambda _
              (copy-recursively "usr/" ".")
              ;; Use the more standard lib directory for everything.
              (rename-file "opt/" "lib")
              ;; Remove unneeded files.
              (delete-file-recursively "usr")
              ;; Fix the .desktop file binary location.
              (substitute* '("share/applications/signal-desktop.desktop")
                (("/opt/Signal/")
                 (string-append #$output "/bin/"))
                ;; Use a lowercase 'signal' WMClass, to match the
                ;; application ID, otherwise the icon is not displayed
                ;; correctly (see:
                ;; <https://github.com/signalapp/Signal-Desktop/issues/6868>)
                (("StartupWMClass=Signal")
                 "StartupWMClass=signal"))))
          (add-after 'install 'symlink-binary-file
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (symlink (string-append #$output "/lib/Signal/signal-desktop")
                       (string-append #$output "/bin/signal-desktop")))))))
    (home-page "https://signal.org/")
    (synopsis "Private messenger using the Signal protocol")
    (description
     "Signal Desktop is an Electron application that links with Signal on Android
or iOS.")
    ;; doesn't work?
    (properties '((release-monitoring-url . "https://github.com/signalapp/Signal-Desktop/releases")))
    (license license:agpl3)))

(define-public discord
  (package
    (name "discord")
    (version "0.0.130")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.dl2.discordapp.net/apps/linux/"
                           version
                           "/discord-"
                           version
                           ".deb"))
       (sha256
        (base32 "06wp41m1j8bdgs3qa9ad85sx28wy3kj8hyfn4pnzhw72vsgyyk12"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~(let ((rpath '(("out" "/share/discord"))))
          (map (lambda (file)
                 (list (string-append "share/discord/" file) rpath))
               '("Discord"
                 "chrome-sandbox"
                 "chrome_crashpad_handler"
                 "libEGL.so"
                 "libGLESv2.so"
                 "libffmpeg.so"
                 "libvk_swiftshader.so"
                 "libvulkan.so.1")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'setup-cwd
            (lambda _
              (copy-recursively "usr/" ".")
              ;; Remove unneeded files.
              (delete-file-recursively "usr")
              ;; Fix the .desktop file binary location.
              (substitute* '("share/discord/discord.desktop")
                (("/usr/share/discord/Discord")
                 (string-append #$output "/bin/discord"))))))))
    (home-page "https://discord.com/")
    (synopsis "Voice, video, and text chat for communities and friends")
    (description
     "Discord is an all-in-one voice, video, and text chat application for
communities and friends.")
    (license (license:nonfree "https://discord.com/terms"))))
