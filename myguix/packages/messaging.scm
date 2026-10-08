;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021, 2022 PantherX OS Team <team@pantherx.org>
;;; Copyright © 2022, 2023, 2024, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.org>
;;; Copyright © 2025, 2026 Ashish SHUKLA <ashish.is@lostca.se>

(define-module (myguix packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
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
    (version "1.12.27")
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
        (base32 "1b1qfryls2v3y02mchn4ya6xii9x7y42kgliwdabkz2k9i8gx088"))))
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
                 (string-append #$output "/bin/"))
                ;; Use a lowercase 'element' WMClass, to match the
                ;; application ID, otherwise the icon is not displayed
                ;; correctly when using Wayland (see:
                ;; <https://github.com/element-hq/element-web/pull/33635>).
                (("StartupWMClass=Element")
                 "StartupWMClass=element"))))
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
    (version "8.29.0")
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
        (base32 "0l6m2dhmdg4ayk5zm1wnip9vcy2cjhmw5zklbgca5n4czq3mx5rw"))))
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
    (version "1.0.153")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://stable.dl2.discordapp.net/apps/linux/"
                           version
                           "/discord-"
                           version
                           ".deb"))
       (sha256
        (base32 "1zzclkg95nf3v9j3xm7wh5v0gsk7j2d2w2w7bza5i8lpp9qmrw63"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~'(("share/discord/updater_bootstrap"
           (("out" "/share/discord"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'setup-cwd
            (lambda _
              (copy-recursively "usr/" ".")
              ;; Remove unneeded files.
              (delete-file-recursively "usr")
              ;; Fix the launcher and .desktop file binary locations.
              (substitute* '("bin/discord")
                (("bootstrap=/usr/share/\\$BOOTSTRAP_SUFFIX")
                 (string-append "bootstrap=" #$output "/share/$BOOTSTRAP_SUFFIX")))
              (substitute* '("share/discord/discord.desktop"
                             "share/applications/discord.desktop")
                (("/usr/share/discord/Discord")
                 (string-append #$output "/bin/discord"))
                (("/usr/bin/discord")
                 (string-append #$output "/bin/discord"))
                (("Path=/usr/bin")
                 (string-append "Path=" #$output "/bin"))))))))
    (home-page "https://discord.com/")
    (synopsis "Voice, video, and text chat for communities and friends")
    (description
     "Discord is an all-in-one voice, video, and text chat application for
communities and friends.")
    (license (license:nonfree "https://discord.com/terms"))))

(define-public webex
  (package
    (name "webex")
    (version "46.8.0.35631")
    (source
     (origin
       (method url-fetch)
       ;; Cisco publishes this as the current Linux DEB rather than under a
       ;; stable versioned URL.
       (uri
        "https://binaries.webex.com/WebexDesktop-Ubuntu-Official-Package/Webex.deb")
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 "1lrwjvq8s2yvf9p6bfgrd7kpm1jk2007pfsxwf5imrh7fimdn6m1"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; The unpacked package is about 1.1 GiB.
      #:substitutable? #f
      #:validate-runpath? #f ;TODO: fails on bundled Qt/CEF plugins.
      #:wrapper-plan
      #~(let ((rpath '(("out" "/lib/Webex/bin")
                       ("out" "/lib/Webex/lib")
                       ("out" "/lib/Webex/lib/plugins/platforms")
                       ("out" "/lib/Webex/lib/plugins/xcbglintegrations")
                       ("nss" "/lib/nss"))))
          (map (lambda (file)
                 (list file rpath))
               '("opt/Webex/bin/CiscoCollabHost"
                 "opt/Webex/bin/CiscoCollabHostCef"
                 "opt/Webex/bin/CiscoCollabHostCefWM"
                 "opt/Webex/bin/WebexFileSelector"
                 "opt/Webex/bin/pxgsettings")))
      #:install-plan
      #~'(("opt/" "/lib")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'patch-desktop-entry
            (lambda _
              (mkdir-p "usr/share/applications")
              (mkdir-p "usr/share/icons/hicolor/96x96/apps")
              (copy-file "opt/Webex/bin/webex.desktop"
                         "usr/share/applications/webex.desktop")
              (copy-file "opt/Webex/bin/sparklogosmall.png"
                         "usr/share/icons/hicolor/96x96/apps/webex.png")
              (substitute* "usr/share/applications/webex.desktop"
                (("^Exec=/opt/Webex/bin/CiscoCollabHost %U")
                 (string-append "Exec=" #$output "/bin/webex %U"))
                (("^Icon=/opt/Webex/bin/sparklogosmall.png")
                 "Icon=webex")
                (("^Categories=.*")
                 "Categories=Network;InstantMessaging;VideoConference;\n"))
              #t))
          (add-before 'install 'delete-broken-qt-ffmpeg-plugin
            (lambda _
              ;; Cisco's Qt multimedia ffmpeg plugin links against FFmpeg 7
              ;; SONAMEs, which this Guix snapshot does not provide.  Leaving a
              ;; broken plugin around is worse than falling back to Webex's
              ;; bundled conferencing media stack.
              (delete-file "opt/Webex/lib/plugins/multimedia/libffmpegmediaplugin.so")
              #t))
          (add-after 'strip 'patch-webex-rpaths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim))
              (define (input-library-directories)
                (let ((directories '()))
                  (for-each
                   (lambda (input)
                     (let ((lib (string-append (cdr input) "/lib")))
                       (when (directory-exists? lib)
                         (set! directories (cons lib directories))))
                     (when (string=? (car input) "nss")
                       (let ((nss (string-append (cdr input) "/lib/nss")))
                         (when (directory-exists? nss)
                           (set! directories (cons nss directories))))))
                   inputs)
                  (reverse directories)))
              (define bundled-rpaths
                '("$ORIGIN"
                  "$ORIGIN/../bin"
                  "$ORIGIN/../lib"
                  "$ORIGIN/plugins/platforms"
                  "$ORIGIN/plugins/xcbglintegrations"
                  "$ORIGIN/../lib/plugins/platforms"
                  "$ORIGIN/../lib/plugins/xcbglintegrations"))
              (define runtime-rpath
                (string-join (append bundled-rpaths
                                     (input-library-directories))
                             ":"))
              (define (command-output . command)
                (let* ((port (apply open-pipe* OPEN_READ command))
                       (output (read-string port)))
                  (close-pipe port)
                  (string-trim-right output #\newline)))
              (define (current-rpath file)
                (catch #t
                  (lambda _
                    (command-output "patchelf" "--print-rpath" file))
                  (lambda _
                    "")))
              (define (patch-rpath file)
                (when (elf-file? file)
                  (let* ((old-rpath (current-rpath file))
                         (new-rpath
                          (if (string-null? old-rpath)
                              runtime-rpath
                              (string-append old-rpath ":" runtime-rpath))))
                    (format #t "Patching Webex ELF RPATH: ~a~%" file)
                    (invoke "patchelf" "--set-rpath" new-rpath file))))
              (for-each patch-rpath
                        (find-files (string-append (assoc-ref outputs "out")
                                                   "/lib/Webex")))))
          (add-before 'install-wrapper 'install-entrypoint
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/webex"))
                     (webex (string-append #$output "/lib/Webex"))
                     (target (string-append webex
                                            "/bin/CiscoCollabHost"))
                     (sh (string-append (assoc-ref inputs "bash-minimal")
                                        "/bin/sh")))
                (mkdir-p bin)
                (with-output-to-file exe
                  (lambda _
                    (display "#!")
                    (display sh)
                    (display "\n")
                    (display "webex_dir=\"")
                    (display webex)
                    (display "\"\n")
                    (display "export ACCESSIBILITY_ENABLED=${ACCESSIBILITY_ENABLED:-1}\n")
                    (display "export QT_PLUGIN_PATH=\"$webex_dir/lib/plugins${QT_PLUGIN_PATH:+:$QT_PLUGIN_PATH}\"\n")
                    (display "export QML2_IMPORT_PATH=\"$webex_dir/qml${QML2_IMPORT_PATH:+:$QML2_IMPORT_PATH}\"\n")
                    (display "export XDG_DATA_DIRS=\"")
                    (display #$output)
                    (display "/share${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}\"\n")
                    (display "export LD_LIBRARY_PATH=\"$webex_dir/bin:$webex_dir/lib:$webex_dir/lib/plugins/platforms:$webex_dir/lib/plugins/xcbglintegrations${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n")
                    (display "if [ \"${WEBEX_ALLOW_WAYLAND:-0}\" != 1 ]; then\n")
                    (display "  export QT_QPA_PLATFORM=${QT_QPA_PLATFORM:-xcb}\n")
                    (display "  export GDK_BACKEND=${GDK_BACKEND:-x11}\n")
                    (display "  unset WAYLAND_DISPLAY\n")
                    (display "fi\n")
                    (display "if [ -n \"${HOME:-}\" ]; then\n")
                    (display "  \"")
                    (display #$(file-append coreutils "/bin/mkdir"))
                    (display "\" -p \"$HOME/.local/share/Webex/hostLogs\" \"$HOME/.local/share/WebexLauncher\" 2>/dev/null || true\n")
                    (display "fi\n")
                    (display "cd \"$webex_dir/bin\"\n")
                    (display "exec \"")
                    (display target)
                    (display "\" \"$@\"\n")))
                (chmod exe #o555)
                #t))))))
    (inputs (list libglvnd
                  libxcrypt
                  libxscrnsaver
                  openssl-1.1
                  upower
                  wayland
                  xcb-util-cursor
                  `(,zstd "lib")))
    (home-page "https://www.webex.com/")
    (synopsis "Cisco Webex messaging, meeting, and calling client")
    (description
     "Webex is Cisco's client for messaging, meetings, and one-to-one calling.")
    (license (license:nonfree "https://www.webex.com/terms-of-service.html"))))
