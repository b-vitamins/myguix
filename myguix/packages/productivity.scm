;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Karl Hallsby <karl@hallsby.com

(define-module (myguix packages productivity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (myguix build-system chromium-binary)
  #:use-module (myguix build-system binary)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module ((guix licenses)
                #:prefix free-license:))

(define-public anydesk
  (package
    (name "anydesk")
    (version "6.3.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.anydesk.com/linux/anydesk_"
                           version "_amd64.deb"))
       (sha256
        (base32 "13b5ab4a889vz39d36f45mhv3mlaxb305wsh3plk3dbjcrkkkirb"))))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:strip-binaries? #f ;For some reason it breaks the program
       #:patchelf-plan `(("usr/bin/anydesk" ("atk" "cairo"
                                             "fontconfig"
                                             "freetype"
                                             "gcc:lib"
                                             "gdk-pixbuf"
                                             "glib"
                                             "glu"
                                             "gtk+"
                                             "gtkglext"
                                             "libice"
                                             "libsm"
                                             "libx11"
                                             "libxcb"
                                             "libxdamage"
                                             "libxext"
                                             "libxfixes"
                                             "libxi"
                                             "libxkbfile"
                                             "libxmu"
                                             "libxrandr"
                                             "libxrender"
                                             "libxsts"
                                             "libxt"
                                             "mesa"
                                             "minizip"
                                             "pango"
                                             "pangox-compat"
                                             "polkit"
                                             "polkit-gnome"
                                             "pulseaudio")))
       #:install-plan `(("usr/bin/anydesk" "/bin/")
                        ("usr/share/polkit-1/actions/com.anydesk.anydesk.policy"
                         "/etc/polkit-1/actions/")
                        ("usr/share/icons" "/share/icons/"))
       #:phases (modify-phases %standard-phases
                  (replace 'binary-unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (let* ((files (filter (lambda (f)
                                              (not (string=? (basename f)
                                                    "environment-variables")))
                                            (find-files (getcwd))))
                             (binary-file (car files)))
                        (when (= 1
                                 (length files))
                          (mkdir "binary")
                          (chdir "binary")
                          (invoke "ar" "x" binary-file)
                          (invoke "tar" "xvf" "data.tar.gz")
                          (invoke "rm"
                                  "-rfv"
                                  "control.tar.gz"
                                  "data.tar.gz"
                                  binary-file
                                  "debian-binary")))))
                  (add-after 'wrap-program 'install-desktop-entry
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((desktop-file
                              "usr/share/applications/anydesk.desktop")
                             (out (assoc-ref outputs "out"))
                             (applications (string-append out
                                            "/share/applications")))
                        (substitute* desktop-file
                          (("^Exec=.*")
                           (string-append "Exec=" out "/bin/anydesk\n"))
                          (("^Icon=.*")
                           "Icon=anydesk\n"))
                        (install-file desktop-file applications)) #t)))))
    (inputs `(("atk" ,atk)
              ("cairo" ,cairo)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("gcc:lib" ,gcc "lib")
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("glib" ,glib)
              ("glu" ,glu)
              ("gtk+" ,gtk+-2)
              ("gtkglext" ,gtkglext)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxi" ,libxi)
              ("libxkbfile" ,libxkbfile)
              ("libxmu" ,libxmu)
              ("libxrandr" ,libxrandr)
              ("libxrender" ,libxrender)
              ("libxsts" ,libxtst)
              ("libxt" ,libxt)
              ("mesa" ,mesa)
              ("minizip" ,minizip)
              ("pango" ,pango)
              ("pangox-compat" ,pangox-compat)
              ("polkit" ,polkit)
              ("polkit-gnome" ,polkit-gnome)
              ("pulseaudio" ,pulseaudio)))
    (synopsis "Remote desktop software")
    (supported-systems '("x86_64-linux"))
    (description
     "Connect to a computer remotely, be it from the other end of
the office or halfway around the world.  AnyDesk ensures secure and reliable
remote desktop connections for IT professionals and on-the-go individuals alike.")
    (home-page "https://anydesk.com/")
    (license (license:nonfree "https://anydesk.com/en/terms"))))

(define-public anytype
  (package
    (name "anytype")
    (version "0.43.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://anytype-release.fra1.cdn.digitaloceanspaces.com/" name
             "_" version "_amd64.deb"))
       (file-name (string-append "anytype-" version ".deb"))
       (sha256
        (base32 "0w0gqvc0l4aszjnixnkkbf7qvcij7p3fq2x0cm9cj1r2aipp66vv"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; almost 300MB
      #:substitutable? #f
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan #~(map (lambda (file)
                              (string-append "opt/Anytype/" file))
                            '("anytype" "chrome-sandbox"
                              "chrome_crashpad_handler"
                              "libEGL.so"
                              "libffmpeg.so"
                              "libGLESv2.so"
                              "libvk_swiftshader.so"
                              "libvulkan.so.1"
                              "resources/app.asar.unpacked/node_modules/keytar/build/Release/keytar.node"
                              "resources/app.asar.unpacked/node_modules/keytar/build/Release/obj.target/keytar.node"))
      #:install-plan #~'(("opt/" "/share")
                         ("usr/share/" "/share"))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'binary-unpack 'disable-auto-updates
                     (lambda _
                       (delete-file "opt/Anytype/resources/app-update.yml")))
                   ;; We don't need regedit, a node library to interact with Windows
                   ;; hosts.
                   (add-after 'binary-unpack 'strip-regedit
                     (lambda _
                       (delete-file-recursively (string-append
                                                 "opt/Anytype/resources/app.asar.unpacked/"
                                                 "node_modules/regedit"))))
                   (add-after 'binary-unpack 'strip-python
                     (lambda _
                       (delete-file (string-append
                                     "opt/Anytype/resources/app.asar.unpacked/"
                                     "node_modules/keytar/build/node_gyp_bins/python3"))))
                   (add-before 'install 'patch-assets
                     (lambda _
                       (let* ((bin (string-append #$output "/bin"))
                              (usr/share "./usr/share")
                              (old-exe "/opt/Anytype/anytype")
                              (exe (string-append bin "/anytype")))
                         (substitute* (string-append usr/share
                                       "/applications/anytype.desktop")
                           (((string-append "^Exec=" old-exe))
                            (string-append "Exec=" exe))))))
                   (add-before 'install-wrapper 'symlink-entrypoint
                     (lambda _
                       (let* ((bin (string-append #$output "/bin"))
                              (exe (string-append bin "/anytype"))
                              (share (string-append #$output "/share/Anytype"))
                              (target (string-append share "/anytype")))
                         (mkdir-p bin)
                         (symlink target exe)
                         (wrap-program exe
                           `("LD_LIBRARY_PATH" =
                             (,share)))))))))
    (inputs (list bzip2
                  flac
                  gdk-pixbuf
                  harfbuzz
                  libexif
                  libglvnd
                  libpng
                  libva
                  libxscrnsaver
                  opus
                  pciutils
                  snappy
                  util-linux
                  xdg-utils
                  wget))
    (synopsis "Productivity and note-taking app")
    (supported-systems '("x86_64-linux"))
    (description
     "Anytype is an E2E encrypted, cross-platform, productivity and
note taking app. It stores all the data locally and allows for peer-to-peer
synchronization.")
    (home-page "https://anytype.io")
    (license (license:nonfree
              "https://github.com/anyproto/anytype-ts/blob/main/LICENSE.md"))))

(define-public zotero
  (package
    (name "zotero")
    (version "6.0.35")
    (source
     (origin
       ;; Can switch to git-fetch from Github too!
       (method url-fetch)
       (uri (string-append "https://download.zotero.org/client/release/"
                           version "/Zotero-" version "_linux-x86_64.tar.bz2"))
       (sha256
        (base32 "17f9an43jwnqpcslbvnhg7hrzkvs2whzwg4ysdgy2gl4m6cln18w"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    ;; Disable Zotero's automatic update feature.
                    (substitute* "defaults/preferences/prefs.js"
                      (("pref\\(\"app.update.enabled\", true\\)")
                       "pref(\"app.update.enabled\", false)")
                      (("pref\\(\"app.update.auto\", true\\)")
                       "pref(\"app.update.auto\", false)"))))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~70 MiB
      #:substitutable? #f
      #:validate-runpath? #t
      #:wrapper-plan #~'("zotero-bin")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'install-wrapper 'install-entrypoint
                     (lambda _
                       (let* ((bin (string-append #$output "/bin")))
                         (mkdir-p bin)
                         (symlink (string-append #$output "/zotero")
                                  (string-append bin "/zotero")))))
                   (add-after 'install 'create-desktop-file
                     (lambda _
                       (make-desktop-entry-file (string-append #$output
                                                 "/share/applications/zotero.desktop")
                                                #:name "Zotero"
                                                #:type "Application"
                                                #:generic-name
                                                "Reference Management"
                                                #:exec (string-append #$output
                                                        "/bin/zotero -url %U")
                                                #:icon "zotero"
                                                #:keywords '("zotero")
                                                #:categories '("Office"
                                                               "Database")
                                                #:terminal #f
                                                #:startup-notify #t
                                                #:startup-w-m-class "zotero"
                                                ;; MIME-type list taken from Zotero's shipped .desktop file
                                                #:mime-type '("x-scheme-handler/zotero"
                                                              "text/plain"
                                                              "application/x-research-info-systems"
                                                              "text/x-research-info-systems"
                                                              "text/ris"
                                                              "application/x-endnote-refer"
                                                              "application/x-inst-for-Scientific-info"
                                                              "application/mods+xml"
                                                              "application/rdf+xml"
                                                              "application/x-bibtex"
                                                              "text/x-bibtex"
                                                              "application/marc"
                                                              "application/vnd.citationstyles.style+xml")
                                                #:comment '(("en"
                                                             "Collect, organize, cite, and share your research sources")
                                                            (#f
                                                             "Collect, organize, cite, and share your research sources")))))
                   (add-after 'install 'install-icons
                     (lambda _
                       (let ((icon-sizes (list 16 32 48 256)))
                         (for-each (lambda (size)
                                     (mkdir-p (string-append #$output
                                               "/share/icons/hicolor/"
                                               size
                                               "x"
                                               size
                                               "/apps"))
                                     (copy-file (string-append
                                                 "chrome/icons/default/default"
                                                 size ".png")
                                                (string-append #$output
                                                 "/share/icons/hicolor/"
                                                 size
                                                 "x"
                                                 size
                                                 "/apps/zotero.png")))
                                   (map number->string icon-sizes))))))))
    ;; The zotero script that we wrap (which produces .zotero-real), has
    ;; this open file limit step done for us. If that script ever goes
    ;; away, then we can just uncomment this one.
    ;; (add-after 'install-wrapper 'raise-open-file-limit
    ;; (lambda _
    ;; (let ((file (string-append #$output "/bin/zotero")))
    ;; (with-output-to-file file
    ;; (lambda _
    ;; (display
    ;; (string-append
    ;; "#!/bin/sh\n"
    ;; ;; Raise the open files limit because Mozilla file
    ;; ;; functions leave files open for a tiny bit longer than
    ;; ;; necessary, so an installation with many translators and
    ;; ;; styles can exceed the default 1024 file limit. ulimit
    ;; ;; is a shell built-in, so we cannot use Guix's
    ;; ;; program-file function.
    ;; "ulimit -n 4096\n"
    ;; #$output "/bin/zotero-bin" " -app " #$output "/application.ini" " \"$@\""))))
    ;; (chmod file #o755))))
    (inputs (list dbus-glib libxt))
    (synopsis "Collect, organize, cite, and share your research sources")
    ;; If we build from source, then we may be able to support more
    ;; architectures. But Zotero is a Firefox/Electron app that uses a lot of
    ;; JavaScript, which may be problematic when packaging using Guix.
    (supported-systems '("x86_64-linux"))
    (description
     "Zotero is a research reference and bibliography tool.
Zotero helps you organize your research any way you want.  You can sort items
into collections and tag them with keywords.  Zotero instantly creates
references and bibliographies for any text editor, and directly inside Word,
LibreOffice, and Google Docs for over 10,000 citation styles.")
    (home-page "https://www.zotero.org")
    (license free-license:agpl3)))
