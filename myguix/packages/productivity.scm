;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (myguix build-system chromium-binary)
  #:use-module (myguix build-system binary)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module ((guix licenses)
                #:prefix free-license:))

(define-public anytype
  (package
    (name "anytype")
    (version "0.52.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://anytype-release.fra1.cdn.digitaloceanspaces.com/" name
             "_" version "_amd64.deb"))
       (file-name (string-append "anytype-" version ".deb"))
       (sha256
        (base32 "0b6x20wqi428qki6379sjrvq7xfp7g4ghcxc0d2j9nv7vspqmyy6"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; almost 300MB
      #:substitutable? #f
      #:validate-runpath? #f ;TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~(map (lambda (file)
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
      #:install-plan
      #~'(("opt/" "/share")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
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
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,share)))))))))
    (inputs (list bzip2
                  flac
                  `(,gcc-14 "lib")
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

(define-public obsidian
  (package
    (name "obsidian")
    (version "1.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/obsidianmd/obsidian-releases/releases/download/"
             "v"
             version
             "/obsidian-"
             version
             (match (or (%current-target-system)
                        (%current-system))
               ("x86_64-linux" "") ;x86_64 does not have any special indication
               ("aarch64-linux" "-arm64")
               ;; We should provide a default case.
               (_ "unsupported"))
             ".tar.gz"))
       (file-name (string-append "obsidian-" version ".tar.gz"))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "1pn8hk09q7dribjn9zn13v3h1x25l17zg0w0pq3qwgqjrzgjdsvj")
                  ("aarch64-linux"
                   "1n7vn74f5vgjnj3gqlz5x63cacn86y206kklyb904kna449qj83w")
                  ;; We need a valid base case for base32
                  (_ "0000000000000000000000000000000000000000000000000000"))))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ;TODO: fails on wrapped binary (.obsidian-real)
      #:substitutable? #f
      #:wrapper-plan
      #~(list "obsidian")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-wrapper 'install-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/obsidian")
                         (string-append bin "/obsidian")))))
          ;; NOTE: Obsidian's icon SVG does not conform to SVG standards.
          (add-after 'install 'create-desktop-icons
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((convert (search-input-file inputs "/bin/convert"))
                    (svg (assoc-ref inputs "obsidian-logo-gradient.svg"))
                    (sizes (list "32x32"
                                 "48x48"
                                 "64x64"
                                 "128x128"
                                 "256x256"
                                 "512x512")))
                (for-each (lambda (size)
                            (mkdir-p (string-append #$output
                                                    "/share/icons/hicolor/"
                                                    size "/apps"))
                            (invoke convert
                                    "-background"
                                    "none"
                                    "-resize"
                                    size
                                    svg
                                    (string-append #$output
                                                   "/share/icons/hicolor/"
                                                   size "/apps/obsidian.png")))
                          sizes))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (make-desktop-entry-file (string-append #$output
                                        "/share/applications/obsidian.desktop")
                                       #:name "Obsidian"
                                       #:type "Application"
                                       #:generic-name "Markdown Editor"
                                       #:exec (string-append #$output
                                                             "/bin/obsidian")
                                       #:icon "obsidian"
                                       #:keywords '("obsidian")
                                       #:categories '("Application" "Office")
                                       #:terminal #f
                                       #:startup-notify #t
                                       #:startup-w-m-class "obsidian"
                                       #:mime-type "x-scheme-handler/obsidian"
                                       #:comment '(("en" "Knowledge base")
                                                   (#f "Knowledge base"))))))))
    (native-inputs
     ;; imagemagick & inkscape needed to create desktop icons. We use the
     ;; stable versions because we only need them for generating icons.
     (list imagemagick/stable inkscape/pinned))
    (inputs (list (origin
                    (method url-fetch)
                    (uri
                     "https://obsidian.md/images/obsidian-logo-gradient.svg")
                    (sha256 (base32
                             "100j8fcrc5q8zv525siapminffri83s2khs2hw4kdxwrdjwh36qi")))))
    (synopsis "Markdown-based knowledge base")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (description
     "Obsidian is a powerful knowledge base that works on top of a
local folder of plain text Markdown files.  Obsidian makes following
connections frictionless, and with the connections in place, you can explore
all of your knowledge in the interactive graph view.  Obsidian supports
CommonMark and GitHub Flavored Markdown (GFM), along with other useful
notetaking features such as tags, LaTeX mathematical expressions, mermaid
diagrams, footnotes, internal links and embedding Obsidian notes or external
files.  Obsidian also has a plugin system to expand its capabilities.")
    (home-page "https://obsidian.md")
    (license (license:nonfree "https://obsidian.md/license"))))

(define-public google-antigravity
  (let ((antigravity-logo (origin
                            (method url-fetch)
                            (uri
                             "https://antigravity.google/assets/image/antigravity-logo.png")
                            (file-name "antigravity-logo.png")
                            (sha256 (base32
                                     "169766fbb91klrpaa6kk1a83wrq58pf2y3hh9l5r7gqxsb99a2wg")))))
    (package
      (name "google-antigravity")
      (version "2.19.1")
      (source
       (origin
         (method url-fetch)
         (uri (let* ((system (or (%current-target-system)
                                 (%current-system)))
                     (arch (match system
                             ("x86_64-linux" "linux-x64")
                             ("aarch64-linux" "linux-arm")
                             (_ "unsupported"))))
                (string-append
                 "https://storage.googleapis.com/antigravity-public/"
                 "antigravity-hub/"
                 version
                 "-6046815158665216/"
                 arch
                 "/Antigravity.tar.gz")))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 (match (or (%current-target-system)
                             (%current-system))
                    ("x86_64-linux"
                     "1bx4bza8qlcwsh9j6w7c12fq92vljws6hksfw8ji4niy3i3zls3h")
                    ("aarch64-linux"
                     "1a611f0f1d5h5rpjdi0v3hy073bj9mp1jphmlqqlmvnw4sgf6m7c")
                    (_ "0000000000000000000000000000000000000000000000000000"))))))
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (build-system chromium-binary-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:validate-runpath? #f ;TODO: fails on wrapped binaries and bundled node modules
        #:wrapper-plan
        #~(let ((rpath '(("out" "/share/antigravity"))))
            (map (lambda (file)
                   (list file rpath))
                 '("antigravity" "chrome-sandbox"
                   "chrome_crashpad_handler"
                   "libffmpeg.so"
                   "libvk_swiftshader.so"
                   "libvulkan.so.1"
                   "resources/bin/language_server"
                   "resources/bin/webm_encoder")))
        #:install-plan
        #~'(("." "/share/antigravity"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'binary-unpack 'disable-auto-updates
              (lambda _
                (delete-file "resources/app-update.yml")))
            (add-after 'disable-auto-updates 'disable-protocol-registration
              (lambda _
                ;; Guix installs the scheme handler through the desktop file.
                ;; Avoid Electron invoking xdg-mime during startup.  Patch the
                ;; asar payload in place with a same-length replacement so the
                ;; archive offsets remain valid.
                (use-modules (rnrs bytevectors)
                             (rnrs io ports))
                (let* ((file "resources/app.asar")
                       (needle (string->utf8
                                "electron_1.app.setAsDefaultProtocolClient(PROTOCOL);"))
                       (replacement (string->utf8
                                     "electron_1.app.isDefaultProtocolClient(PROTOCOL);   "))
                       (data (call-with-input-file file
                               get-bytevector-all
                               #:binary #t)))
                  (define (match-at? offset)
                    (let loop
                      ((index 0))
                      (or (= index
                             (bytevector-length needle))
                          (and (= (bytevector-u8-ref data
                                                     (+ offset index))
                                  (bytevector-u8-ref needle index))
                               (loop (+ index 1))))))
                  (define (find-needle)
                    (let ((limit (- (bytevector-length data)
                                    (bytevector-length needle))))
                      (let loop
                        ((offset 0))
                        (cond
                          ((> offset limit)
                           #f)
                          ((match-at? offset)
                           offset)
                          (else (loop (+ offset 1)))))))
                  (let ((offset (find-needle)))
                    (unless offset
                      (error
                       "Antigravity protocol registration hook not found"))
                    (bytevector-copy! replacement 0 data offset
                                      (bytevector-length replacement))
                    (call-with-output-file file
                      (lambda (port)
                        (put-bytevector port data))
                      #:binary #t)))
                #t))
            (add-after 'disable-protocol-registration 'patch-browser-launcher
              (lambda _
                (let ((launcher
                       "resources/app/extensions/antigravity-browser-launcher/dist/extension.js"))
                  ;; The browser launcher extension was removed/moved in newer
                  ;; upstream releases.
                  (when (file-exists? launcher)
                    (substitute* launcher
                      (("\"/opt/google/chrome/chrome\"")
                       "\"/opt/google/chrome/chrome\",\"/run/current-system/profile/bin/google-chrome\""))))
                #t))
            (add-after 'patch-browser-launcher 'patch-workbench-folder-picker
              (lambda _
                ;; The welcome screen's Open Folder action routes through the
                ;; workbench file-dialog service and can stall on Linux when it
                ;; tries to use the native picker path.  Force the simplified
                ;; in-app picker instead.
                (let ((workbench
                       "resources/app/out/vs/workbench/workbench.desktop.main.js"))
                  (when (file-exists? workbench)
                    (substitute* workbench
                      (("this\\.g\\.getValue\\(\"files\\.simpleDialog\\.enable\"\\)===!0")
                       "!0"))))
                #t))
            (add-before 'install-wrapper 'install-entrypoint
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (exe (string-append bin "/antigravity"))
                       (target (string-append #$output
                                "/share/antigravity/antigravity")))
                  (mkdir-p bin)
                  (with-output-to-file exe
                    (lambda _
                      (display "#!/bin/sh\n")
                      (display (string-append "exec \"" target "\" \"$@\"\n"))))
                  (chmod exe #o555))))
            (add-after 'install 'install-desktop-entry
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((applications (string-append #$output
                                                    "/share/applications"))
                       (icons (string-append #$output
                               "/share/icons/hicolor/256x256/apps"))
                       (logo #$antigravity-logo))
                  (mkdir-p applications)
                  (mkdir-p icons)
                  (copy-file logo
                             (string-append icons "/antigravity.png"))
                  (make-desktop-entry-file (string-append applications
                                            "/antigravity.desktop")
                                           #:name "Google Antigravity"
                                           #:type "Application"
                                           #:exec (string-append #$output
                                                   "/bin/antigravity %U")
                                           #:icon "antigravity"
                                           #:categories '("Development" "IDE")
                                           #:terminal #f
                                           #:startup-notify #t
                                           #:startup-w-m-class "Antigravity"
                                           #:mime-type
                                           "x-scheme-handler/antigravity"))))
            (add-after 'install-wrapper 'configure-runtime-wrapper
              (lambda _
                (substitute* (string-append #$output "/bin/.antigravity-real")
                  ;; Avoid Electron's portal-backed folder picker, which can
                  ;; leave the welcome screen's Open Folder action inert.
                  (("^exec .*$")
                   (string-append
                    "if [ -z \"${PLAYWRIGHT_NODEJS_PATH:-}\" ] && command -v node >/dev/null 2>&1; then"
                    "\n"
                    "  export PLAYWRIGHT_NODEJS_PATH=\"$(command -v node)\""
                    "\n"
                    "fi"
                    "\n"
                    "exec \""
                    #$output
                    "/share/antigravity/antigravity\""
                    " --xdg-portal-required-version=999 \"$@\""))) #t)))))
      (inputs (list node))
      (home-page "https://antigravity.google")
      (synopsis "AI-powered development environment")
      (description
       "Google Antigravity is an AI-powered development environment and code editor.")
      (license (license:nonfree "https://antigravity.google/terms")))))

(define-public chatgpt-desktop
  (package
    (name "chatgpt-desktop")
    (version "26.930.61225")
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
               "https://persistent.oaistatic.com/codex-app-prod/linux/deb/"
               "pool/main/c/chatgpt/chatgpt_"
               version
               "_"
               arch
               ".deb")))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "1f2vd7g01mclw208h71p9b2s752piqm50sc4bdd2mh9v6pwq02mr")
                  ("aarch64-linux"
                   "1gfn8s1yhpxgphspv0c777nl0qv29xyqh685vfyp4y9sl524f6sl")
                  (_ "0000000000000000000000000000000000000000000000000000"))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~374 MiB for x86_64.
      #:substitutable? #f
      #:validate-runpath? #f ;TODO: fails on bundled node modules and wrapped binary
      #:wrapper-plan
      #~(let ((rpath '(("out" "/lib/chatgpt")
                       ("nss" "/lib/nss"))))
          (map (lambda (file)
                 (list file rpath))
               '("usr/lib/chatgpt/ChatGPT"
                 "usr/lib/chatgpt/browser_crashpad_handler"
                 "usr/lib/chatgpt/libEGL.so"
                 "usr/lib/chatgpt/libGLESv2.so"
                 "usr/lib/chatgpt/libqt5_shim.so"
                 "usr/lib/chatgpt/libqt6_shim.so"
                 "usr/lib/chatgpt/libvk_swiftshader.so"
                 "usr/lib/chatgpt/libvulkan.so.1")))
      #:install-plan
      #~'(("usr/lib/" "/lib")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patchelf 'patch-cua-node-interpreter
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((interpreter
                     (car (find-files (assoc-ref inputs "libc")
                                      "ld-linux.*\\.so")))
                    (node "usr/lib/chatgpt/resources/cua_node/bin/node"))
                (invoke "patchelf" "--set-interpreter" interpreter node))))
          (add-before 'install 'patch-desktop-entry
            (lambda _
              (substitute* "usr/share/applications/chatgpt.desktop"
                (("^Exec=chatgpt %U")
                 (string-append "Exec=" #$output "/bin/chatgpt %U")))
              #t))
          (add-after 'install 'patch-native-node-modules
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define system
                (or #$(%current-target-system)
                    #$(%current-system)))
              (define machine-token
                (cond
                 ((string=? system "x86_64-linux") "x86-64")
                 ((string=? system "aarch64-linux") "ARM aarch64")
                 (else "")))
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
              (define runtime-rpath
                (string-join (input-library-directories) ":"))
              (define (file-description file)
                (string-trim-right
                 (with-output-to-string
                   (lambda _
                     (invoke "file" "-b" file)))
                 #\newline))
              (define (native-module-for-system? file)
                (let ((description (file-description file)))
                  (and (string-contains description "ELF")
                       (string-contains description machine-token)
                       (not (string-contains file "musl")))))
              (define (patch-rpath file)
                (let* ((current-rpath
                        (string-trim-right
                         (with-output-to-string
                           (lambda _
                             (invoke "patchelf" "--print-rpath" file)))
                         #\newline))
                       (new-rpath
                        (if (string-null? current-rpath)
                            runtime-rpath
                            (string-append current-rpath ":" runtime-rpath))))
                  (invoke "patchelf" "--set-rpath" new-rpath file)))
              (for-each
               (lambda (file)
                 (when (native-module-for-system? file)
                   (patch-rpath file)))
               (find-files (string-append (assoc-ref outputs "out")
                                          "/lib/chatgpt")
                           "\\.node$"))))
          (add-before 'install-wrapper 'install-entrypoint
            (lambda* (#:key inputs #:allow-other-keys)
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
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/chatgpt"))
                     (resources (string-append #$output
                                  "/lib/chatgpt/resources"))
                     (codex (string-append resources "/codex"))
                     (node (string-append resources "/cua_node/bin/node"))
                     (node-wrapper (string-append resources
                                                  "/cua_node/bin/node-guix"))
                     (node-repl (string-append resources
                                               "/cua_node/bin/node_repl"))
                     (plugins (string-append resources
                               "/plugins/openai-bundled"))
                     (runtime-bin (string-append resources "/guix-bin"))
                     (target (string-append #$output
                              "/lib/chatgpt/codex-launcher"))
                     (bash (string-append (assoc-ref inputs "bash-minimal")
                                          "/bin/bash"))
                     (cat* #$(file-append coreutils "/bin/cat"))
                     (chmod* #$(file-append coreutils "/bin/chmod"))
                     (cp* #$(file-append coreutils "/bin/cp"))
                     (git-bin (string-append (assoc-ref inputs "git-minimal")
                                             "/bin"))
                     (ln* #$(file-append coreutils "/bin/ln"))
                     (mkdir* #$(file-append coreutils "/bin/mkdir"))
                     (mktemp* #$(file-append coreutils "/bin/mktemp"))
                     (mv* #$(file-append coreutils "/bin/mv"))
                     (rm* #$(file-append coreutils "/bin/rm"))
                     (sh (string-append (assoc-ref inputs "bash-minimal")
                                        "/bin/sh"))
                     (python-bin (string-append
                                  (assoc-ref inputs "python-wrapper")
                                  "/bin"))
                     (runtime-library-path
                      (string-join (input-library-directories) ":")))
                (with-output-to-file node-wrapper
                  (lambda _
                    (display "#!")
                    (display sh)
                    (display "\n")
                    (display "export LD_LIBRARY_PATH=\"")
                    (display runtime-library-path)
                    (display "${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"\n")
                    (display "exec \"")
                    (display node)
                    (display "\" \"$@\"\n")))
                (chmod node-wrapper #o555)
                (mkdir-p runtime-bin)
                (symlink node-wrapper (string-append runtime-bin "/node"))
                (symlink (string-append python-bin "/python")
                         (string-append runtime-bin "/python"))
                (symlink (string-append python-bin "/python3")
                         (string-append runtime-bin "/python3"))
                (setenv "PATH"
                        (string-append runtime-bin ":" python-bin ":"
                                       (getenv "PATH")))
                (mkdir-p bin)
                (with-output-to-file exe
                  (lambda _
                    (display "#!/bin/sh\n")
                    (display "cat_cmd=\"")
                    (display cat*)
                    (display "\"\n")
                    (display "chmod_cmd=\"")
                    (display chmod*)
                    (display "\"\n")
                    (display "cp_cmd=\"")
                    (display cp*)
                    (display "\"\n")
                    (display "ln_cmd=\"")
                    (display ln*)
                    (display "\"\n")
                    (display "mkdir_cmd=\"")
                    (display mkdir*)
                    (display "\"\n")
                    (display "mktemp_cmd=\"")
                    (display mktemp*)
                    (display "\"\n")
                    (display "mv_cmd=\"")
                    (display mv*)
                    (display "\"\n")
                    (display "rm_cmd=\"")
                    (display rm*)
                    (display "\"\n")
                    (display "if [ -n \"${XDG_CACHE_HOME:-}\" ]; then\n")
                    (display "  cache_home=$XDG_CACHE_HOME\n")
                    (display "elif [ -n \"${HOME:-}\" ]; then\n")
                    (display "  cache_home=$HOME/.cache\n")
                    (display "else\n")
                    (display "  cache_home=\n")
                    (display "fi\n")
                    (display "guix_profile_path=\n")
                    (display "if [ -n \"${HOME:-}\" ]; then\n")
                    (display "  guix_profile_path=$HOME/.guix-home/profile/bin:$HOME/.guix-home/profile/sbin:$HOME/.config/guix/current/bin:$HOME/.guix-profile/bin\n")
                    (display "fi\n")
                    (display "system_profile_path=/run/privileged/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin\n")
                    (display "package_path=")
                    (display runtime-bin)
                    (display ":")
                    (display python-bin)
                    (display ":")
                    (display git-bin)
                    (display ":")
                    (display (string-append (assoc-ref inputs "bash-minimal")
                                            "/bin"))
                    (display "\n")
                    (display "export PATH=$package_path${guix_profile_path:+:$guix_profile_path}:$system_profile_path${PATH:+:$PATH}\n")
                    (display "export SHELL=\"")
                    (display bash)
                    (display "\"\n")
                    (display "if [ -n \"$cache_home\" ]; then\n")
                    (display "  resources_source=\"")
                    (display resources)
                    (display "\"\n")
                    (display "  bundled_source=\"")
                    (display plugins)
                    (display "\"\n")
                    (display "  resources_copy=$cache_home/chatgpt-desktop/bundled-plugin-resources\n")
                    (display "  bundled_target=$resources_copy/plugins/openai-bundled\n")
                    (display "  \"$mkdir_cmd\" -p \"$resources_copy\"\n")
                    (display "  for resource_name in accessibility app.asar app.asar.unpacked artifact-template-picker codex codex-classic.wav codex-code-mode-host codex-notification.wav default_app icon-chatgpt.png linux-package-metadata.json native owl-app.ini owl-electron-app.json rg skills tectonic; do\n")
                    (display "    resource_source=$resources_source/$resource_name\n")
                    (display "    resource_target=$resources_copy/$resource_name\n")
                    (display "    if [ -e \"$resource_source\" ] || [ -L \"$resource_source\" ]; then\n")
                    (display "      \"$rm_cmd\" -rf \"$resource_target\"\n")
                    (display "      \"$ln_cmd\" -s \"$resource_source\" \"$resource_target\"\n")
                    (display "    fi\n")
                    (display "  done\n")
                    (display "  \"$rm_cmd\" -rf \"$resources_copy/cua_node\"\n")
                    (display "  \"$mkdir_cmd\" -p \"$resources_copy/cua_node/bin\"\n")
                    (display "  for cua_resource in LICENSE manifest.json lib; do\n")
                    (display "    if [ -e \"$resources_source/cua_node/$cua_resource\" ] || [ -L \"$resources_source/cua_node/$cua_resource\" ]; then\n")
                    (display "      \"$ln_cmd\" -s \"$resources_source/cua_node/$cua_resource\" \"$resources_copy/cua_node/$cua_resource\"\n")
                    (display "    fi\n")
                    (display "  done\n")
                    (display "  for cua_bin in corepack node-guix node_repl setup.ps1 setup.sh; do\n")
                    (display "    if [ -e \"$resources_source/cua_node/bin/$cua_bin\" ] || [ -L \"$resources_source/cua_node/bin/$cua_bin\" ]; then\n")
                    (display "      \"$ln_cmd\" -s \"$resources_source/cua_node/bin/$cua_bin\" \"$resources_copy/cua_node/bin/$cua_bin\"\n")
                    (display "    fi\n")
                    (display "  done\n")
                    (display "  \"$ln_cmd\" -s \"")
                    (display node-wrapper)
                    (display "\" \"$resources_copy/cua_node/bin/node\"\n")
                    (display "  if [ -d \"$bundled_source\" ]; then\n")
                    (display "    source_id=$(\"$cat_cmd\" \"$bundled_source/.bundle-id\" 2>/dev/null || true)\n")
                    (display "    target_id=$(\"$cat_cmd\" \"$bundled_target/.bundle-id\" 2>/dev/null || true)\n")
                    (display "    if [ \"$source_id\" != \"$target_id\" ]; then\n")
                    (display "      \"$mkdir_cmd\" -p \"$resources_copy/plugins\"\n")
                    (display "      staging=$(\"$mktemp_cmd\" -d \"$bundled_target.staging.XXXXXX\") || exit 1\n")
                    (display "      if \"$cp_cmd\" -R --no-preserve=mode,ownership \"$bundled_source/.\" \"$staging/\"; then\n")
                    (display "        \"$chmod_cmd\" -R u+rwX \"$staging\" 2>/dev/null || true\n")
                    (display "        \"$rm_cmd\" -rf \"$bundled_target\"\n")
                    (display "        \"$mv_cmd\" \"$staging\" \"$bundled_target\"\n")
                    (display "      else\n")
                    (display "        \"$rm_cmd\" -rf \"$staging\"\n")
                    (display "      fi\n")
                    (display "    fi\n")
                    (display "    if [ -d \"$bundled_target\" ]; then\n")
                    (display "      export CODEX_ELECTRON_BUNDLED_PLUGINS_RESOURCES_PATH=$resources_copy\n")
                    (display "    fi\n")
                    (display "  fi\n")
                    (display "fi\n")
                    (display ": \"${CODEX_ELECTRON_RESOURCES_PATH:=")
                    (display resources)
                    (display "}\"\n")
                    (display "export CODEX_ELECTRON_RESOURCES_PATH\n")
                    (display ": \"${CODEX_CLI_PATH:=")
                    (display codex)
                    (display "}\"\n")
                    (display "export CODEX_CLI_PATH\n")
                    (display ": \"${CODEX_BROWSER_USE_NODE_PATH:=")
                    (display node-wrapper)
                    (display "}\"\n")
                    (display "export CODEX_BROWSER_USE_NODE_PATH\n")
                    (display ": \"${CODEX_NODE_REPL_PATH:=")
                    (display node-repl)
                    (display "}\"\n")
                    (display "export CODEX_NODE_REPL_PATH\n")
                    (display (string-append "exec \"" target "\" \"$@\"\n"))))
                (chmod exe #o555))))
          (add-after 'install-entrypoint 'patch-installed-node-shebangs
            (lambda* (#:key outputs #:allow-other-keys)
              (define read-line (@ (ice-9 rdelim) read-line))
              (define node-wrapper
                (string-append (assoc-ref outputs "out")
                               "/lib/chatgpt/resources/cua_node/bin/node-guix"))
              (define (starts-with? prefix string)
                (and (>= (string-length string) (string-length prefix))
                     (string=? prefix
                               (substring string 0 (string-length prefix)))))
              (define (node-shebang? line)
                (and (string? line)
                     (or (starts-with? "#!/usr/bin/env node" line)
                         (starts-with? "#! /usr/bin/env node" line)
                         (starts-with? "#!/usr/bin/env -S node" line)
                         (starts-with? "#! /usr/bin/env -S node" line)
                         (starts-with? "#!/usr/bin/node" line)
                         (starts-with? "#! /usr/bin/node" line)
                         (starts-with? "#!/bin/node" line)
                         (starts-with? "#! /bin/node" line))))
              (define (first-line file)
                (catch #t
                  (lambda _
                    (call-with-input-file file read-line))
                  (lambda _
                    #f)))
              (for-each
               (lambda (file)
                 (when (node-shebang? (first-line file))
                   (substitute* file
                     (("^#! */usr/bin/env node(.*)$" _ arguments)
                      (string-append "#!" node-wrapper arguments))
                     (("^#! */usr/bin/env -S node(.*)$" _ arguments)
                      (string-append "#!" node-wrapper arguments))
                     (("^#! */usr/bin/node(.*)$" _ arguments)
                      (string-append "#!" node-wrapper arguments))
                     (("^#! */bin/node(.*)$" _ arguments)
                      (string-append "#!" node-wrapper arguments)))))
               (find-files (string-append (assoc-ref outputs "out")
                                          "/lib/chatgpt"))))))))
    (inputs (list git-minimal libusb openssl python-wrapper tpm2-tss))
    (home-page "https://developers.openai.com/codex/app")
    (synopsis "OpenAI ChatGPT desktop app with Codex integration")
    (description
     "ChatGPT Desktop is OpenAI's Electron-based desktop application for
ChatGPT, including local Codex integration for software development workflows.")
    (license (license:nonfree "https://openai.com/policies/terms-of-use"))))

(define-public zotero
  (package
    (name "zotero")
    (version "9.0.1")
    (source
     (origin
       ;; Can switch to git-fetch from Github too!
       (method url-fetch)
       (uri (string-append "https://download.zotero.org/client/release/"
                           version "/Zotero-" version "_linux-x86_64.tar.xz"))
       (sha256
        (base32 "1m2yg4r5ipx8sm6cf6i43cz0c3xvrsaf2ip1hasc5ykh8as7pw5q"))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    ;; Disable Zotero's automatic update feature.
                    (let ((prefs (cond
                                  ((file-exists? "defaults/preferences/prefs.js")
                                   "defaults/preferences/prefs.js")
                                  ((file-exists?
                                    "Zotero_linux-x86_64/defaults/preferences/prefs.js")
                                   "Zotero_linux-x86_64/defaults/preferences/prefs.js")
                                  (else #f))))
                      (if prefs
                          (substitute* prefs
                            (("pref\\(\"app.update.enabled\", true\\)")
                             "pref(\"app.update.enabled\", false)")
                            (("pref\\(\"app.update.auto\", true\\)")
                             "pref(\"app.update.auto\", false)"))
                          (let* ((root (if (file-exists? "Zotero_linux-x86_64")
                                           "Zotero_linux-x86_64"
                                           "."))
                                 (pref-dir (string-append root "/defaults/pref"))
                                 (pref-file (string-append pref-dir
                                                           "/guix-updates.js")))
                            (mkdir-p pref-dir)
                            (call-with-output-file pref-file
                              (lambda (port)
                                (display "pref(\"app.update.enabled\", false);\n"
                                         port)
                                (display "pref(\"app.update.auto\", false);\n"
                                         port))))))))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~70 MiB
      #:substitutable? #f
      #:validate-runpath? #t
      #:wrapper-plan
      #~'("zotero-bin")
      #:phases
      #~(modify-phases %standard-phases
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
                                       #:generic-name "Reference Management"
                                       #:exec (string-append #$output
                                               "/bin/zotero -url %U")
                                       #:icon "zotero"
                                       #:keywords '("zotero")
                                       #:categories '("Office" "Database")
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
              (let* ((icon-map (if (file-exists?
                                    "chrome/icons/default/default16.png")
                                   '(("16" . "chrome/icons/default/default16.png")
                                     ("32" . "chrome/icons/default/default32.png")
                                     ("48" . "chrome/icons/default/default48.png")
                                     ("256" . "chrome/icons/default/default256.png"))
                                   '(("16" . "icons/icon32.png")
                                     ("32" . "icons/icon32.png")
                                     ("48" . "icons/icon64.png")
                                     ("64" . "icons/icon64.png")
                                     ("128" . "icons/icon128.png")))))
                (for-each (lambda (entry)
                            (let* ((size (car entry))
                                   (source (cdr entry))
                                   (destination-directory
                                    (string-append #$output
                                                   "/share/icons/hicolor/"
                                                   size
                                                   "x"
                                                   size
                                                   "/apps")))
                              (mkdir-p destination-directory)
                              (copy-file source
                                         (string-append destination-directory
                                                        "/zotero.png"))))
                          icon-map)
                (when (file-exists? "icons/symbolic.svg")
                  (let ((destination-directory
                         (string-append #$output
                                        "/share/icons/hicolor/scalable/apps")))
                    (mkdir-p destination-directory)
                    (copy-file "icons/symbolic.svg"
                               (string-append destination-directory
                                              "/zotero-symbolic.svg"))))))))))
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
