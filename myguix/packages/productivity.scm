;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Karl Hallsby <karl@hallsby.com

(define-module (myguix packages productivity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
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
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (myguix build-system chromium-binary)
  #:use-module (myguix build-system binary)
  #:use-module ((gnu packages admin)
                #:select (libcap-ng))
  #:use-module ((gnu packages firmware)
                #:select (ovmf-x86-64))
  #:use-module ((myguix packages node-pqrs)
                #:select (node-anthropic-ai-claude-code))
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module ((guix licenses)
                #:prefix free-license:))

(define-public anytype
  (package
    (name "anytype")
    (version "0.57.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://anytype-release.fra1.cdn.digitaloceanspaces.com/" name
             "_" version "_amd64.deb"))
       (file-name (string-append "anytype-" version ".deb"))
       (sha256
        (base32 "131sp1zgfjaq5j8h362rz9pffk0r3xg0j2r3i0an9ws6kk69az7x"))))
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
               "resources/app.asar.unpacked/node_modules/keytar/build/Release/keytar.node"))
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
              (let ((python (string-append
                             "opt/Anytype/resources/app.asar.unpacked/"
                             "node_modules/keytar/build/node_gyp_bins/python3")))
                (when (file-exists? python)
                  (delete-file python)))))
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
    (version "1.14.4")
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
                   "080ir1nbgscqzgl15xnkgf3r2ss4bhzlbv2gm2cggrw76zb7q2z7")
                  ("aarch64-linux"
                   "0bd4wbp1v5qlri7vxi27330bqwlq015vrxayzs2ys4spa422h8gw")
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
                                     "0wzdxxdzw2hhcrvny3rmay7711m8l0mgj7bs1k6j6771bn0a2fqr")))))
    (package
      (name "google-antigravity")
      (version "2.24.0")
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
                 "-5780333019398144/"
                 arch
                 "/Antigravity.tar.gz")))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 (match (or (%current-target-system)
                             (%current-system))
                    ("x86_64-linux"
                     "1inwmhb84k62nwjgrnd0bx5g0x3lydpydqa2a4vd3i9k01cby248")
                    ("aarch64-linux"
                     "1c0af7p9h6qkqvg7cjxhsbq7jf3gxa0z928iy029apc8ryaldbjz")
                    (_ "0000000000000000000000000000000000000000000000000000"))))))
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (build-system chromium-binary-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:validate-runpath? #f ;TODO: fails on wrapped binaries and bundled node modules
        #:wrapper-plan
        ;; The application runs Node.js sidecars by invoking its own binary
        ;; with ELECTRON_RUN_AS_NODE, bypassing the wrapper, so the binary
        ;; must find NSS through its RUNPATH.
        #~(let ((rpath '(("out" "/share/antigravity")
                         ("nss" "/lib/nss"))))
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
            (add-after 'disable-auto-updates 'patch-app-asar
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (define get-string-all
                  (@ (ice-9 textual-ports) get-string-all))
                (define (ensure-patched file text)
                  (unless (string-contains
                           (call-with-input-file file get-string-all) text)
                    (error "failed to patch app.asar:" file)))
                (let ((python (search-input-file (or native-inputs inputs)
                                                 "/bin/python3"))
                      (asar-tool #$(local-file "aux-files/asar.py"))
                      (asar "resources/app.asar")
                      (tree "app-asar"))
                  (invoke python asar-tool "extract" asar tree)
                  (with-directory-excursion (string-append tree "/dist")
                    ;; Guix installs the scheme handler through the desktop
                    ;; file; avoid Electron invoking xdg-mime at startup.
                    (substitute* "main.js"
                      (("app\\.setAsDefaultProtocolClient\\(PROTOCOL\\);")
                       "app.isDefaultProtocolClient(PROTOCOL);"))
                    (ensure-patched "main.js"
                                    "app.isDefaultProtocolClient(PROTOCOL);\n")
                    ;; Guix manages updates; electron-updater only warns
                    ;; hourly that this is not an AppImage.
                    (substitute* "updater.js"
                      (("^function startAutoUpdateChecks\\(\\) \\{" all)
                       (string-append all "\n    return;")))
                    (ensure-patched "updater.js" "{\n    return;")
                    ;; The IDE installer unpacks a binary that cannot run on
                    ;; Guix System; never offer it.
                    (substitute* "ideInstall/constants.js"
                      (("^async function shouldShowIdeInstallWizard\\(.*\\{"
                        all)
                       (string-append all "\n    return false;")))
                    (ensure-patched "ideInstall/constants.js"
                                    "{\n    return false;"))
                  (invoke python asar-tool "pack" asar tree)
                  (delete-file-recursively tree))))
            (add-after 'patch-app-asar 'patch-chrome-locations
              (lambda _
                ;; Let the Chrome DevTools MCP server find Chrome in Guix
                ;; profiles as well as under /opt/google.
                (define (locations name)
                  (string-append
                   ",...[process.env.HOME+'/.guix-home/profile/bin',"
                   "process.env.HOME+'/.guix-profile/bin',"
                   "'/run/current-system/profile/bin']"
                   ".map(directory=>directory+'/" name "')"))
                (let ((file (string-append
                             "resources/app.asar.unpacked/node_modules/"
                             "chrome-devtools-mcp/build/src/third_party/"
                             "index.js")))
                  (substitute* file
                    (("'/opt/google/chrome/chrome'" all)
                     (string-append all (locations "google-chrome")))
                    (("'/opt/google/chrome-beta/chrome'" all)
                     (string-append all (locations "google-chrome-beta")))
                    (("'/opt/google/chrome-unstable/chrome'" all)
                     (string-append all
                                    (locations "google-chrome-unstable"))))
                  (unless (string-contains
                           (call-with-input-file file
                             (@ (ice-9 textual-ports) get-string-all))
                           "/.guix-profile/bin")
                    (error "failed to patch Chrome locations:" file)))))
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
                    "/share/antigravity/antigravity\" \"$@\""))) #t)))))
      (native-inputs (list python-minimal))
      (inputs (list node))
      (home-page "https://antigravity.google")
      (synopsis "AI-powered development environment")
      (description
       "Google Antigravity is an AI-powered development environment and code editor.")
      (license (license:nonfree "https://antigravity.google/terms")))))

(define-public chatgpt-desktop
  (package
    (name "chatgpt-desktop")
    (version "26.1013.40934")
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
                   "1mx1ydfvw3ia47xihx7z4s0apdc46xl81mlszgibjhb6k9zrx99w")
                  ("aarch64-linux"
                   "16v23kryb506wh0bz5qdga96z4r8h6vwmdbfw2hymsrwyfxcpcls")
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
              (let ((interpreter
                     (car (find-files (assoc-ref inputs "libc")
                                      "ld-linux.*\\.so")))
                    (rpath (string-join (input-library-directories) ":"))
                    (node "usr/lib/chatgpt/resources/cua_node/bin/node"))
                (invoke "patchelf" "--set-interpreter" interpreter node)
                (invoke "patchelf" "--set-rpath" rpath node))))
          (add-before 'install 'patch-desktop-entry
            (lambda _
              (substitute* "usr/share/applications/chatgpt.desktop"
                (("^Exec=chatgpt %U")
                 (string-append "Exec=" #$output "/bin/chatgpt %U")))
              #t))
          (add-after 'install 'patch-app-asar
            (lambda* (#:key inputs #:allow-other-keys)
              (define any (@ (srfi srfi-1) any))
              (define get-string-all (@ (ice-9 textual-ports) get-string-all))
              (define (contains? needle)
                (lambda (file)
                  (string-contains (call-with-input-file file get-string-all)
                                   needle)))
              (define (ensure-patched files old new)
                (when (or (null? files)
                          (and old (any (contains? old) files))
                          (not (any (contains? new) files)))
                  (error "failed to patch app.asar:" new)))
              (let* ((python (search-input-file inputs "/bin/python3"))
                     (asar-tool #$(local-file "aux-files/asar.py"))
                     (lib (string-append #$output "/lib/chatgpt"))
                     (asar (string-append lib "/resources/app.asar"))
                     (node (string-append lib "/resources/cua_node/bin/node"))
                     (tree "app-asar")
                     ;; The function resolving paths into the downloaded
                     ;; primary runtime, after it has read runtime.json.
                     (runtime-resolver
                      (string-append
                       "(async function [A-Za-z_$]+\\(\\{bundleFormatVersion:"
                       "[a-z],runtimeRoot:([a-z]),targetPlatform:[a-z]\\}\\)"
                       "\\{let ([a-z])=await [A-Za-z_$]+\\([a-z]\\)),"))
                     ;; Run the runtime fixup, once per bundle version, before
                     ;; resolving those paths.
                     (hook
                      (string-append
                       "(globalThis.__guixFixupRuntime??=((runs=new Map)=>"
                       "(root,metadata)=>{let key=root+\":\"+"
                       "(metadata?.bundleVersion??\"\"),run=runs.get(key);"
                       "if(run==null){run=new Promise(resolve=>"
                       "require(\"node:child_process\").execFile(\""
                       lib "/guix-runtime-fixup\",[root],{timeout:9e5},"
                       "(error,stdout,stderr)=>{if(error!=null){"
                       "runs.delete(key);console.error("
                       "\"Guix runtime fixup failed:\",error.message,stderr)}"
                       "resolve()}));runs.set(key,run)}return run})())")))
                (invoke python asar-tool "extract" asar tree)
                (let ((startup (find-files tree
                                           "^application-network-startup-.*\\.js$"))
                      (sources (find-files tree "^src-.*\\.js$"))
                      (hosts (find-files (string-append tree "/webview")
                                         "^host-.*\\.js$")))
                  (substitute* startup
                    ((runtime-resolver _ head root metadata)
                     (string-append head ";await(" hook ")(" root ","
                                    metadata ");let ")))
                  (ensure-patched startup #f "__guixFixupRuntime")
                  ;; Guix System has no /bin/cat, /usr/bin/head or
                  ;; /usr/bin/git; let PATH, local or remote, resolve them.
                  (substitute* sources
                    (("\\[`/bin/cat`,") "[`cat`,"))
                  (ensure-patched sources "`/bin/cat`" "[`cat`,")
                  (substitute* hosts
                    (("\\[`/usr/bin/head`,") "[`head`,")
                    ((":`/usr/bin/git`") ":`git`"))
                  (ensure-patched hosts "`/usr/bin/head`" "[`head`,")
                  (ensure-patched hosts "`/usr/bin/git`" ":`git`")
                  (for-each (lambda (file)
                              (invoke node "--check" file))
                            (append startup sources hosts)))
                (invoke python asar-tool "pack" asar tree)
                (delete-file-recursively tree))))
          (add-after 'strip 'patch-native-node-modules
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim)
                           (rnrs bytevectors)
                           (rnrs io ports))
              (define system
                (or #$(%current-target-system)
                    #$(%current-system)))
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
              (define (elf-file? file)
                (catch #t
                  (lambda _
                    (call-with-input-file file
                      (lambda (port)
                        (let ((magic (get-bytevector-n port 4)))
                          (and (= (bytevector-length magic) 4)
                               (= (bytevector-u8-ref magic 0) #x7f)
                               (= (bytevector-u8-ref magic 1) (char->integer #\E))
                               (= (bytevector-u8-ref magic 2) (char->integer #\L))
                               (= (bytevector-u8-ref magic 3) (char->integer #\F)))))
                      #:binary #t))
                  (lambda _
                    #f)))
              (define (foreign-node-module? file)
                (or (string-contains file "darwin")
                    (string-contains file "win32")
                    (string-contains file "android")
                    (string-contains file "musl")
                    (and (string=? system "x86_64-linux")
                         (or (string-contains file "linux-arm")
                             (string-contains file "arm64")
                             (string-contains file "armv")))
                    (and (string=? system "aarch64-linux")
                         (or (string-contains file "linux-x64")
                             (string-contains file "x64")
                             (string-contains file "x86")))))
              (define (native-module-for-system? file)
                (and (elf-file? file)
                     (not (foreign-node-module? file))))
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
                (let* ((old-rpath (current-rpath file))
                       (new-rpath
                        (if (string-null? old-rpath)
                            runtime-rpath
                            (string-append old-rpath ":" runtime-rpath))))
                  (invoke "patchelf" "--set-rpath" new-rpath file)))
              (for-each
               (lambda (file)
                 (when (native-module-for-system? file)
                   (format #t "Patching native Node module: ~a~%" file)
                   (patch-rpath file)))
               (find-files (string-append (assoc-ref outputs "out")
                                          "/lib/chatgpt")
                           "\\.node$"))))
          (add-after 'patch-native-node-modules 'patch-helper-executables
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim))
              (define system
                (or #$(%current-target-system)
                    #$(%current-system)))
              (define interpreter
                (car (find-files (assoc-ref inputs "libc")
                                 "ld-linux.*\\.so")))
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
              (define (patch-executable file)
                (when (file-exists? file)
                  (let* ((old-rpath (current-rpath file))
                         (new-rpath
                          (if (string-null? old-rpath)
                              runtime-rpath
                              (string-append old-rpath ":" runtime-rpath))))
                    (format #t "Patching helper executable: ~a~%" file)
                    (invoke "patchelf" "--set-interpreter" interpreter file)
                    (invoke "patchelf" "--set-rpath" new-rpath file))))
              (define helper-paths
                (cond
                 ((string=? system "x86_64-linux")
                  '("resources/cua_node/bin/node"
                    "resources/cua_node/lib/node_modules/.bin/sky_linux_x64"
                    "resources/cua_node/lib/node_modules/@oai/cua/bin/linux/sky_linux_x64"
                    "resources/cua_node/lib/node_modules/@oai/sky/bin/linux/sky_linux_x64"
                    "resources/plugins/openai-bundled/plugins/chrome/extension-host/linux/x64/extension-host"))
                 ((string=? system "aarch64-linux")
                  '("resources/cua_node/bin/node"
                    "resources/cua_node/lib/node_modules/.bin/sky_linux_arm64"
                    "resources/cua_node/lib/node_modules/@oai/cua/bin/linux/sky_linux_arm64"
                    "resources/cua_node/lib/node_modules/@oai/sky/bin/linux/sky_linux_arm64"
                    "resources/plugins/openai-bundled/plugins/chrome/extension-host/linux/arm64/extension-host"))
                 (else '())))
              (for-each
               (lambda (path)
                 (patch-executable
                  (string-append (assoc-ref outputs "out")
                                 "/lib/chatgpt/"
                                 path)))
               helper-paths)))
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
              (define (library-directory file)
                (dirname (search-input-file inputs file)))
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/chatgpt"))
                     (lib (string-append #$output "/lib/chatgpt"))
                     (resources (string-append lib "/resources"))
                     (chatgpt-binary (string-append lib "/ChatGPT"))
                     (node (string-append resources "/cua_node/bin/node"))
                     (node-wrapper (string-append resources
                                                  "/cua_node/bin/node-guix"))
                     (runtime-bin (string-append resources "/guix-bin"))
                     (target (string-append lib "/codex-launcher"))
                     (fixup (string-append lib "/guix-runtime-fixup"))
                     (bash-bin (string-append (assoc-ref inputs "bash-minimal")
                                              "/bin"))
                     (sh (string-append bash-bin "/sh"))
                     (git-bin (string-append (assoc-ref inputs "git-minimal")
                                             "/bin"))
                     (python-bin (string-append
                                  (assoc-ref inputs "python-wrapper")
                                  "/bin"))
                     (interpreter
                      (car (find-files (assoc-ref inputs "libc")
                                       "ld-linux.*\\.so")))
                     (runtime-library-path
                      (string-join (input-library-directories) ":"))
                     ;; Host libraries, beyond glibc, needed by the primary
                     ;; runtime that ChatGPT downloads at run time.
                     (primary-runtime-library-path
                      (string-join
                       (cons* #$(file-append curl "/lib")
                              #$(file-append lcms "/lib")
                              #$(file-append libxcrypt "/lib")
                              #$(file-append pixman "/lib")
                              (map library-directory
                                   '("lib/libstdc++.so.6"
                                     "lib/libz.so.1"
                                     "lib/libexpat.so.1"
                                     "lib/libfontconfig.so.1"
                                     "lib/libfreetype.so.6"
                                     "lib/nss/libnss3.so"
                                     "lib/libnspr4.so")))
                       ":")))
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
                (with-output-to-file target
                  (lambda _
                    (display "#!")
                    (display sh)
                    (display "\n")
                    ;; Upstream uses XWayland while native Wayland is experimental.
                    ;; The bundled Chromium always uses the XDG FileChooser
                    ;; portal; it no longer has --xdg-portal-required-version.
                    (display "exec \"")
                    (display chatgpt-binary)
                    (display "\" --ozone-platform=x11 \"$@\"\n")))
                (chmod target #o555)
                (mkdir-p runtime-bin)
                (symlink node-wrapper (string-append runtime-bin "/node"))
                (symlink (string-append python-bin "/python")
                         (string-append runtime-bin "/python"))
                ;; python-wrapper provides "python" only.
                (symlink (string-append python-bin "/python")
                         (string-append runtime-bin "/python3"))
                (setenv "PATH"
                        (string-append runtime-bin ":" python-bin ":"
                                       (getenv "PATH")))
                (copy-file #$(local-file
                              "aux-files/chatgpt-desktop/runtime-fixup.py")
                           fixup)
                (substitute* fixup
                  (("@PYTHON@")
                   (string-append python-bin "/python"))
                  (("@PATCHELF@")
                   #$(file-append patchelf-0.16 "/bin/patchelf"))
                  (("@INTERPRETER@") interpreter)
                  (("@LIBRARY_PATH@") primary-runtime-library-path)
                  (("@FIXUP_ID@") #$output))
                (chmod fixup #o555)
                (mkdir-p bin)
                (copy-file #$(local-file "aux-files/chatgpt-desktop/chatgpt.in")
                           exe)
                (substitute* exe
                  (("@SH@") sh)
                  (("@OUT@") #$output)
                  (("@COREUTILS@") #$coreutils)
                  (("@PACKAGE_PATH@")
                   (string-join (list runtime-bin python-bin git-bin bash-bin)
                                ":"))
                  (("@BASH@") (string-append bash-bin "/bash")))
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
                                          "/lib/chatgpt")))))
          (add-after 'patch-installed-node-shebangs 'patch-bundled-mcp-shells
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((node-wrapper
                     (string-append (assoc-ref outputs "out")
                                    "/lib/chatgpt/resources/cua_node/bin/node-guix"))
                    (sh (string-append (assoc-ref inputs "bash-minimal")
                                       "/bin/sh")))
                (for-each
                 (lambda (file)
                   (substitute* file
                     (("\"command\": \"/bin/sh\"")
                      (string-append "\"command\": \"" sh "\""))
                     (("\"command\": \"node\"")
                      (string-append "\"command\": \"" node-wrapper "\""))))
                 (find-files (string-append (assoc-ref outputs "out")
                                            "/lib/chatgpt")
                             "\\.mcp\\.json$"))))))))
    (inputs (list git-minimal libusb openssl python-wrapper tpm2-tss))
    (home-page "https://developers.openai.com/codex/app")
    (synopsis "OpenAI ChatGPT desktop app with Codex integration")
    (description
     "ChatGPT Desktop is OpenAI's Electron-based desktop application for
ChatGPT, including local Codex integration for software development workflows.")
    (license (license:nonfree "https://openai.com/policies/terms-of-use"))))

(define-public claude-desktop
  (package
    (name "claude-desktop")
    (version "2.42304.0")
    (source
     (origin
       (method url-fetch)
       (uri (let ((arch (match (or (%current-target-system)
                                   (%current-system))
                          ("x86_64-linux" "amd64")
                          ("aarch64-linux" "arm64")
                          (_ "unsupported"))))
              (string-append
               "https://downloads.claude.ai/claude-desktop/apt/stable/"
               "pool/main/c/claude-desktop/claude-desktop_"
               version
               "_"
               arch
               ".deb")))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "192adlb8jpz940sfg0rv4r5a5hkc5j3vqp259qa6ahfj5762rkbl")
                  ("aarch64-linux"
                   "0j466bbnckgdmsc9pqwisrnacgbx1hsjz0ncg81jz22k7qhc9cwg")
                  (_ "0000000000000000000000000000000000000000000000000000"))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:validate-runpath? #f ;bundled node modules and wrapped binaries
      #:wrapper-plan
      #~(let ((rpath '(("out" "/lib/claude-desktop")
                       ("nss" "/lib/nss")))
              (arch #$(if (target-aarch64?) "arm64" "x64")))
          (map (lambda (file)
                 (list (string-append "usr/lib/claude-desktop/" file) rpath))
               (list "claude-desktop"
                     "chrome-sandbox"
                     "chrome_crashpad_handler"
                     "libffmpeg.so"
                     "libvk_swiftshader.so"
                     "libvulkan.so.1"
                     "resources/chrome-native-host"
                     "resources/disclaimer"
                     "resources/virtiofsd"
                     (string-append "resources/app.asar.unpacked/node_modules/"
                                    "@ant/claude-native/"
                                    "claude-native-binding.node")
                     (string-append "resources/app.asar.unpacked/node_modules/"
                                    "node-pty/prebuilds/linux-" arch
                                    "/pty.node"))))
      #:install-plan
      #~'(("usr/lib/" "/lib")
          ("usr/share/applications" "/share/")
          ("usr/share/icons" "/share/")
          ("usr/share/doc" "/share/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patchelf 'patch-app-asar
            (lambda* (#:key inputs #:allow-other-keys)
              (define any (@ (srfi srfi-1) any))
              (define get-string-all (@ (ice-9 textual-ports) get-string-all))
              (define (contents file)
                (call-with-input-file file get-string-all))
              (let* ((python (search-input-file inputs "/bin/python3"))
                     (asar-tool #$(local-file "aux-files/asar.py"))
                     (lib "usr/lib/claude-desktop")
                     (asar (string-append lib "/resources/app.asar"))
                     (tree "app-asar")
                     (ovmf (string-append #$output
                                          "/share/claude-desktop/ovmf"))
                     ;; FHS executables absent from Guix System.
                     (programs
                      `(("\"/bin/ps\"" . ,(string-append "\"" #$procps
                                                         "/bin/ps\""))
                        ("\"/usr/bin/pgrep\"" . ,(string-append "\"" #$procps
                                                                "/bin/pgrep\""))
                        ("\"/usr/bin/secret-tool\""
                         . ,(string-append "\"" #$libsecret
                                           "/bin/secret-tool\""))
                        ("\"/usr/bin/busctl\"" . "\"busctl\"")))
                     ;; Let CLAUDE_CODE_LOCAL_BINARY select the Claude Code
                     ;; executable; release builds compile this out, leaving
                     ;; the downloaded, unpatched CLI as the only choice.
                     (local-binary
                      (string-append
                       "process.env.CLAUDE_CODE_LOCAL_BINARY&&("
                       "this.localBinaryInitPromise=this.initLocalBinary("
                       "process.env.CLAUDE_CODE_LOCAL_BINARY))"))
                     (firmware "\"/usr/share/OVMF/OVMF_CODE_4M.fd\""))
                (invoke python asar-tool "extract" asar tree)
                (let ((files (filter (lambda (file)
                                       (any (lambda (needle)
                                              (string-contains (contents file)
                                                               needle))
                                            (cons* "CLAUDE_CODE_LOCAL_BINARY}"
                                                   firmware
                                                   "\"/usr/libexec/virtiofsd\""
                                                   (map car programs))))
                                     (find-files (string-append tree
                                                                "/.vite/build")
                                                 "\\.js$"))))
                  (substitute* files
                    (((string-append "process\\.env\\.CLAUDE_CODE_LOCAL_BINARY"
                                     "\\}async initLocalBinary\\("))
                     (string-append local-binary "}async initLocalBinary("))
                    (("\"/bin/ps\"")
                     (assoc-ref programs "\"/bin/ps\""))
                    (("\"/usr/bin/pgrep\"")
                     (assoc-ref programs "\"/usr/bin/pgrep\""))
                    (("\"/usr/bin/secret-tool\"")
                     (assoc-ref programs "\"/usr/bin/secret-tool\""))
                    (("\"/usr/bin/busctl\"")
                     "\"busctl\"")
                    ;; Cowork boots its VM with OVMF; offer the Guix build
                    ;; under the names the application expects.
                    (("\"/usr/share/OVMF/OVMF_CODE_4M\\.fd\"")
                     (string-append "\"" ovmf "/OVMF_CODE.fd\"," firmware))
                    ;; Outside Ubuntu 22.04 Cowork ignores its bundled,
                    ;; patched virtiofsd and requires one in /usr.
                    (("\"/usr/libexec/virtiofsd\"" all)
                     (string-append "\"" #$output "/lib/claude-desktop/"
                                    "resources/virtiofsd\"," all)))
                  (when (any (lambda (file)
                               (any (lambda (needle)
                                      (string-contains (contents file) needle))
                                    (map car programs)))
                             files)
                    (error "failed to patch FHS paths in app.asar"))
                  (unless (any (lambda (file)
                                 (string-contains (contents file) local-binary))
                               files)
                    (error "failed to enable CLAUDE_CODE_LOCAL_BINARY")))
                (invoke python asar-tool "pack" asar tree)
                (delete-file-recursively tree))))
          (add-before 'install 'patch-integration-files
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((exe (string-append #$output "/bin/claude-desktop"))
                    (provider "usr/lib/claude-desktop/resources/gnome-search-provider"))
                (substitute* "usr/share/applications/com.anthropic.Claude.desktop"
                  (("^Exec=claude-desktop")
                   (string-append "Exec=" exe)))
                (substitute* (string-append provider "/searchProvider.js")
                  (("const EXECUTABLE = \"claude-desktop\";")
                   (string-append "const EXECUTABLE = \"" exe "\";")))
                (substitute* (string-append
                              provider
                              "/com.anthropic.Claude.SearchProvider.service")
                  (("^Exec=.*$")
                   (string-append "Exec=" #$(file-append gjs "/bin/gjs")
                                  " -m " #$output "/lib/claude-desktop/"
                                  "resources/gnome-search-provider/"
                                  "searchProvider.js\n"))))))
          (add-after 'install 'install-integration-files
            (lambda _
              (let ((resources (string-append #$output
                                              "/lib/claude-desktop/resources"))
                    (share (string-append #$output "/share")))
                (install-file (string-append resources "/linux-mime/"
                                             "com.anthropic.Claude.xml")
                              (string-append share "/mime/packages"))
                (install-file (string-append resources
                                             "/gnome-search-provider/com."
                                             "anthropic.Claude.search-provider.ini")
                              (string-append share
                                             "/gnome-shell/search-providers"))
                (install-file (string-append resources
                                             "/gnome-search-provider/com."
                                             "anthropic.Claude.SearchProvider.service")
                              (string-append share "/dbus-1/services")))
              (when #$(target-x86-64?)
                (let ((ovmf (string-append #$output
                                           "/share/claude-desktop/ovmf"))
                      (firmware #$(file-append ovmf-x86-64 "/share/firmware")))
                  (mkdir-p ovmf)
                  (symlink (string-append firmware "/ovmf_code_x64.bin")
                           (string-append ovmf "/OVMF_CODE.fd"))
                  (symlink (string-append firmware "/ovmf_vars_x64.bin")
                           (string-append ovmf "/OVMF_VARS.fd"))))))
          (add-before 'install-wrapper 'install-entrypoint
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin"))
                    (sh (search-input-file inputs "/bin/sh")))
                (mkdir-p bin)
                (with-output-to-file (string-append bin "/claude-desktop")
                  (lambda _
                    (format #t "#!~a~%" sh)
                    ;; Run Claude Code from Guix rather than an unpatched
                    ;; download.
                    (format #t ": \"${CLAUDE_CODE_LOCAL_BINARY:=~a}\"~%"
                            #$(file-append node-anthropic-ai-claude-code
                                           "/bin/claude"))
                    (display "export CLAUDE_CODE_LOCAL_BINARY\n")
                    (format #t "exec \"~a/lib/claude-desktop/claude-desktop\" \"$@\"~%"
                            #$output)))
                (chmod (string-append bin "/claude-desktop") #o555)))))))
    (native-inputs (list python-minimal))
    (inputs (list libcap-ng libseccomp pipewire))
    (home-page "https://claude.com/download")
    (synopsis "Anthropic Claude desktop application")
    (description
     "Claude Desktop is Anthropic's Electron-based desktop application for
Claude, including chat, Claude Code and Cowork.  This package runs Claude Code
sessions with the Guix @code{node-anthropic-ai-claude-code} package; Cowork
additionally needs QEMU in @env{PATH} and access to @file{/dev/kvm}.")
    (license (license:nonfree "https://www.anthropic.com/legal/consumer-terms"))))

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
