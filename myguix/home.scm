(define-module (myguix home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services fontutils)
  #:use-module (guix gexp)
  #:use-module (myguix system install)
  #:export (%my-base-home-services xdg-directory-service
                                   %default-xdg-directories
                                   %default-dotguile
                                   %default-mimeapps
                                   %my-shell-environment-variables
                                   %my-shell-aliases))

;;; Service constructors

(define-public (xdg-directory-service directories)
  "Create XDG directories from DIRECTORIES list."
  (simple-service 'xdg-directories home-activation-service-type
                  #~(begin
                      (use-modules (ice-9 format)
                                   (guix build utils))
                      (for-each (lambda (dir)
                                  (let ((expanded-dir (string-append (getenv
                                                                      "HOME")
                                                                     (substring
                                                                      dir 1))))
                                    (unless (file-exists? expanded-dir)
                                      (format #t "Creating directory: ~a~%"
                                              expanded-dir)
                                      (mkdir-p expanded-dir)
                                      ;; Set appropriate permissions for sensitive directories
                                      (when (or (string-contains expanded-dir
                                                                 "gnupg")
                                                (string-contains expanded-dir
                                                                 "ssh")
                                                (string-contains expanded-dir
                                                 "password-store"))
                                        (chmod expanded-dir #o700)))))
                                '#$directories))))

;;; Shell configuration variables

(define-public %my-shell-environment-variables
  `( ;Shell environment
     ("HISTSIZE" . "50000")
    ("SAVEHIST" . "50000")
    ("HISTFILE" . "$XDG_STATE_HOME/zsh/history")
    ("LESSHISTFILE" . "$XDG_STATE_HOME/less/history")
    ("NODE_REPL_HISTORY" . "$XDG_STATE_HOME/node/repl_history")
    ("SQLITE_HISTORY" . "$XDG_STATE_HOME/sqlite/history")
    ("PSQL_HISTORY" . "$XDG_STATE_HOME/psql/history")
    ("MYSQL_HISTFILE" . "$XDG_STATE_HOME/mysql/history")

    ;; Python
    ("PYTHONSTARTUP" . "$XDG_CONFIG_HOME/python/pythonrc")
    ("PYTHONHISTFILE" . "$XDG_STATE_HOME/python/history")
    ("PYTHONUSERBASE" . "$XDG_DATA_HOME/python")
    ("PYTHON_EGG_CACHE" . "$XDG_CACHE_HOME/python-eggs")
    ("PIPENV_VENV_IN_PROJECT" . "1")
    ("WORKON_HOME" . "$XDG_DATA_HOME/virtualenvs")
    ("JUPYTER_CONFIG_DIR" . "$XDG_CONFIG_HOME/jupyter")

    ;; Rust
    ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
    ("RUSTUP_HOME" . "$XDG_DATA_HOME/rustup")

    ;; Go
    ("GOPATH" . "$XDG_DATA_HOME/go")
    ("GOMODCACHE" . "$XDG_CACHE_HOME/go/mod")

    ;; Security tools
    ("GNUPGHOME" . "$XDG_DATA_HOME/gnupg")
    ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/password-store")
    ("NETRC" . "$XDG_CONFIG_HOME/netrc")

    ;; Development tools
    ("GRADLE_USER_HOME" . "$XDG_DATA_HOME/gradle")
    ("ANDROID_HOME" . "$XDG_DATA_HOME/android")
    ("ANDROID_USER_HOME" . "$XDG_DATA_HOME/android")
    ("ANDROID_PREFS_ROOT" . "$XDG_CONFIG_HOME/android")
    ("ANDROID_EMULATOR_HOME" . "$XDG_DATA_HOME/android/emulator")

    ;; Network tools
    ("WGETRC" . "$XDG_CONFIG_HOME/wget/wgetrc")
    ("CURL_HOME" . "$XDG_CONFIG_HOME/curl")

    ;; Database tools
    ("PGPASSFILE" . "$XDG_CONFIG_HOME/pg/pgpass")
    ("PGSERVICEFILE" . "$XDG_CONFIG_HOME/pg/pg_service.conf")
    ("PGSYSCONFDIR" . "$XDG_CONFIG_HOME/pg")

    ;; Cloud tools
    ("AWS_SHARED_CREDENTIALS_FILE" . "$XDG_CONFIG_HOME/aws/credentials")
    ("AWS_CONFIG_FILE" . "$XDG_CONFIG_HOME/aws/config")
    ("DOCKER_CONFIG" . "$XDG_CONFIG_HOME/docker")

    ;; Other applications
    ("INPUTRC" . "$XDG_CONFIG_HOME/readline/inputrc")
    ("SCREENRC" . "$XDG_CONFIG_HOME/screen/screenrc")
    ("TMUX_TMPDIR" . "$XDG_RUNTIME_DIR")
    ("LESSKEY" . "$XDG_CONFIG_HOME/less/lesskey")
    ("PARALLEL_HOME" . "$XDG_CONFIG_HOME/parallel")

    ;; Disable less history if XDG not supported
    ("LESSHISTSIZE" . "0")))

(define-public %my-shell-aliases
  '(("wget" . "wget --hsts-file=\"$XDG_CACHE_HOME/wget-hsts\"")
    ("yarn" . "yarn --use-yarnrc \"$XDG_CONFIG_HOME/yarn/config\"")))

;;; Defaults

(define-public %default-xdg-directories
  '( ;XDG Base Directories
     "~/.local/bin"
    "~/.local/share"
    "~/.local/state"
    "~/.cache"
    "~/.config"

    ;; State directories for history files
    "~/.local/state/zsh"
    "~/.local/state/less"
    "~/.local/state/python"
    "~/.local/state/gdb"
    "~/.local/state/node"
    "~/.local/state/sqlite"
    "~/.local/state/psql"

    ;; Cache directories
    "~/.cache/zsh"
    "~/.cache/npm"
    "~/.cache/pip"
    "~/.cache/fontconfig"
    "~/.cache/mesa_shader_cache"
    "~/.cache/git"
    "~/.cache/wget"
    "~/.cache/less"
    "~/.cache/mozilla"

    ;; Config directories
    "~/.config/python"
    "~/.config/npm"
    "~/.config/git"
    "~/.config/fontconfig"
    "~/.config/wget"
    "~/.config/readline"
    "~/.config/pg"
    "~/.config/mysql"
    "~/.config/aws"
    "~/.config/docker"
    "~/.config/ssl"
    "~/.config/claude"

    ;; Data directories
    "~/.local/share/cargo"
    "~/.local/share/rustup"
    "~/.local/share/go"
    "~/.local/share/gnupg"
    "~/.local/share/npm"
    "~/.local/share/fonts"
    "~/.local/share/applications"
    "~/.local/share/icons"
    "~/.local/share/password-store"
    "~/.local/share/authinfo"
    "~/.local/share/certificates"
    "~/.local/share/ca-certificates"
    "~/.local/share/aws"
    "~/.local/share/docker"
    "~/.local/share/gradle"
    "~/.local/share/android"
    "~/.local/share/claude"
    "~/.local/share/zsh"
    "~/.local/share/mysql"
    "~/.local/share/psql"))

(define-public %default-dotguile
  (plain-file "guile" "(use-modules (ice-9 readline)
                           (ice-9 colorized))
(activate-readline)
(activate-colorized)"))

(define-public %default-inputrc
  (plain-file "inputrc" "# See ~/.inputrc"))

(define-public %default-npmrc
  (plain-file "npmrc" "prefix=${XDG_DATA_HOME}/npm
cache=${XDG_CACHE_HOME}/npm
init-module=${XDG_CONFIG_HOME}/npm/config/npm-init.js
logs-dir=${XDG_STATE_HOME}/npm/logs"))

(define-public %default-wgetrc
  (plain-file "wgetrc" "hsts-file = ~/.cache/wget/hsts"))

(define-public %default-pythonrc
  (plain-file "pythonrc"
   "import os
import atexit
import readline

histfile = os.path.join(os.environ.get('XDG_STATE_HOME', os.path.expanduser('~/.local/state')), 'python', 'history')
try:
    readline.read_history_file(histfile)
except FileNotFoundError:
    pass

atexit.register(readline.write_history_file, histfile)"))

(define-public %default-authinfo-readme
  (plain-file "authinfo-readme"
              "Place your authinfo.gpg file in this directory:
$XDG_DATA_HOME/authinfo/authinfo.gpg"))

(define-public %default-mimeapps
  (plain-file "mimeapps.list"
              "[Default Applications]
text/html=firefox.desktop
x-scheme-handler/http=firefox.desktop
x-scheme-handler/https=firefox.desktop
x-scheme-handler/about=firefox.desktop
x-scheme-handler/unknown=firefox.desktop
application/pdf=org.gnome.Evince.desktop
image/png=org.gnome.eog.desktop
image/jpeg=org.gnome.eog.desktop
image/gif=org.gnome.eog.desktop
image/webp=org.gnome.eog.desktop
image/svg+xml=org.gnome.eog.desktop
video/mp4=mpv.desktop
video/x-matroska=mpv.desktop
video/webm=mpv.desktop
video/avi=mpv.desktop
video/quicktime=mpv.desktop
audio/mpeg=audacious.desktop
audio/flac=audacious.desktop
audio/ogg=audacious.desktop
audio/x-vorbis+ogg=audacious.desktop
audio/x-wav=audacious.desktop
audio/x-m4a=audacious.desktop
audio/mp4=audacious.desktop
text/plain=org.gnome.TextEditor.desktop
text/x-c=org.gnome.TextEditor.desktop
text/x-c++=org.gnome.TextEditor.desktop
text/x-python=org.gnome.TextEditor.desktop
text/x-java=org.gnome.TextEditor.desktop
text/x-makefile=org.gnome.TextEditor.desktop
text/x-readme=org.gnome.TextEditor.desktop
text/x-log=org.gnome.TextEditor.desktop
text/markdown=org.gnome.TextEditor.desktop
application/x-shellscript=org.gnome.TextEditor.desktop
application/xml=org.gnome.TextEditor.desktop
application/json=org.gnome.TextEditor.desktop
application/javascript=org.gnome.TextEditor.desktop
application/x-yaml=org.gnome.TextEditor.desktop
inode/directory=org.gnome.Nautilus.desktop
application/x-compressed-tar=org.gnome.FileRoller.desktop
application/x-tar=org.gnome.FileRoller.desktop
application/zip=org.gnome.FileRoller.desktop
application/x-7z-compressed=org.gnome.FileRoller.desktop
application/x-rar-compressed=org.gnome.FileRoller.desktop
"))

(define-public %my-base-home-services
  (list
   ;; Shell configuration
   (service home-inputrc-service-type
            (home-inputrc-configuration (key-bindings `(("Control-l" . "clear-screen")
                                                        ("TAB" . "menu-complete")))
                                        (variables `(("bell-style" . "visible")
                                                     ("editing-mode" . "emacs")
                                                     ("show-all-if-ambiguous" . #t)
                                                     ("mark-symlinked-directories" . #t)
                                                     ("visible-stats" . #t)
                                                     ("colored-stats" . #t)
                                                     ("colored-completion-prefix" . #t)
                                                     ("menu-complete-display-prefix" . #t)))))

   ;; Dotfiles
   (service home-files-service-type
            `((".guile" ,%default-dotguile)))

   ;; XDG config files
   (service home-xdg-configuration-files-service-type
            `(("readline/inputrc" ,%default-inputrc)
              ("npm/npmrc" ,%default-npmrc)
              ("wget/wgetrc" ,%default-wgetrc)
              ("python/pythonrc" ,%default-pythonrc)
              ("authinfo/README" ,%default-authinfo-readme)
              ("mimeapps.list" ,%default-mimeapps)))

   ;; Create XDG directories
   (xdg-directory-service %default-xdg-directories)

   ;; Guix channels
   (service home-channels-service-type %my-channels)

   ;; Font configuration (keeping the same as before)
   (simple-service 'custom-fontconfig home-fontconfig-service-type
                   (list
                    ;; Include font paths
                    "~/.guix-home/profile/share/fonts"
                    "~/.local/share/fonts"

                    ;; Font preferences and aliases
                    '(alias (family "monospace")
                            (prefer (family "SF Mono")
                                    (family "JetBrains Mono")
                                    (family "Fira Code")
                                    (family "Fira Mono")
                                    (family "Roboto Mono")
                                    (family "Iosevka")
                                    (family "Hack")
                                    (family "Source Code Pro")
                                    (family "Liberation Mono")))

                    '(alias (family "sans-serif")
                            (prefer (family "SF Pro Display")
                                    (family "SF Pro Text")
                                    (family "Inter")
                                    (family "Roboto")
                                    (family "Fira Sans")
                                    (family "Open Sans")
                                    (family "Liberation Sans")
                                    (family "Noto Sans")))

                    '(alias (family "serif")
                            (prefer (family "New York")
                                    (family "Charter")
                                    (family "Roboto Serif")
                                    (family "Roboto Slab")
                                    (family "Liberation Serif")
                                    (family "Noto Serif")))

                    ;; Better font rendering
                    '(match (target "font")
                       (edit (mode "assign")
                             (name "antialias")
                             (bool "true")))

                    '(match (target "font")
                       (edit (mode "assign")
                             (name "hinting")
                             (bool "true")))

                    '(match (target "font")
                       (edit (mode "assign")
                             (name "hintstyle")
                             (const "hintslight")))

                    '(match (target "font")
                       (edit (mode "assign")
                             (name "rgba")
                             (const "rgb")))

                    '(match (target "font")
                       (edit (mode "assign")
                             (name "lcdfilter")
                             (const "lcddefault")))

                    ;; Enable ligatures for programming fonts
                    '(match (target "font")
                       (test (name "family")
                             (string "Fira Code"))
                       (edit (mode "assign")
                             (name "fontfeatures")
                             (string "liga on, calt on")))

                    '(match (target "font")
                       (test (name "family")
                             (string "JetBrains Mono"))
                       (edit (mode "assign")
                             (name "fontfeatures")
                             (string "liga on, calt on")))

                    ;; Emoji font configuration
                    '(match (target "pattern")
                       (test (name "family")
                             (string "monospace"))
                       (edit (mode "append")
                             (name "family")
                             (string "Apple Color Emoji")))

                    '(match (target "pattern")
                       (test (name "family")
                             (string "sans-serif"))
                       (edit (mode "append")
                             (name "family")
                             (string "Noto Color Emoji")))

                    '(match (target "pattern")
                       (test (name "family")
                             (string "serif"))
                       (edit (mode "append")
                             (name "family")
                             (string "Apple Color Emoji")))

                    ;; Disable bitmap fonts
                    '(selectfont (rejectfont (pattern (patelt (name "scalable")
                                                              (bool "false")))))))

   ;; XDG user directories
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration (desktop "$HOME/desktop")
                                                     (documents
                                                      "$HOME/documents")
                                                     (download
                                                      "$HOME/downloads")
                                                     (music "$HOME/music")
                                                     (pictures
                                                      "$HOME/pictures")
                                                     (publicshare
                                                      "$HOME/public")
                                                     (templates
                                                      "$HOME/templates")
                                                     (videos "$HOME/videos")))))
