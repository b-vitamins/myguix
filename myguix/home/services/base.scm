(define-module (myguix home services base)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services fontutils)
  #:use-module (guix gexp)
  #:use-module (myguix system install)
  #:export (;; Service constructors
            xdg-directory-service
            
            ;; Main service bundle
            %my-base-home-services
            
            ;; Environment variables
            %base-shell-environment-variables
            %development-environment-variables
            %xdg-compliance-environment-variables
            %base-shell-aliases
            
            ;; XDG directories
            %base-xdg-directories
            %development-xdg-directories
            
            ;; Configuration files
            %default-dotguile
            %default-gdbinit
            %default-nanorc
            %default-inputrc
            %default-npmrc
            %default-wgetrc
            %default-pythonrc
            %default-authinfo-readme
            
            ;; Input configuration
            %default-inputrc-config
            
            ;; Font configuration
            %base-font-config))

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

;; Base environment variables that everyone needs
(define-public %base-shell-environment-variables
  `(;; Basic environment
    ("EDITOR" . "nano")  ; Safe default, users can override
    ("VISUAL" . "nano")
    ("PAGER" . "less")
    ("LESS" . "-FRX")
    ("BROWSER" . "firefox")
    
    ;; Shell history
    ("HISTSIZE" . "50000")
    ("SAVEHIST" . "50000")
    ("HISTFILE" . "$XDG_STATE_HOME/bash/history")
    
    ;; Disable less history for privacy
    ("LESSHISTSIZE" . "0")
    
    ;; Python best practices
    ("PYTHONDONTWRITEBYTECODE" . "1")
    ("PYTHONUNBUFFERED" . "1")))

;; Development-specific variables
(define-public %development-environment-variables
  `(;; Build tools
    ("MAKEFLAGS" . "-j8")  ; Safe default, users can override
    ("CMAKE_GENERATOR" . "Ninja")
    
    ;; CUDA
    ("CUDA_CACHE_PATH" . "$XDG_CACHE_HOME/nv")
    
    ;; Compilation
    ("CCACHE_DIR" . "$XDG_CACHE_HOME/ccache")))

;; XDG compliance variables
(define-public %xdg-compliance-environment-variables
  `(;; Shell and CLI tools
    ("LESSHISTFILE" . "$XDG_STATE_HOME/less/history")
    ("NODE_REPL_HISTORY" . "$XDG_STATE_HOME/node/repl_history")
    ("SQLITE_HISTORY" . "$XDG_STATE_HOME/sqlite/history")
    ("PSQL_HISTORY" . "$XDG_STATE_HOME/psql/history")
    ("MYSQL_HISTFILE" . "$XDG_STATE_HOME/mysql/history")
    ("INPUTRC" . "$XDG_CONFIG_HOME/readline/inputrc")
    
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
    
    ;; Node.js
    ("NPM_CONFIG_USERCONFIG" . "$XDG_CONFIG_HOME/npm/npmrc")
    ("NPM_CONFIG_CACHE" . "$XDG_CACHE_HOME/npm")
    
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
    ("SCREENRC" . "$XDG_CONFIG_HOME/screen/screenrc")
    ("TMUX_TMPDIR" . "$XDG_RUNTIME_DIR")
    ("LESSKEY" . "$XDG_CONFIG_HOME/less/lesskey")
    ("PARALLEL_HOME" . "$XDG_CONFIG_HOME/parallel")))

(define-public %base-shell-aliases
  '(("ls" . "ls --color=auto")
    ("ll" . "ls -l")
    ("la" . "ls -la")
    ("grep" . "grep --color=auto")
    ("wget" . "wget --hsts-file=\"$XDG_CACHE_HOME/wget-hsts\"")
    ("yarn" . "yarn --use-yarnrc \"$XDG_CONFIG_HOME/yarn/config\"")))

;;; XDG Directories

;; Essential directories everyone needs
(define-public %base-xdg-directories
  '(;; XDG Base Directories
    "~/.local/bin"
    "~/.local/share"
    "~/.local/state"
    "~/.cache"
    "~/.config"
    
    ;; Essential subdirectories
    "~/.local/share/applications"
    "~/.local/share/fonts"
    "~/.local/share/icons"
    "~/.config/fontconfig"
    "~/.cache/fontconfig"))

;; Development-related directories
(define-public %development-xdg-directories
  '(;; State directories for history files
    "~/.local/state/bash"
    "~/.local/state/zsh"
    "~/.local/state/less"
    "~/.local/state/python"
    "~/.local/state/gdb"
    "~/.local/state/node"
    "~/.local/state/sqlite"
    "~/.local/state/psql"
    "~/.local/state/mysql"
    
    ;; Cache directories
    "~/.cache/npm"
    "~/.cache/pip"
    "~/.cache/go"
    "~/.cache/ccache"
    "~/.cache/python-eggs"
    "~/.cache/wget"
    
    ;; Config directories
    "~/.config/python"
    "~/.config/npm"
    "~/.config/git"
    "~/.config/pg"
    "~/.config/mysql"
    "~/.config/aws"
    "~/.config/docker"
    "~/.config/android"
    "~/.config/readline"
    "~/.config/wget"
    
    ;; Data directories
    "~/.local/share/cargo"
    "~/.local/share/rustup"
    "~/.local/share/go"
    "~/.local/share/gnupg"
    "~/.local/share/npm"
    "~/.local/share/python"
    "~/.local/share/virtualenvs"
    "~/.local/share/gradle"
    "~/.local/share/android"))

;;; Configuration files

(define-public %default-dotguile
  (plain-file "guile" "(use-modules (ice-9 readline)
                           (ice-9 colorized))
(activate-readline)
(activate-colorized)"))

(define-public %default-gdbinit
  (plain-file "gdbinit" "set history save on
set history filename ~/.local/state/gdb/history
set history size 10000
set history remove-duplicates unlimited"))

(define-public %default-nanorc
  (plain-file "nanorc" "set positionlog
set historylog
set suspendable
set autoindent
set tabsize 4
set linenumbers
set mouse
set smarthome
set softwrap
set zap
set stateflags"))

(define-public %default-inputrc
  (plain-file "inputrc" "$include /etc/inputrc
set bell-style visible
set editing-mode emacs
set show-all-if-ambiguous on
set show-all-if-unmodified on
set mark-symlinked-directories on
set visible-stats on
set colored-stats on
set colored-completion-prefix on
set menu-complete-display-prefix on
set completion-ignore-case on
set completion-map-case on
set expand-tilde on
# Bindings
Control-l: clear-screen
TAB: menu-complete
\"\\e[Z\": menu-complete-backward"))

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

;;; Input configuration

(define-public %default-inputrc-config
  (home-inputrc-configuration
   (key-bindings
    `(("Control-l" . "clear-screen")
      ("TAB" . "menu-complete")
      ("\\e[Z" . "menu-complete-backward"))) ; Shift-Tab
   (variables
    `(("bell-style" . "visible")
      ("editing-mode" . "emacs")
      ("show-all-if-ambiguous" . #t)
      ("show-all-if-unmodified" . #t)
      ("mark-symlinked-directories" . #t)
      ("visible-stats" . #t)
      ("colored-stats" . #t)
      ("colored-completion-prefix" . #t)
      ("menu-complete-display-prefix" . #t)
      ("completion-ignore-case" . #t)
      ("completion-map-case" . #t)
      ("expand-tilde" . #t)))))

;;; Font configuration

(define-public %base-font-config
  (list
   ;; Include font paths
   "~/.guix-home/profile/share/fonts"
   "~/.local/share/fonts"
   
   ;; Font preferences and aliases
   '(alias (family "monospace")
           (prefer (family "JetBrains Mono")
                   (family "Fira Code")
                   (family "Fira Mono")
                   (family "Roboto Mono")
                   (family "Iosevka")
                   (family "Hack")
                   (family "Source Code Pro")
                   (family "Liberation Mono")))
   
   '(alias (family "sans-serif")
           (prefer (family "Inter")
                   (family "Roboto")
                   (family "Fira Sans")
                   (family "Open Sans")
                   (family "Liberation Sans")
                   (family "Noto Sans")))
   
   '(alias (family "serif")
           (prefer (family "Charter")
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
            (string "Noto Color Emoji")))
   
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
            (string "Noto Color Emoji")))
   
   ;; Disable bitmap fonts
   '(selectfont (rejectfont (pattern (patelt (name "scalable")
                                             (bool "false")))))))

;;; Base home services

(define-public %my-base-home-services
  (list
   ;; Shell configuration
   (service home-inputrc-service-type %default-inputrc-config)
   
   ;; Dotfiles
   (service home-files-service-type
            `((".guile" ,%default-dotguile)))
   
   ;; XDG config files
   (service home-xdg-configuration-files-service-type
            `(("gdb/gdbinit" ,%default-gdbinit)
              ("nano/nanorc" ,%default-nanorc)
              ("readline/inputrc" ,%default-inputrc)
              ("npm/npmrc" ,%default-npmrc)
              ("wget/wgetrc" ,%default-wgetrc)
              ("python/pythonrc" ,%default-pythonrc)
              ("authinfo/README" ,%default-authinfo-readme)))
   
   ;; Create XDG directories
   (xdg-directory-service (append %base-xdg-directories
                                  %development-xdg-directories))
   
   ;; Guix channels
   (service home-channels-service-type %my-channels)
   
   ;; Font configuration
   (simple-service 'base-fontconfig home-fontconfig-service-type
                   %base-font-config)
   
   ;; XDG user directories
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (desktop "$HOME/desktop")
             (documents "$HOME/documents")
             (download "$HOME/downloads")
             (music "$HOME/music")
             (pictures "$HOME/pictures")
             (publicshare "$HOME/public")
             (templates "$HOME/templates")
             (videos "$HOME/videos")))
   
   ;; Base environment variables
   (simple-service 'base-environment-variables
                   home-environment-variables-service-type
                   (append %base-shell-environment-variables
                           %xdg-compliance-environment-variables))))