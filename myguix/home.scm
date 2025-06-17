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
  #:use-module (guix gexp)
  #:export (%my-base-home-services %my-shell-base-services
                                   %my-development-base-services
                                   xdg-directory-service
                                   legacy-migration-service
                                   cleanup-cache-service
                                   make-zsh-plugin-file
                                   %default-xdg-directories
                                   %default-legacy-migrations
                                   %default-dotguile
                                   %default-gdbinit
                                   %default-nanorc))

;;; Service constructors

(define-public (xdg-directory-service directories)
  "Create XDG directories from DIRECTORIES list."
  (simple-service 'xdg-directories home-activation-service-type
                  #~(begin
                      (use-modules (ice-9 format))
                      (for-each (lambda (dir)
                                  (let ((expanded-dir (string-append (getenv
                                                                      "HOME")
                                                                     (substring
                                                                      dir 1))))
                                    (unless (file-exists? expanded-dir)
                                      (format #t "Creating directory: ~a~%"
                                              expanded-dir)
                                      (system* "mkdir" "-p" expanded-dir))))
                                '#$directories))))

(define-public (legacy-migration-service migrations
                                         #:key (marker-file ".legacy-migrated"))
  "Migrate files according to MIGRATIONS alist of (OLD . NEW) pairs."
  (simple-service 'legacy-migration home-activation-service-type
                  #~(begin
                      (use-modules (ice-9 format))
                      (let ((home (getenv "HOME"))
                            (marker (string-append (or (getenv
                                                        "XDG_STATE_HOME")
                                                       (string-append (getenv
                                                                       "HOME")
                                                        "/.local/state")) "/"
                                                   #$marker-file)))
                        (unless (file-exists? marker)
                          (for-each (lambda (pair)
                                      (let* ((old (string-append home "/"
                                                                 (car pair)))
                                             (new (string-append home "/"
                                                                 (cdr pair))))
                                        (when (and (file-exists? old)
                                                   (not (file-exists? new)))
                                          (format #t "Migrating ~a -> ~a~%"
                                                  old new)
                                          (system* "mkdir" "-p"
                                                   (dirname new))
                                          (rename-file old new))))
                                    '#$migrations)
                          (system* "mkdir" "-p"
                                   (dirname marker))
                          (call-with-output-file marker
                            (const #t)))))))

(define-public (cleanup-cache-service cleanup-specs)
  "Clean up cache files per CLEANUP-SPECS: (DIRECTORY PATTERN AGE-DAYS) tuples."
  (simple-service 'cleanup-cache home-activation-service-type
                  #~(begin
                      (for-each (lambda (spec)
                                  (let ((directory (car spec))
                                        (pattern (cadr spec))
                                        (age-days (caddr spec)))
                                    (let ((dir (string-append (getenv "HOME")
                                                              "/" directory)))
                                      (when (file-exists? dir)
                                        (system* "find"
                                                 dir
                                                 "-type"
                                                 "f"
                                                 "-name"
                                                 pattern
                                                 "-mtime"
                                                 (string-append "+"
                                                                (number->string
                                                                 age-days))
                                                 "-delete")))))
                                '#$cleanup-specs))))

(define-public (make-zsh-plugin-file name package path)
  "Source zsh plugin from PACKAGE at PATH."
  (mixed-text-file name "source " package path))

;;; Defaults

(define-public %default-xdg-directories
  '("~/.local/state" "~/.local/state/zsh" "~/.local/state/less" "~/.cache/zsh"
    "~/.config"))

(define-public %default-legacy-migrations
  '((".zsh_history" . ".local/state/zsh/history")
    (".lesshst" . ".local/state/less/history")))

(define-public %default-dotguile
  (plain-file "guile" "(use-modules (ice-9 readline)
                           (ice-9 colorized))
(activate-readline)
(activate-colorized)"))

(define-public %default-gdbinit
  (plain-file "gdbinit" "set history save on
set history filename ~/.local/state/gdb_history
set history size 10000"))

(define-public %default-nanorc
  (plain-file "nanorc" "set positionlog
set historylog
set suspendable"))

(define-public %my-shell-base-services
  (list (service home-inputrc-service-type
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

        (simple-service 'shell-environment-variables
                        home-environment-variables-service-type
                        `(("HISTSIZE" . "50000") ("SAVEHIST" . "50000")
                          ("LESSHISTFILE" . "$XDG_STATE_HOME/less/history")))))

(define-public %my-development-base-services
  (list (simple-service 'development-environment-variables
                        home-environment-variables-service-type
                        `(("PYTHONSTARTUP" . "$XDG_CONFIG_HOME/python/pythonrc")
                          ("PYTHONHISTFILE" . "$XDG_STATE_HOME/python_history")
                          ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
                          ("RUSTUP_HOME" . "$XDG_DATA_HOME/rustup")
                          ("GOPATH" . "$XDG_DATA_HOME/go")
                          ("GNUPGHOME" . "$XDG_DATA_HOME/gnupg")))))

(define-public %my-base-home-services
  (append %my-shell-base-services %my-development-base-services
          (list (service home-files-service-type
                         `((".guile" ,%default-dotguile)))

                (service home-xdg-configuration-files-service-type
                         `(("gdb/gdbinit" ,%default-gdbinit)
                           ("nano/nanorc" ,%default-nanorc)))

                (xdg-directory-service %default-xdg-directories)
                (legacy-migration-service %default-legacy-migrations)

                (service home-xdg-user-directories-service-type
                         (home-xdg-user-directories-configuration (desktop
                                                                   "$HOME/desktop")
                                                                  (documents
                                                                   "$HOME/documents")
                                                                  (download
                                                                   "$HOME/downloads")
                                                                  (music
                                                                   "$HOME/music")
                                                                  (pictures
                                                                   "$HOME/pictures")
                                                                  (publicshare
                                                                   "$HOME/public")
                                                                  (templates
                                                                   "$HOME/templates")
                                                                  (videos
                                                                   "$HOME/videos"))))))
