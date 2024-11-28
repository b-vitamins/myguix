(define-module (myguix home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services xdg)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:export (%my-home-services))

(define %my-xdefaults
  (plain-file "Xdefaults" "XTerm*utf8: always
XTerm*metaSendsEscape: false
"))

(define %my-home-services
  (list (service home-files-service-type
                 `((".guile" ,%default-dotguile)
                   (".Xdefaults" ,%my-xdefaults)))

        (service home-xdg-configuration-files-service-type
                 `(("gdb/gdbinit" ,%default-gdbinit)
                   ("nano/nanorc" ,%default-nanorc)))

        (service home-zsh-service-type
                 (home-zsh-configuration (xdg-flavor? #t)
                                         (zshenv (list (local-file
                                                        "etc/zsh/zshenv")))
                                         (zshrc (list (local-file
                                                       "etc/zsh/zshrc")
                                                      (mixed-text-file "zshrc"
                                                       "source "
                                                       zsh-syntax-highlighting
                                                       "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh")
                                                      (mixed-text-file "zshrc"
                                                       "source "
                                                       zsh-history-substring-search
                                                       "/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh")
                                                      (mixed-text-file "zshrc"
                                                       "fpath+=\"${0:A:h}"
                                                       zsh-completions
                                                       "share/zsh/site-functions/\"")
                                                      (mixed-text-file "zshrc"
                                                       "source "
                                                       zsh-autosuggestions
                                                       "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh")
                                                      (mixed-text-file "zshrc"
                                                       "source " zsh-autopair
                                                       "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh")))))

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

        (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration (pinentry-program (file-append (specification->package
                                                                               "pinentry")
                                                                  "/bin/pinentry"))
                                               (ssh-support? #t)
                                               (default-cache-ttl 28800)
                                               (max-cache-ttl 28800)
                                               (default-cache-ttl-ssh 28800)
                                               (max-cache-ttl-ssh 28800)))

        (service home-xdg-user-directories-service-type
                 (home-xdg-user-directories-configuration (desktop
                                                           "$HOME/desktop")
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
                                                          (videos
                                                           "$HOME/videos")))))
