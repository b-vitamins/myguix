(define-module (myguix home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services xdg)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:export (%my-home-services))

(define %my-home-services
  (list
   ;; Essential Home Services
   (service home-files-service-type
            `((".guile" ,%default-dotguile)
              (".Xdefaults" ,%default-xdefaults)))

   (service home-xdg-configuration-files-service-type
            `(("gdb/gdbinit" ,%default-gdbinit)
              ("nano/nanorc" ,%default-nanorc)))

   (simple-service 'some-useful-env-vars-service
                   home-environment-variables-service-type
                   `(("SHELL" unquote
                      (file-append zsh "/bin/zsh"))
                     ("VISUAL" . "emacsclient")
                     ("EDITOR" . "emacsclient")))

   ;; Shells
   (service home-zsh-service-type
            (home-zsh-configuration (environment-variables '(("PROMPT" . "%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f ")
                                                             ("RPROMPT" . "%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f")
                                                             ("GUILE_AUTO_COMPILE" . "0")
                                                             ("HISTFILE" . "$HOME/.zhistory")
                                                             ("HISTSIZE" . "10000")
                                                             ("SAVEHIST" . "10000")
                                                             ("ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE" . "fg=4")))
                                    (zshrc `(,(plain-file "add-zsh-hook"
                                               "autoload -Uz add-zsh-hook\n") ,
                                             (plain-file "compinit"
                                                         (string-append
                                                          "autoload -Uz compinit\n"
                                                          "compinit -u\n"))
                                             ,(plain-file "colors"
                                               "autoload -Uz colors && colors\n")
                                             ,(plain-file "setopt"
                                                          (string-append
                                                           "setopt "
                                                           "NO_BEEP "
                                                           "EXTENDED_GLOB "
                                                           "NO_CLOBBER "
                                                           "SHARE_HISTORY "
                                                           "HIST_IGNORE_DUPS "
                                                           "HIST_IGNORE_SPACE "
                                                           "INC_APPEND_HISTORY\n"))
                                             ,(plain-file "rprompt"
                                                          (string-append
                                                           "function update_clock_precmd {\n"
                                                           "    RPROMPT='%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f'
"
                                                           "}\n"
                                                           "add-zsh-hook precmd update_clock_precmd\n"))
                                             ,(plain-file "trap-alarm"
                                                          (string-append
                                                           "TRAPALRM() {\n"
                                                           "    zle reset-prompt\n"
                                                           "}\n" "TMOUT=1\n"))
                                             ,(plain-file "aliases"
                                                          (string-append
                                                           "alias ls='ls --color=auto'\n"
                                                           "alias grep='grep --color=auto'\n"
                                                           "alias diff='diff --color=auto'\n"
                                                           "alias ll='ls -lh'\n"
                                                           "alias la='ls -A'\n"))))))

   (service home-inputrc-service-type
            (home-inputrc-configuration (key-bindings `(("Control-l" . "clear-screen")))
                                        (variables `(("bell-style" . "visible")
                                                     ("colored-completion-prefix" . #t)
                                                     ("editing-mode" . "vi")
                                                     ("show-mode-in-prompt" . #t)))))

   ;; GNU Privacy Guard
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration (pinentry-program (file-append (specification->package
                                                                          "pinentry")
                                                             "/bin/pinentry"))
                                          (ssh-support? #t)
                                          (default-cache-ttl 28800)
                                          (max-cache-ttl 28800)
                                          (default-cache-ttl-ssh 28800)
                                          (max-cache-ttl-ssh 28800)))
   ;; XDG User Directories
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration (desktop "$HOME/desktop")
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
                                                      "$HOME/videos")))))
