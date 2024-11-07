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
                                    (zshrc `(,(plain-file "guix-profile"
                                                          (string-append
                                                           "GUIX_PROFILE=\"$HOME/.config/guix/current\"
"
                                                           "[ -f \"$GUIX_PROFILE/etc/profile\" ] && . \"$GUIX_PROFILE/etc/profile\"
")) ,
                                             (plain-file "add-zsh-hook"
                                              "autoload -Uz add-zsh-hook\n")
                                             ,(plain-file "compinit"
                                                          (string-append
                                                           "autoload -Uz compinit\n"
                                                           "compinit -u\n"
                                                           "autoload -Uz bashcompinit && bashcompinit
"
                                                           "if [ -d \"$HOME/.guix-profile/share/zsh/site-functions\" ]; then
"
                                                           "    fpath+=($HOME/.guix-profile/share/zsh/site-functions)
"
                                                           "fi\n"
                                                           "if [ -d \"$HOME/.guix-home/profile/share/zsh/site-functions\" ]; then
"
                                                           "    fpath+=($HOME/.guix-home/profile/share/zsh/site-functions)
"
                                                           "fi\n"))
                                             ,(plain-file "completion-options"
                                               (string-append
                                                "zstyle ':completion:*' menu select\n"
                                                "zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
"
                                                "zstyle ':completion:*' list-colors ''\n"
                                                "zstyle ':completion:*' list-suffixes 'yes'
"
                                                "zstyle ':completion:*' expand 'yes'\n"
                                                "zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{A-Z}={a-z}'
"
                                                "zstyle ':completion:*' use-cache on\n"
                                                "zstyle ':completion:*' cache-path \"$HOME/.zcompdump\"
"))
                                             ,(plain-file "setopt"
                                                          (string-append
                                                           "setopt "
                                                           "NO_BEEP "
                                                           "EXTENDED_GLOB "
                                                           "NO_CLOBBER "
                                                           "SHARE_HISTORY "
                                                           "HIST_IGNORE_DUPS "
                                                           "HIST_IGNORE_SPACE "
                                                           "INC_APPEND_HISTORY "
                                                           "CORRECT_ALL\n"))
                                             ,(plain-file "colors"
                                               "autoload -Uz colors && colors\n")
                                             ,(plain-file "prompt-env"
                                                          (string-append
                                                           "function prompt_env_precmd {\n"
                                                           "    if [ -n \"$GUIX_ENVIRONMENT\" ]; then\n"
                                                           "        export PROMPT='%F{#ff9999}環境%f %F{#98be65}%2~%f %F{#51afef}>%f '
"
                                                           "    else\n"
                                                           "        export PROMPT='%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f '
"
                                                           "    fi\n"
                                                           "}\n"
                                                           "add-zsh-hook precmd prompt_env_precmd\n"))
                                             ,(plain-file "rprompt"
                                                          (string-append
                                                           "function update_clock_precmd {\n"
                                                           "    RPROMPT='%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f'
"
                                                           "}\n"
                                                           "add-zsh-hook precmd update_clock_precmd\n"))
                                             ,(plain-file "history-search"
                                                          (string-append
                                                           "bindkey '^[[A' history-search-backward\n"
                                                           "bindkey '^[[B' history-search-forward\n"))
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
                                                           "alias la='ls -A'\n"
                                                           "alias openaikey='export OPENAI_API_KEY=$(pass show apis/openai | head -n 1)'
"))))))

   (service home-inputrc-service-type
            (home-inputrc-configuration (key-bindings `(("Control-l" . "clear-screen")
                                                        ("Meta-b" . "backward-word")
                                                        ("Meta-f" . "forward-word")
                                                        ("Control-u" . "unix-line-discard")
                                                        ("Control-w" . "unix-word-rubout")))
                                        (variables `(("bell-style" . "visible")
                                                     ("colored-completion-prefix" . #t)
                                                     ("editing-mode" . "emacs")
                                                     ("show-all-if-ambiguous" . #t)
                                                     ("mark-symlinked-directories" . #t)
                                                     ("meta-flag" . #t)
                                                     ("input-meta" . #t)
                                                     ("convert-meta" . #f)
                                                     ("output-meta" . #t)
                                                     ("enable-bracketed-paste" . #t)))))

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
                                                     (music "$HOME/music")
                                                     (pictures
                                                      "$HOME/pictures")
                                                     (publicshare
                                                      "$HOME/public")
                                                     (templates
                                                      "$HOME/templates")
                                                     (videos "$HOME/videos")))))
