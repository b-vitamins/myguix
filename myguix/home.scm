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
                 (home-zsh-configuration (environment-variables '(("PROMPT" . "$%F{39}%n@%m%f %F{34}%~%f %F{160}>%f ")
                                                                  ("GUILE_AUTO_COMPILE" . "0")
                                                                  ("HISTFILE" . "$XDG_CONFIG_HOME/zsh/.zhistory")
                                                                  ("HISTSIZE" . "10000")
                                                                  ("SAVEHIST" . "10000")))
                                         (zshrc `(,(plain-file "zprofile"
                                                               (string-append
                                                                "if [ -f \"$HOME/.profile\" ]; then\n"
                                                                "    source \"$HOME/.profile\"\n"
                                                                "fi\n")) ,(plain-file
                                                                           "add-zsh-hook"
                                                                           "autoload -Uz add-zsh-hook\n")
                                                  ,(plain-file "compinit"
                                                               (string-append
                                                                "autoload -Uz compinit\n"
                                                                "compinit -u\n"))
                                                  ,(plain-file "colors"
                                                               (string-append
                                                                "autoload -Uz colors\n"
                                                                "colors\n"))
                                                  ,(plain-file "setopt"
                                                               (string-append
                                                                "setopt NO_BEEP\n"
                                                                "setopt EXTENDED_GLOB\n"
                                                                "setopt NO_CLOBBER\n"
                                                                "setopt NO_SHARE_HISTORY\n"
                                                                "setopt APPEND_HISTORY\n"
                                                                "setopt INC_APPEND_HISTORY\n"
                                                                "setopt HIST_IGNORE_DUPS\n"
                                                                "setopt HIST_IGNORE_SPACE\n"
                                                                "setopt HIST_REDUCE_BLANKS\n"))
                                                  ,(plain-file
                                                    "completion-options"
                                                    (string-append
                                                     "zstyle ':completion:*' menu select\n"
                                                     "zstyle ':completion:*' list-suffixes 'yes'
"
                                                     "zstyle ':completion:*' expand 'yes'\n"
                                                     "zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
"
                                                     "zstyle ':completion:*' use-cache on\n"
                                                     "zstyle ':completion:*' cache-path \"$XDG_CONFIG_HOME/zsh/.zcompdump\"
"))
                                                  ,(plain-file "colors"
                                                               "colors\n")
                                                  ,(plain-file "functions"
                                                               (string-append
                                                                "function is_utf8_locale {\n"
                                                                "    [[ \"${LC_ALL:-}${LANG:-}${GDM_LANG:-}\" =~ [Uu][Tt][Ff]-?8 ]]
"
                                                                "}\n\n"
                                                                "function prompt_env_precmd {\n"
                                                                "    local display_time\n"
                                                                "    [[ \"$SHOW_TIMESTAMP\" -eq 1 ]] && display_time=\"%F{#ECBE7B}%D{%H:%M:%S}%f %F{#88c0d0}%D{%d %b}%f \"

"
                                                                "    if is_utf8_locale; then\n"
                                                                "        if [[ -n \"$GUIX_ENVIRONMENT\" ]]; then
"
                                                                "            export PROMPT=\"${display_time}%F{#51afef}環境%f %F{#98be65}%2~%f %F{#ff6c6b}>%f \"
"
                                                                "        else\n"
                                                                "            export PROMPT=\"${display_time}%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f \"
"
                                                                "        fi\n"
                                                                "    else\n"
                                                                "        if [[ -n \"$GUIX_ENVIRONMENT\" ]]; then
"
                                                                "            export PROMPT=\"${display_time}%F{39}%n@%m%f %F{34}%~%f %F{220}[env]%f %F{160}>%f \"
"
                                                                "        else\n"
                                                                "            export PROMPT=\"${display_time}%F{39}%n@%m%f %F{34}%~%f %F{160}>%f \"
"
                                                                "        fi\n"
                                                                "    fi\n"
                                                                "}\n\n"
                                                                "function toggle_timestamp {\n"
                                                                "    SHOW_TIMESTAMP=$((1 - SHOW_TIMESTAMP))
"
                                                                "    prompt_env_precmd\n"
                                                                "    zle reset-prompt\n"
                                                                "}\n\n"
                                                                "zle -N toggle_timestamp\n"
                                                                "bindkey '^@' toggle_timestamp\n\n"
                                                                "add-zsh-hook precmd prompt_env_precmd\n"))
                                                  ,(plain-file "keybindings"
                                                               (string-append
                                                                "bindkey '^[[A' history-search-backward\n"
                                                                "bindkey '^[[B' history-search-forward\n"))
                                                  ,(plain-file "trap-alarm"
                                                    "TRAPALRM() { zle reset-prompt }\nTMOUT=1\n")
                                                  ,(plain-file "aliases"
                                                               (string-append
                                                                "alias ls='ls --color=auto'\n"
                                                                "alias grep='grep --color=auto'\n"
                                                                "alias diff='diff --color=auto'\n"
                                                                "alias ll='ls -lh --color=auto'\n"
                                                                "alias la='ls -A --color=auto'\n"
                                                                "alias e='emacs'\n"
                                                                "alias ec='emacsclient -c'\n"
                                                                "alias em='emacs -nw'\n"))))))

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
                                                          ("menu-complete-display-prefix" . #t)
                                                          ("meta-flag" . #t)
                                                          ("input-meta" . #t)
                                                          ("convert-meta" . #f)
                                                          ("output-meta" . #t)
                                                          ("enable-bracketed-paste" . #t)))))

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
