(define-module (myguix home services emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages statistics)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (my-home-emacs-service-type))

(define (my-home-emacs-profile-service-type config)
  (list emacs
        emacs-academic-phrases
        emacs-ace-window
        emacs-all-the-icons
        emacs-all-the-icons-completion
        emacs-all-the-icons-dired
        emacs-arei
        emacs-async
        emacs-auctex
        emacs-auctex-latexmk
        emacs-calc-currency
        emacs-cape
        emacs-cargo
        emacs-cdlatex
        emacs-chronos
        emacs-circadian
        emacs-clojure-ts-mode
        emacs-citar
        emacs-citar-embark
        emacs-citar-org-roam
        emacs-csharp-mode
        emacs-clang-format
        emacs-cmake-mode
        emacs-combobulate
        emacs-company-auctex
        emacs-consult
        emacs-consult-bibtex
        emacs-consult-eglot
        emacs-consult-org-roam
        emacs-corfu
        emacs-corfu-candidate-overlay
        emacs-corfu-doc
        emacs-daemons
        emacs-dape
        emacs-dashboard
        emacs-devdocs
        emacs-dired-rsync
        emacs-dired-hacks
        emacs-display-wttr
        emacs-docker
        emacs-dockerfile-mode
        emacs-eat
        emacs-ebdb
        emacs-ednc
        emacs-eglot
        emacs-elfeed
        emacs-elfeed-score
        emacs-elfeed-org
        emacs-ellama
        emacs-elixir-mode
        emacs-elm-mode
        emacs-embark
        emacs-embark-consult
        emacs-emms
        emacs-envrc
        emacs-direnv
        emacs-erlang
        emacs-ess
        emacs-exec-path-from-shell
        emacs-eshell-prompt-extras
        emacs-eshell-syntax-highlighting
        emacs-eslint-fix
        emacs-expand-region
        emacs-flymake
        emacs-flymake-eslint
        emacs-flyspell-correct
        emacs-flyspell-correct-ivy
        emacs-fontaine
        emacs-geiser
        emacs-geiser-eros
        emacs-geiser-guile
        emacs-git-gutter
        emacs-git-gutter-fringe
        emacs-git-link
        emacs-git-timemachine
        emacs-go-mode
        emacs-gptel
        emacs-gptel-quick
        emacs-guix
        emacs-haskell-mode
        emacs-helm-bibtex
        emacs-helpful
        emacs-hide-mode-line
        emacs-hide-header-line
        emacs-header-minions
        emacs-highlight-indent-guides
        emacs-hl-todo
        emacs-htmlize
        emacs-hydra
        emacs-info+
        emacs-jit-spell
        emacs-js2-mode
        emacs-julia-mode
        emacs-jupyter
        emacs-json-simple-flymake
        emacs-justify-kp
        emacs-keycast
        emacs-kotlin-mode
        emacs-kind-icon
        emacs-langtool
        emacs-latex-snippets
        emacs-lispy
        emacs-lua-mode
        emacs-llm
        emacs-magit
        emacs-magit-todos
        emacs-marginalia
        emacs-mini-frame
        emacs-markdown-mode
        emacs-markdown-toc
        emacs-meson-mode
        emacs-minions
        emacs-mjolnir-mode
        emacs-moody
        emacs-modus-themes
        emacs-mpv
        emacs-nerd-icons
        emacs-nerd-icons-completion
        emacs-nerd-icons-dired
        emacs-nerd-icons-ibuffer
        emacs-nix-mode
        emacs-nov-el
        emacs-ol-notmuch
        emacs-olivetti
        emacs-orderless
        emacs-org-agenda-files-track-ql
        emacs-org-appear
        emacs-org-contrib
        emacs-org-fragtog
        emacs-org-make-toc
        emacs-org-modern
        emacs-org-node
        emacs-org-noter
        emacs-org-pdftools
        emacs-org-pomodoro
        emacs-org-ref
        emacs-org-recur
        emacs-org-roam
        emacs-org-roam-bibtex
        emacs-org-super-agenda
        emacs-org-wild-notifier
        emacs-ox-html-stable-ids
        emacs-ox-pandoc
        emacs-pandoc-mode
        emacs-pdf-tools
        emacs-perspective
        emacs-php-mode
        emacs-plantuml-mode
        emacs-poetry
        emacs-pomidor
        emacs-pulseaudio-control
        emacs-pyenv-mode
        emacs-pytest
        emacs-python-black
        emacs-racket-mode
        emacs-python-pytest
        emacs-python-test
        emacs-pyvenv
        emacs-rainbow-delimiters
        emacs-rainbow-identifiers
        emacs-rainbow-mode
        emacs-request
        emacs-rust-mode
        emacs-enh-ruby-mode
        emacs-s
        emacs-scala-mode
        emacs-saveplace-pdf-view
        emacs-sly
        emacs-sly-asdf
        emacs-sly-quicklisp
        emacs-sly-repl-ansi-color
        emacs-smartparens
        emacs-telega
        emacs-tempel
        emacs-toml-mode
        emacs-transient
        emacs-typescript-mode
        emacs-use-package
        emacs-vertico
        emacs-visual-fill-column
        emacs-wc-mode
        emacs-web-mode
        emacs-webpaste
        emacs-which-key
        emacs-writegood-mode
        emacs-ws-butler
        emacs-yaml-mode
        emacs-ytdl
        emacs-zotra))

(define my-home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Add Emacs packages to home environment profile.")
                (extensions (list (service-extension home-profile-service-type
                                   my-home-emacs-profile-service-type)))
                (default-value #f)))
