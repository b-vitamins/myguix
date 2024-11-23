(define-module (myguix home services emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (my-home-emacs-service-type))

(define (my-home-emacs-profile-service-type config)
  (list emacs
        emacs-0x0
        emacs-aio
        emacs-ac-geiser
        emacs-academic-phrases
        emacs-ace-window
        emacs-adaptive-wrap
        emacs-ag
        emacs-all-the-icons
        emacs-arei
        emacs-arxiv-mode
        emacs-async-await
        emacs-auctex
        emacs-auth-source-pass
        emacs-auto-complete
        emacs-blacken
        emacs-bui
        emacs-citar
        emacs-citar-org-roam
        emacs-citre
        emacs-cmake-mode
        emacs-combat
        emacs-compat
        emacs-combobulate
        emacs-consult
        emacs-consult-bibtex
        emacs-consult-dir
        emacs-consult-flycheck
        emacs-consult-lsp
        emacs-consult-org-roam
        emacs-consult-yasnippet
        emacs-corfu
        emacs-corfu-candidate-overlay
        emacs-ctrlf
        emacs-dash
        emacs-deadgrep
        emacs-devil
        emacs-dimmer
        emacs-dired-hacks
        emacs-dockerfile-mode
        emacs-doom-modeline
        emacs-doom-themes
        emacs-dumb-jump
        emacs-elegant-agenda-mode
        emacs-eldoc
        emacs-elfeed
        emacs-elfeed-goodies
        emacs-elfeed-org
        emacs-elfeed-protocol
        emacs-elfeed-score
        emacs-emacsql
        emacs-engine-mode
        emacs-eval-in-repl-geiser
        emacs-exec-path-from-shell
        emacs-f
        emacs-focus
        emacs-frecency
        emacs-flycheck
        emacs-flycheck-guile
        emacs-flycheck-haskell
        emacs-flycheck-inline
        emacs-flycheck-package
        emacs-flycheck-rust
        emacs-gnuplot
        emacs-god-mode
        emacs-gptel
        emacs-geiser
        emacs-geiser-guile
        emacs-geiser-racket
        emacs-guix
        emacs-haskell-mode
        emacs-helpful
        emacs-ht
        emacs-hydra
        emacs-hyperspace
        emacs-iedit
        emacs-jit-spell
        emacs-json-mode
        emacs-julia-mode
        emacs-kind-icon
        emacs-lispy
        emacs-log4e
        emacs-logview
        emacs-lsp-consult-lsp
        emacs-lsp-docker
        emacs-lsp-haskell
        emacs-lsp-jedi
        emacs-lsp-mode
        emacs-lsp-treemacs
        emacs-lsp-ui
        emacs-lua-mode
        emacs-lua-mode
        emacs-magit
        emacs-markdown-mode
        emacs-marginalia
        emacs-memory-usage
        emacs-mjolnir-mode
        emacs-multiple-cursors
        emacs-nnreddit
        emacs-no-littering
        emacs-nov-el
        emacs-nerd-icons
        emacs-nerd-icons-completion
        emacs-nerd-icons-corfu
        emacs-nerd-icons-dired
        emacs-nerd-icons-ibuffer
        emacs-oauth2
        emacs-olivetti
        emacs-openwith
        emacs-org-fragtog
        emacs-org-modern
        emacs-org-node
        emacs-org-roam
        emacs-org-roam-bibtex
        emacs-org-roam-ui
        emacs-org-super-agenda
        emacs-orderless
        emacs-paredit
        emacs-password-store
        emacs-pdf-tools
        emacs-pdfgrep
        emacs-pg
        emacs-pgmacs
        emacs-pinentry
        emacs-prescient
        emacs-projectile
        emacs-ripgrep
        emacs-rg
        emacs-restart-emacs
        emacs-refactor
        emacs-rainbow-delimiters
        emacs-rainbow-mode
        emacs-rust-mode
        emacs-rustic
        emacs-s
        emacs-seq
        emacs-setup
        emacs-smartparens
        emacs-spinner
        emacs-tablist
        emacs-toml-mode
        emacs-transient-posframe
        emacs-transmission
        emacs-treemacs
        emacs-treemacs-extra
        emacs-vterm
        emacs-vterm-toggle
        emacs-vertico
        emacs-vertico-posframe
        emacs-web-mode
        emacs-websocket
        emacs-which-key
        emacs-windsize
        emacs-xdg-recent-files
        emacs-xref
        emacs-xref-union
        emacs-yaml-mode
        emacs-yasnippet))

(define my-home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Add Emacs packages to home environment profile.")
                (extensions (list (service-extension home-profile-service-type
                                   my-home-emacs-profile-service-type)))
                (default-value #f)))
