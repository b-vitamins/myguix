(define-module (myguix services home emacs)
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
        emacs-setup
        emacs-spinner
        emacs-modus-themes
        emacs-doom-themes
        emacs-doom-modeline
        emacs-rainbow-delimiters
        emacs-rainbow-mode
        emacs-adaptive-wrap
        emacs-smartparens
        emacs-paredit
        emacs-nerd-icons
        emacs-nerd-icons-dired
        emacs-nerd-icons-corfu
        emacs-nerd-icons-ibuffer
        emacs-nerd-icons-completion
        emacs-kind-icon
        emacs-all-the-icons
        emacs-no-littering
        emacs-exec-path-from-shell
        emacs-guix
        emacs-mjolnir-mode
        emacs-windsize
        emacs-ace-window
        emacs-olivetti
        emacs-which-key
        emacs-org-fragtog
        emacs-vertico
        emacs-orderless
        emacs-marginalia
        emacs-compat
        emacs-consult
        emacs-consult-org-roam
        emacs-corfu
        emacs-embark
        emacs-citar
        emacs-org-roam
        emacs-org-roam-ui
        emacs-websocket
        emacs-simple-httpd
        emacs-pdf-tools
        emacs-dired-hacks
        emacs-oauth2
        emacs-pinentry
        emacs-auth-source-pass
        emacs-password-store
        emacs-yasnippet
        emacs-geiser
        emacs-geiser-guile
        emacs-lsp-mode
        emacs-lsp-ui
        emacs-flycheck
        emacs-flycheck-inline
        emacs-flycheck-guile
        emacs-flycheck-package
        emacs-flycheck-rust
        emacs-flycheck-haskell
        emacs-lsp-jedi
        emacs-lsp-haskell
        emacs-rust-mode
        emacs-rustic
        emacs-ht
        emacs-hydra
        emacs-let-alist
        emacs-haskell-mode
        emacs-gnuplot
        emacs-lua-mode
        emacs-json-mode
        emacs-dockerfile-mode
        emacs-yaml-mode
        emacs-toml-mode
        emacs-julia-mode
        emacs-cmake-mode
        emacs-god-mode
        emacs-gptel
        emacs-magit
        emacs-auctex
        emacs-jit-spell
        emacs-web-mode
        emacs-combobulate
        emacs-markdown-mode
        emacs-vterm
        emacs-vterm-toggle
        emacs-projectile
        emacs-dash
        emacs-eldoc
        emacs-dashboard
        emacs-f
        emacs-tablist
        emacs-treemacs
        emacs-treemacs-extra
        emacs-lsp-treemacs
        emacs-lispy
        emacs-org-modern
        emacs-helpful
        emacs-restart-emacs
        emacs-ripgrep
        emacs-rg
        emacs-corfu-candidate-overlay
        emacs-cape
        emacs-ac-geiser
        emacs-iedit
        emacs-s
        emacs-seq
        emacs-multiple-cursors
        emacs-arxiv-mode))

(define my-home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Add Emacs packages to home environment profile.")
                (extensions (list (service-extension home-profile-service-type
                                   my-home-emacs-profile-service-type)))
                (default-value #f)))
