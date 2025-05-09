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
  (list emacs-next
        emacs-ace-window
        emacs-adaptive-wrap
        emacs-all-the-icons
        emacs-arxiv-mode
        emacs-auctex
        emacs-auth-source-pass
        emacs-cape
        emacs-citar
        emacs-cmake-mode
        emacs-consult
        emacs-consult-lsp
        emacs-consult-org-roam
        emacs-corfu
        emacs-dired-hacks
        emacs-direnv
        emacs-dockerfile-mode
        emacs-doom-modeline
        emacs-doom-themes
        emacs-embark
        emacs-engrave-faces
        emacs-exec-path-from-shell
        emacs-f
        emacs-flycheck
        emacs-flycheck-guile
        emacs-flycheck-haskell
        emacs-flycheck-inline
        emacs-flycheck-package
        emacs-flycheck-rust
        emacs-geiser
        emacs-geiser-guile
        emacs-gnuplot
        emacs-god-mode
        emacs-gptel
        emacs-guix
        emacs-haskell-mode
        emacs-helpful
        emacs-jit-spell
        emacs-json-mode
        emacs-julia-mode
        emacs-kind-icon
        emacs-lsp-mode
        emacs-lsp-ui
        emacs-lua-mode
        emacs-magit
        emacs-marginalia
        emacs-markdown-mode
        emacs-mjolnir-mode
        emacs-modus-themes
        emacs-nerd-icons
        emacs-nerd-icons-completion
        emacs-nerd-icons-corfu
        emacs-nerd-icons-dired
        emacs-nerd-icons-ibuffer
        emacs-no-littering
        emacs-oauth2
        emacs-olivetti
        emacs-orderless
        emacs-org-fragtog
        emacs-org-roam
        emacs-org-roam-ui
        emacs-password-store
        emacs-pdf-tools
        emacs-pinentry
        emacs-rainbow-delimiters
        emacs-rainbow-mode
        emacs-reformatter
        emacs-setup
        emacs-simple-httpd
        emacs-smartparens
        emacs-toml-mode
        emacs-treemacs
        emacs-vertico
        emacs-vterm
        emacs-websocket
        emacs-which-key
        emacs-windsize
        emacs-xref
        emacs-yaml-mode
        emacs-yasnippet))

(define my-home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Add Emacs packages to home environment profile.")
                (extensions (list (service-extension home-profile-service-type
                                   my-home-emacs-profile-service-type)))
                (default-value #f)))
