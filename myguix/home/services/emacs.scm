(define-module (myguix home services emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (my-home-emacs-service-type))

(define (my-home-emacs-profile-service-type config)
  (list emacs-next
        emacs-academic-phrases
        emacs-ace-window
        emacs-all-the-icons
        emacs-all-the-icons-completion
        emacs-all-the-icons-dired
        emacs-auctex
        emacs-auctex-latexmk
        emacs-cape
        emacs-cargo
        emacs-cdlatex
        emacs-chronos
        emacs-citar
        emacs-citar-embark
        emacs-citar-org-roam
        emacs-clang-format
        emacs-cmake-mode
        emacs-company-auctex
        emacs-consult
        emacs-consult-eglot
        emacs-corfu
        emacs-dired-rsync
        emacs-dired-hacks
        emacs-eat
        emacs-ednc
        emacs-elfeed
        emacs-elfeed-org
        emacs-embark
        emacs-embark-consult
        emacs-emms
        emacs-eshell-prompt-extras
        emacs-eshell-syntax-highlighting
        emacs-expand-region
        emacs-flyspell-correct
        emacs-flyspell-correct-ivy
        emacs-geiser
        emacs-geiser-eros
        emacs-geiser-guile
        emacs-git-gutter
        emacs-git-gutter-fringe
        emacs-git-link
        emacs-git-timemachine
        emacs-gptel
        emacs-gptel-quick
        emacs-helpful
        emacs-hide-mode-line
        emacs-highlight-indent-guides
        emacs-hl-todo
        emacs-htmlize
        emacs-hydra
        emacs-info+
        emacs-jupyter
        emacs-justify-kp
        emacs-kind-icon
        emacs-langtool
        emacs-latex-snippets
        emacs-lispy
        emacs-magit
        emacs-magit-todos
        emacs-marginalia
        emacs-markdown-mode
        emacs-markdown-toc
        emacs-minions
        emacs-mjolnir-mode
        emacs-modus-themes
        emacs-nerd-icons
        emacs-nerd-icons-completion
        emacs-nerd-icons-dired
        emacs-nerd-icons-ibuffer
        emacs-nov-el
        emacs-olivetti
        emacs-orderless
        emacs-org-appear
        emacs-org-contrib
        emacs-org-make-toc
        emacs-org-modern
        emacs-org-noter
        emacs-org-pomodoro
        emacs-org-ref
        emacs-org-roam
        emacs-org-super-agenda
        emacs-org-wild-notifier
        emacs-ox-html-stable-ids
        emacs-ox-pandoc
        emacs-pandoc-mode
        emacs-pdf-tools
        emacs-perspective
        emacs-poetry
        emacs-pomidor
        emacs-pyenv-mode
        emacs-pytest
        emacs-python-black
        emacs-python-pytest
        emacs-python-test
        emacs-pyvenv
        emacs-rainbow-delimiters
        emacs-rust-mode
        emacs-saveplace-pdf-view
        emacs-sly
        emacs-sly-asdf
        emacs-sly-quicklisp
        emacs-sly-repl-ansi-color
        emacs-smartparens
        emacs-toml-mode
        emacs-use-package
        emacs-vertico
        emacs-visual-fill-column
        emacs-wc-mode
        emacs-webpaste
        emacs-which-key
        emacs-writegood-mode
        emacs-ws-butler
        emacs-zotra))

(define my-home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Add Emacs packages to home environment profile.")
                (extensions (list (service-extension home-profile-service-type
                                   my-home-emacs-profile-service-type)))
                (default-value #f)))
