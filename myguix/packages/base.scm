(define-module (myguix packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages music)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nicotine)
  #:use-module (gnu packages node)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-xyz)
                #:hide (python-jedi))
  #:use-module (gnu packages rsync)
  #:use-module ((gnu packages rust)
                #:hide (rust))
  #:use-module (gnu packages screen)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xorg)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (myguix packages llvm-pqrs)
  #:use-module (myguix packages mozilla)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages rust)
  #:export (%system-core-packages %compression-tools-packages
                                  %filesystem-management-packages
                                  %terminal-tools-packages
                                  %network-tools-packages
                                  %development-tools-packages
                                  %rust-development-packages
                                  %python-development-packages
                                  %guile-development-packages
                                  %perl-development-packages
                                  %emacs-core-packages
                                  %language-support-packages
                                  %system-monitoring-packages
                                  %security-tools-packages
                                  %media-tools-packages
                                  %desktop-environment-packages
                                  %document-formatting-packages))

;; System Core Packages
(define %system-core-packages
  (list coreutils
        binutils
        findutils
        tree
        grep
        sed
        plocate))

;; Compression and archiving tools
(define %compression-tools-packages
  (list tar
        gzip
        zstd
        bzip2
        xz
        unzip))

;; Filesystem and partition management tools
(define %filesystem-management-packages
  (list parted dosfstools e2fsprogs exfat-utils))

;; Shell and terminal utilities
(define %terminal-tools-packages
  (list screen tmux alacritty pandoc python-gmap))

;; Networking and file sharing tools
(define %network-tools-packages
  (list wget
        curl
        rsync
        rclone
        borg
        borgmatic
        nmap
        tcpdump
        wireshark
        traceroute
        net-tools
        whois
        nicotine+
        deluge
        qbittorrent-enhanced
        transmission
        megacmd
        openssl
        speedtest-cli))

;; General development tools
(define %development-tools-packages
  (list git
        cmake
        autoconf
        automake
        libtool
        openjdk
        pkg-config
        gcc-toolchain
        strace
        ltrace
        llvm-with-bolt-17
        jemalloc
        node))

;; Rust development tools
(define %rust-development-packages
  (list rust rust-analyzer))

;; Python development tools
(define %python-development-packages
  (list python python-jedi python-jedi-language-server python-flake8
        python-black))

;; Guile development tools
(define %guile-development-packages
  (list guile-3.0 guile-readline guile-colorized))

;; Perl development tools
(define %perl-development-packages
  (list perl perl-critic perltidy))

;; Emacs core packages and plugins
(define %emacs-core-packages
  (list emacs-setup
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

;; Tree-sitter packages for language parsing
(define %language-support-packages
  (list tree-sitter tree-sitter-cli tree-sitter-rust tree-sitter-python
        tree-sitter-scheme))

;; System monitoring and performance tools
(define %system-monitoring-packages
  (list htop sysstat procps atop inxi))

;; Encryption and password management tools
(define %security-tools-packages
  (list password-store))

;; Media and image manipulation tools
(define %media-tools-packages
  (list vlc
        yt-dlp
        imagemagick
        blender
        kdenlive
        gimp
        audacity
        inkscape
        obs
        audacious))

;; Desktop environment utilities
(define %desktop-environment-packages
  (list firefox gnome-tweaks gnome-boxes solaar xvfb-run))

;; TeX and document formatting tools
(define %document-formatting-packages
  (list texlive hunspell hunspell-dict-en-us hunspell-dict-en-gb-ize))
