(define-module (myguix packages)
  #:use-module (gnu packages)
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
  (list (specification->package "coreutils")
        (specification->package "binutils")
        (specification->package "findutils")
        (specification->package "grep")
        (specification->package "sed")
        (specification->package "mlocate")))

;; Compression and archiving tools
(define %compression-tools-packages
  (list (specification->package "tar")
        (specification->package "gzip")
        (specification->package "zstd")
        (specification->package "bzip2")
        (specification->package "xz")
        (specification->package "unzip")))

;; Filesystem and partition management tools
(define %filesystem-management-packages
  (list (specification->package "parted")
        (specification->package "dosfstools")
        (specification->package "e2fsprogs")
        (specification->package "exfat-utils")))

;; Shell and terminal utilities
(define %terminal-tools-packages
  (list (specification->package "screen")
        (specification->package "tmux")
        (specification->package "alacritty")))

;; Networking and file sharing tools
(define %network-tools-packages
  (list (specification->package "wget")
        (specification->package "curl")
        (specification->package "rsync")
        (specification->package "nmap")
        (specification->package "tcpdump")
        (specification->package "wireshark")
        (specification->package "traceroute")
        (specification->package "iproute2")
        (specification->package "net-tools")
        (specification->package "whois")
        (specification->package "nicotine+")
        (specification->package "deluge")))

;; General development tools
(define %development-tools-packages
  (list (specification->package "git")
        (specification->package "make")
        (specification->package "cmake")
        (specification->package "autoconf")
        (specification->package "automake")
        (specification->package "libtool")
        (specification->package "openjdk")
        (specification->package "pkg-config")
        (specification->package "gcc-toolchain")
        (specification->package "strace")
        (specification->package "ltrace")
        (specification->package "llvm-with-bolt")
        (specification->package "jemalloc")
        (specification->package "node")))

;; Rust development tools
(define %rust-development-packages
  (list (specification->package "rust")
        (specification->package "rust-analyzer")))

;; Python development tools
(define %python-development-packages
  (list (specification->package "python")
        (specification->package "python-jedi")
        (specification->package "python-jedi-language-server")
        (specification->package "python-flake8")
        (specification->package "python-black")))

;; Guile development tools
(define %guile-development-packages
  (list (specification->package "guile")
        (specification->package "guile-readline")
        (specification->package "guile-colorized")))

;; Perl development tools
(define %perl-development-packages
  (list (specification->package "perl")
        (specification->package "perl-critic")
        (specification->package "perltidy")))

;; Emacs core packages and plugins
(define %emacs-core-packages
  (list (specification->package "emacs-next")
        (specification->package "emacs-modus-themes")
        (specification->package "emacs-doom-themes")
        (specification->package "emacs-doom-modeline")
        (specification->package "emacs-rainbow-delimiters")
        (specification->package "emacs-rainbow-mode")
        (specification->package "emacs-adaptive-wrap")
        (specification->package "emacs-smartparens")
        (specification->package "emacs-paredit")
        (specification->package "emacs-nerd-icons")
        (specification->package "emacs-nerd-icons-dired")
        (specification->package "emacs-nerd-icons-corfu")
        (specification->package "emacs-nerd-icons-ibuffer")
        (specification->package "emacs-nerd-icons-completion")
        (specification->package "emacs-kind-icon")
        (specification->package "emacs-all-the-icons")
        (specification->package "emacs-no-littering")
        (specification->package "emacs-guix")
        (specification->package "emacs-mjolnir-mode")
        (specification->package "emacs-windsize")
        (specification->package "emacs-ace-window")
        (specification->package "emacs-olivetti")
        (specification->package "emacs-which-key")
        (specification->package "emacs-org-fragtog")
        (specification->package "emacs-vertico")
        (specification->package "emacs-orderless")
        (specification->package "emacs-marginalia")
        (specification->package "emacs-consult")
        (specification->package "emacs-corfu")
        (specification->package "emacs-embark")
        (specification->package "emacs-citar")
        (specification->package "emacs-org-roam")
        (specification->package "emacs-org-roam-ui")
        (specification->package "emacs-websocket")
        (specification->package "emacs-simple-httpd")
        (specification->package "emacs-pdf-tools")
        (specification->package "emacs-dired-hacks")
        (specification->package "emacs-oauth2")
        (specification->package "emacs-pinentry")
        (specification->package "emacs-auth-source-pass")
        (specification->package "emacs-password-store")
        (specification->package "emacs-yasnippet")
        (specification->package "emacs-geiser")
        (specification->package "emacs-geiser-guile")
        (specification->package "emacs-lsp-mode")
        (specification->package "emacs-lsp-ui")
        (specification->package "emacs-flycheck")
        (specification->package "emacs-flycheck-inline")
        (specification->package "emacs-flycheck-guile")
        (specification->package "emacs-flycheck-package")
        (specification->package "emacs-flycheck-rust")
        (specification->package "emacs-flycheck-haskell")
        (specification->package "emacs-lsp-jedi")
        (specification->package "emacs-lsp-haskell")
        (specification->package "emacs-rust-mode")
        (specification->package "emacs-rustic")
        (specification->package "emacs-haskell-mode")
        (specification->package "emacs-gnuplot")
        (specification->package "emacs-lua-mode")
        (specification->package "emacs-json-mode")
        (specification->package "emacs-dockerfile-mode")
        (specification->package "emacs-yaml-mode")
        (specification->package "emacs-toml-mode")
        (specification->package "emacs-julia-mode")
        (specification->package "emacs-cmake-mode")
        (specification->package "emacs-god-mode")
        (specification->package "emacs-gptel")
        (specification->package "emacs-magit")
        (specification->package "emacs-auctex")
        (specification->package "emacs-jit-spell")
        (specification->package "emacs-web-mode")
        (specification->package "emacs-combobulate")
        (specification->package "emacs-markdown-mode")
        (specification->package "emacs-vterm")
        (specification->package "emacs-vterm-toggle")
        (specification->package "emacs-projectile")
        (specification->package "emacs-dashboard")
        (specification->package "emacs-treemacs")
        (specification->package "emacs-treemacs-extra")
        (specification->package "emacs-lsp-treemacs")
        (specification->package "emacs-lispy")
        (specification->package "emacs-org-modern")
        (specification->package "emacs-helpful")
        (specification->package "emacs-restart-emacs")
        (specification->package "emacs-ripgrep")
        (specification->package "emacs-rg")
        (specification->package "emacs-corfu-candidate-overlay")
        (specification->package "emacs-cape")
        (specification->package "emacs-ac-geiser")
        (specification->package "emacs-iedit")
        (specification->package "emacs-multiple-cursors")
        (specification->package "emacs-arxiv-mode")
        (specification->package "emacs-latex-snippets")))

;; Tree-sitter packages for language parsing
(define %language-support-packages
  (list (specification->package "tree-sitter")
        (specification->package "tree-sitter-cli")
        (specification->package "tree-sitter-rust")
        (specification->package "tree-sitter-python")
        (specification->package "tree-sitter-scheme")))

;; System monitoring and performance tools
(define %system-monitoring-packages
  (list (specification->package "htop")
        (specification->package "sysstat")
        (specification->package "procps")
        (specification->package "atop")
        (specification->package "inxi")))

;; Encryption and password management tools
(define %security-tools-packages
  (list (specification->package "password-store")))

;; Media and image manipulation tools
(define %media-tools-packages
  (list (specification->package "vlc")
        (specification->package "yt-dlp")
        (specification->package "imagemagick")
        (specification->package "blender")
        (specification->package "kdenlive")
        (specification->package "gimp")
        (specification->package "audacity")
        (specification->package "inkscape")
        (specification->package "obs")
        (specification->package "audacious")))

;; Desktop environment utilities
(define %desktop-environment-packages
  (list (specification->package "firefox")
        (specification->package "gnome-tweaks")
        (specification->package "gnome-boxes")
        (specification->package "solaar")))

;; TeX and document formatting tools
(define %document-formatting-packages
  (list (specification->package "texlive")))
