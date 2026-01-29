(define-module (myguix packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages antivirus)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages check)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mold)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages nicotine)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages raspberry-pi)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scsi)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (myguix packages chrome)
  #:use-module (myguix packages linux)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages maths)
  #:use-module (myguix packages mozilla)
  #:use-module (myguix packages nlp)
  #:use-module (myguix packages llm)
  #:use-module (myguix packages node-pqrs)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages productivity)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages video)
  #:export ( ;Core System Bundles
             %core-minimal
            %core-extended
            %shell-modern
            %shell-zsh
            %terminal-essentials
            %text-editors
            %security-essentials
            %ai-assistants
            ;; System Management
            %system-monitoring
            %hardware-monitoring
            %nvidia-monitoring
            %system-debugging
            %performance-profiling
            %benchmark-tools
            ;; Development - Core
            %build-essentials
            %compiler-toolchains
            %version-control
            %compression-tools
            ;; Containers
            %container-tools
            ;; Development - Languages
            %c-cpp-development
            %rust-development
            %python-development
            %python-profiling
            %haskell-development
            %clojure-development
            %guile-development
            %perl-development
            ;; Language Servers
            %language-servers-core
            %language-servers-extended
            ;; Development - Support
            %tree-sitter-core
            %tree-sitter-extended
            %documentation-tools
            %cuda-packages
            %isaac-sim-packages
            %ml-packages
            ;; Networking
            %network-core
            %network-diagnostics
            %network-performance
            ;; File Management
            %filesystem-core
            %filesystem-advanced
            %cloud-sync
            %backup-tools
            %file-sharing
            %download-tools
            ;; Desktop
            %desktop-browsers
            %desktop-core
            %audio-system
            %bluetooth-system
            ;; Media
            %media-players
            %media-editors
            %media-converters
            ;; Documents
            %latex-core
            %latex-extended
            %document-conversion-packages
            %spell-checkers
            %note-taking
            ;; Math/Science
            %math-core
            %math-applications
            %scientific-computing
            ;; Fonts
            %fonts-essential
            %fonts-programming
            %fonts-document
            %fonts-international))

;; Core System Bundles
(define %core-minimal
  (list coreutils
        binutils
        findutils
        grep
        sed
        bc
        patch))

(define %core-extended
  (list tree
        pciutils
        ripgrep
        fd
        mumi
        parallel
        xdg-utils))

(define %shell-modern
  (list fzf
        bat
        eza
        btop
        zoxide
        jq
        du-dust))

(define %shell-zsh
  (list fzf
        bat
        eza
        ripgrep
        fd
        zoxide
        direnv
        wl-clipboard))

(define %terminal-essentials
  (list alacritty tmux))

(define %text-editors
  (list neovim google-antigravity))

(define %security-essentials
  (list clamav gnupg pinentry-gnome3 password-store))

(define %ai-assistants
  (list node-openai-codex node-anthropic-ai-claude-code))

;; System Management Bundles
(define %system-monitoring
  (list atop btop inxi sysstat procps))

(define %hardware-monitoring
  (list smartmontools hdparm blktrace lsscsi nvme-cli))

(define %nvidia-monitoring
  (list nvidia-system-monitor nvidia-settings nvidia-htop python-nvidia-ml-py
        python-py3nvml))

;; Development - Core
(define %version-control
  (list git
        (list git "send-email") git-crypt pre-commit))

(define %compression-tools
  (list tar
        gzip
        zstd
        (list zstd "lib")
        xz
        p7zip
        lz4))

(define %container-tools
  (list podman
        podman-compose
        docker-compose
        buildah
        skopeo
        crun
        conmon
        slirp4netns
        fuse-overlayfs
        netavark
        aardvark-dns))

;; Networking Bundles
(define %network-core
  (list curl wget network-manager-applet))

(define %network-diagnostics
  (list nmap tcpdump traceroute mtr))

(define %network-performance
  (list iperf socat libnatpmp))

;; Desktop Bundles
(define %desktop-browsers
  (list firefox google-chrome-stable))

(define %desktop-core
  (list fontconfig network-manager-applet))

(define %audio-system
  (list pipewire wireplumber))

(define %bluetooth-system
  (list bluez blueman sbc))

;; File Management Bundles
(define %filesystem-core
  (list parted
        fdisk
        dosfstools
        e2fsprogs
        exfat-utils
        bcache-tools
        rpi-imager))

;; Math/Science Bundles
(define %math-core
  (list calc openblas lapack))

(define %math-applications
  (list maxima wxmaxima))

(define %scientific-computing
  (list gnuplot hdf5 nlopt ipopt nauty))

(define %filesystem-advanced
  (list xfsprogs btrfs-progs f2fs-tools lvm2 mdadm))

(define %file-sharing
  (list rsync
        sshfs
        davfs2
        cifs-utils
        samba
        wsdd
        nfs-utils))

(define %cloud-sync
  (list rclone))

(define %backup-tools
  (list borg restic))

(define %download-tools
  (list aria2 yt-dlp qbittorrent-enhanced nicotine+ miniupnpc))

(define %document-conversion-packages
  (list pandoc
        pdfgrep
        pdf2svg
        pdf2djvu
        djvu2pdf))

;; Documentation and Documents
(define %documentation-tools
  (list plantuml graphviz))

(define %latex-core
  (list texlive-scheme-full))

(define %latex-extended
  (list texlive-latexindent texlive-tools))

(define %spell-checkers
  (list hunspell hunspell-dict-en-us hunspell-dict-en-gb-ize))

(define %note-taking
  (list anytype obsidian zotero))

;; Media Bundles
(define %media-players
  (list audacious))

(define %media-editors
  (list blender obs))

(define %media-converters
  (list imagemagick
        sox
        lame
        flac
        opus
        opus-tools
        opustags
        r128gain
        mkvtoolnix
        ecasound
        soundtouch))

(define %build-essentials
  (list gnu-make
        cmake
        cmakelang
        automake
        autoconf
        direnv
        cloc
        tokei
        shellcheck
        shfmt))

(define %compiler-toolchains
  (list clang mold))

(define %system-debugging
  (list strace gdb valgrind))

(define %performance-profiling
  (list perf flamegraph sysprof))

(define %benchmark-tools
  (list hyperfine))

;; Language Servers
(define %language-servers-core
  (list rust-analyzer ccls node-pyright gopls))

(define %language-servers-extended
  (list python-ruff-lsp guile-lsp-server ocaml-lsp-server vala-language-server))

(define %cuda-packages
  (list cuda-toolkit
        cudnn
        cusparselt
        cutlass-headers
        cutlass-tools
        cutensor
        libfabric-cuda
        nvtx
        psm2-cuda
        ffmpeg-nvidia
        mpv-cuda))

(define %isaac-sim-packages
  (list isaac-sim
        isaac-sim-webrtc-client
        isaac-sim-assets-robots-sensors
        isaac-sim-assets-materials-props
        isaac-sim-assets-environments))

(define %ml-packages
  (list nccl
        python-pytorch-cuda
        python-torchvision-cuda
        python-lion-pytorch-cuda
        python-torchaudio-cuda
        python-chatterbox-tts-cuda
        python-torchcodec-cuda
        python-torchmetrics-cuda
        python-ray
        mujoco))

;; Tree-sitter Bundles
(define %tree-sitter-core
  (list tree-sitter
        tree-sitter-cli
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-cuda
        tree-sitter-python
        tree-sitter-javascript
        tree-sitter-typescript
        tree-sitter-rust
        tree-sitter-go
        tree-sitter-java
        tree-sitter-json
        tree-sitter-toml
        tree-sitter-yaml
        tree-sitter-html
        tree-sitter-css
        tree-sitter-markdown
        tree-sitter-bash))

(define %tree-sitter-extended
  (list tree-sitter-awk
        tree-sitter-bibtex
        tree-sitter-blueprint
        tree-sitter-clojure
        tree-sitter-cmake
        tree-sitter-c-sharp
        tree-sitter-devicetree
        tree-sitter-dockerfile
        tree-sitter-elixir
        tree-sitter-elm
        tree-sitter-erlang
        tree-sitter-gomod
        tree-sitter-haskell
        tree-sitter-hcl
        tree-sitter-heex
        tree-sitter-julia
        tree-sitter-kdl
        tree-sitter-kotlin
        tree-sitter-latex
        tree-sitter-lua
        tree-sitter-matlab
        tree-sitter-meson
        tree-sitter-nix
        tree-sitter-ocaml
        tree-sitter-org
        tree-sitter-php
        tree-sitter-plantuml
        tree-sitter-prisma
        tree-sitter-r
        tree-sitter-racket
        tree-sitter-ron
        tree-sitter-ruby
        tree-sitter-scala
        tree-sitter-scheme
        tree-sitter-sway
        tree-sitter-tlaplus
        tree-sitter-ungrammar
        tree-sitter-verilog
        tree-sitter-vhdl))

(define %guile-development
  (list guile-3.0 guile-readline guile-colorized guile-lib guile-ares-rs))

;; Development - Languages
(define %c-cpp-development
  (list clang gdb valgrind ccls))

(define %rust-development
  (list `(,rust "out")
        `(,rust "rust-src")
        `(,rust "tools")
        `(,rust "cargo")))

(define %python-development
  (list poetry
        python
        python-flake8
        python-black
        python-ruff
        python-pytest
        python-pyclean
        python-mutmut))

(define %python-profiling
  (list python-memory-profiler python-line-profiler py-spy))

(define %perl-development
  (list perl perl-critic perl-tidy))

(define %haskell-development
  (list ghc cabal-install))

(define %clojure-development
  (list clojure clojure-tools))

;; Font Bundles
(define %fonts-essential
  (list font-dejavu font-bitstream-vera font-liberation fontconfig))

(define %fonts-programming
  (list font-iosevka
        font-iosevka-term
        font-fira-code
        font-fira-go
        font-fira-mono
        font-fira-sans
        font-jetbrains-mono
        font-inconsolata
        font-hack
        font-adobe-source-code-pro))

(define %fonts-document
  (list font-libre-franklin
        font-latin-modern
        font-charter
        font-tex-gyre
        font-linuxlibertine
        font-google-roboto))

(define %fonts-international
  (list font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        font-google-noto-serif-cjk
        font-wqy-zenhei
        font-sarasa-gothic
        font-gnu-unifont))
