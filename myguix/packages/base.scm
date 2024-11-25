(define-module (myguix packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages antivirus)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ftp)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gpodder)
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
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages nicotine)
  #:use-module (gnu packages node)
  #:use-module (gnu packages opencog)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module ((gnu packages python-xyz)
                #:hide (python-jedi))
  #:use-module (gnu packages rsync)
  #:use-module ((gnu packages rust)
                #:hide (rust))
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages scribus)
  #:use-module (gnu packages scsi)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages storage)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (myguix packages llvm-pqrs)
  #:use-module (myguix packages mozilla)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages rust)
  #:use-module (myguix packages video)
  #:export (%system-core-packages %nvidia-core-packages
                                  %cuda-accelerated-packages
                                  %bluetooth-packages
                                  %sound-system-packages
                                  %search-and-index-packages
                                  %terminal-tools-packages
                                  %secret-mgmt-packages
                                  %desktop-utilities-packages
                                  %system-monitoring-packages
                                  %basic-filesystem-tools
                                  %ssd-tools
                                  %diagnostic-and-maintenance-tools
                                  %remote-storage-mount-packages
                                  %advanced-filesystem-tools
                                  %compression-tools-packages
                                  %media-consumption-packages
                                  %audio-conversion-tools-packages
                                  %video-conversion-tools-packages
                                  %document-conversion-tools-packages
                                  %graphic-production-packages
                                  %audio-production-packages
                                  %video-production-packages
                                  %document-authoring-packages
                                  %document-manipulation-packages
                                  %file-transfer-tools-packages
                                  %network-analysis-tools-packages
                                  %p2p-file-sharing-packages
                                  %network-utilities-packages
                                  %general-purpose-fonts
                                  %document-fonts
                                  %fira-fonts
                                  %adobe-fonts
                                  %google-fonts
                                  %iosevka-fonts
                                  %monospace-fonts
                                  %sans-serif-fonts
                                  %serif-fonts
                                  %cjk-fonts
                                  %unicode-fonts
                                  %version-control-packages
                                  %build-system-packages
                                  %debugging-tools-packages
                                  %memory-and-optimization-tools-packages
                                  %runtime-packages
                                  %tree-sitter-packages
                                  %guile-development-packages
                                  %rust-development-packages
                                  %python-development-packages
                                  %perl-development-packages
                                  %opencog-packages))

(define %search-and-index-packages
  (list xapian
        omega
        tocc
        searx
        bool
        dataparksearch
        fsearch
        recoll
        plocate
        xapers
        ugrep
        ripgrep
        the-silver-searcher
        cloc))

(define %sound-system-packages
  (list pulseaudio
        pavucontrol
        pulsemixer
        alsa-plugins
        pipewire
        wireplumber))

(define %system-core-packages
  (list clamav
        coreutils
        binutils
        findutils
        pciutils
        tree
        grep
        sed
        plocate
        fontconfig))

(define %nvidia-core-packages
  (list nvidia-system-monitor
        nvidia-settings
        cuda-toolkit-12.1
        cudnn-9.5
        nvidia-htop
        gpustat))

(define %cuda-accelerated-packages
  (list ffmpeg-cuda mpv-cuda))

(define %terminal-tools-packages
  (list parallel screen tmux alacritty))

(define %secret-mgmt-packages
  (list gnupg pinentry password-store))

(define %desktop-utilities-packages
  (list ungoogled-chromium/wayland
        firefox
        icedove
        gpodder
        solaar
        gnome-tweaks
        gnome-boxes))

(define %system-monitoring-packages
  (list htop sysstat procps atop inxi))

(define %basic-filesystem-tools
  (list parted fdisk dosfstools e2fsprogs exfat-utils))

(define %ssd-tools
  (list nvme-cli bcache-tools lsscsi))

(define %diagnostic-and-maintenance-tools
  (list smartmontools hdparm blktrace))

(define %remote-storage-mount-packages
  (list filezilla
        sshfs
        davfs2
        cifs-utils
        samba
        wsdd
        ncftp
        vsftpd
        lftp))

(define %advanced-filesystem-tools
  (list xfsprogs
        btrfs-progs
        jfsutils
        f2fs-tools
        ntfs-3g
        glusterfs
        ceph
        lvm2
        nfs-utils
        mdadm))

(define %compression-tools-packages
  (list tar
        gzip
        zstd
        bzip2
        xz
        unzip
        p7zip
        unrar-free
        lz4
        lrzip))

(define %media-consumption-packages
  (list vlc audacious ephoto))

(define %audio-conversion-tools-packages
  (list abcde
        sox
        lame
        flac
        easytag
        opus
        opus-tools
        opustags
        soundconverter
        r128gain))

(define %video-conversion-tools-packages
  (list handbrake yt-dlp brasero asunder mkvtoolnix))

(define %document-conversion-tools-packages
  (list pandoc
        img2pdf
        pdfgrep
        pdf2svg
        pdf2djvu
        djvu2pdf
        weasyprint))

(define %graphic-production-packages
  (list gimp inkscape imagemagick graphicsmagick))

(define %audio-production-packages
  (list ardour audacity ecasound soundtouch))

(define %video-production-packages
  (list blender kdenlive obs))

(define %document-authoring-packages
  (list texlive scribus hunspell hunspell-dict-en-us hunspell-dict-en-gb-ize))

(define %document-manipulation-packages
  (list qpdf
        mupdf
        pdfarranger
        stapler
        diffpdf
        mediainfo
        perl-image-exiftool
        psutils
        ghostscript
        pdfposter
        poppler
        calibre))

(define %file-transfer-tools-packages
  (list wget
        curl
        megacmd
        s3cmd
        rsync
        rclone
        borg
        borgmatic
        axel))

(define %network-analysis-tools-packages
  (list nmap tcpdump wireshark traceroute speedtest-cli))

(define %p2p-file-sharing-packages
  (list qbittorrent-enhanced aria2 nicotine+ miniupnpc))

(define %bluetooth-packages
  (list bluez-alsa bluez blueman sbc))

(define %network-utilities-packages
  (list net-tools
        whois
        openssl
        libnatpmp
        python-nodriver
        protonvpn-cli))

(define %general-purpose-fonts
  (list font-dejavu font-bitstream-vera font-abattis-cantarell font-liberation
        font-carlito))

(define %document-fonts
  (list font-libre-franklin
        font-latin-modern
        font-charter
        font-tex-gyre
        font-cardo
        font-gfs-ambrosia
        font-cormorant
        font-amiri
        font-sil-gentium
        font-sil-charis))

(define %fira-fonts
  (list font-fira-sans font-fira-mono font-fira-go font-fira-code))

(define %adobe-fonts
  (list font-adobe-source-han-sans font-adobe-source-code-pro
        font-adobe-source-sans-pro font-adobe-source-serif-pro))

(define %google-fonts
  (list font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        font-google-noto-serif-cjk
        font-google-roboto
        font-google-material-design-icons))

(define %iosevka-fonts
  (list font-iosevka
        font-iosevka-slab
        font-iosevka-term
        font-iosevka-term-slab
        font-iosevka-aile
        font-iosevka-curly
        font-iosevka-curly-slab
        font-iosevka-etoile
        font-iosevka-ss08
        font-iosevka-ss09
        font-iosevka-comfy))

(define %monospace-fonts
  (list font-inconsolata
        font-intel-one-mono
        font-borg-sans-mono
        font-jetbrains-mono
        font-juliamono
        font-victor-mono
        font-hack
        font-terminus
        font-anonymous-pro
        font-plemoljp
        font-hermit))

(define %sans-serif-fonts
  (list font-lato
        font-public-sans
        font-dosis
        font-space-grotesk
        font-oswald
        font-catamaran
        font-overpass))

(define %serif-fonts
  (list font-linuxlibertine
        font-libre-franklin
        font-latin-modern
        font-charter
        font-cardo
        font-gfs-ambrosia
        font-cormorant
        font-amiri))

(define %cjk-fonts
  (list font-wqy-zenhei
        font-wqy-microhei
        font-cns11643
        font-cns11643-swjz
        font-sarasa-gothic
        font-lxgw-wenkai
        font-lxgw-wenkai-tc))

(define %unicode-fonts
  (list font-gnu-freefont font-gnu-unifont font-ipa font-ipa-ex))

(define %version-control-packages
  (list git git-lfs git-tools git-crypt pass-git-helper))

(define %build-system-packages
  (list cmake
        autoconf
        automake
        libtool
        openjdk
        pkg-config
        gcc-toolchain))

(define %debugging-tools-packages
  (list gdb patchelf strace ltrace))

(define %memory-and-optimization-tools-packages
  (list jemalloc llvm-with-bolt))

(define %runtime-packages
  (list node openjdk))

(define %tree-sitter-packages
  (list tree-sitter
        tree-sitter-cli
        tree-sitter-rust
        tree-sitter-python
        tree-sitter-scheme
        tree-sitter-org
        tree-sitter-markdown
        tree-sitter-latex
        tree-sitter-json
        tree-sitter-html
        tree-sitter-css
        tree-sitter-cmake
        tree-sitter-bibtex
        tree-sitter-c))

(define %guile-development-packages
  (list guile-3.0 guile-readline guile-colorized guile-lib))

(define %rust-development-packages
  (list rust rust-analyzer))

(define %python-development-packages
  (list python python-jedi python-jedi-language-server python-flake8
        python-black))

(define %perl-development-packages
  (list perl perl-critic perltidy))

(define %opencog-packages
  (list agi-bio
        opencog
        cogutil
        cogserver
        attention
        atomspace))
