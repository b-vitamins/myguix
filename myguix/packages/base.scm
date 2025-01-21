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
  #:use-module (gnu packages rust)
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
  #:use-module (myguix packages chrome)
  #:use-module (myguix packages compression)
  #:use-module (myguix packages maths)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages emacs-pqrs)
  #:use-module (myguix packages fonts)
  #:use-module (myguix packages llvm-pqrs)
  #:use-module (myguix packages messaging)
  #:use-module (myguix packages mozilla)
  #:use-module (myguix packages nvidia)
  #:use-module (myguix packages productivity)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages linux)
  #:use-module (myguix packages nlp)
  #:use-module (myguix packages video)
  #:export ( ;Essential bundles
             %core-packages
            %monitoring-packages
            %versioning-packages
            %compression-packages
            %network-packages
            ;; Desktop bundles
            %desktop-packages
            %audio-packages
            %bluetooth-packages
            ;; File system bundles
            %basic-filesystem-packages
            %advanced-filesystem-packages
            %remote-filesystem-packages
            %file-transfer-packages
            ;; Document bundles
            %document-conversion-packages
            %document-production-packages
            ;; Media and graphic bundles
            %media-packages
            %graphics-packages
            ;; Audio bundles
            %audio-conversion-packages
            %audio-production-packages
            ;; Video bundles
            %video-conversion-packages
            %video-production-packages
            ;; Development bundles
            %development-packages
            %cuda-packages
            %tree-sitter-packages
            %guile-packages
            %rust-packages
            %python-packages
            %perl-packages
            ;; Search and Index bundles
            %search-packages
            %opencog-packages
            ;; Font bundles
            %general-fonts
            %document-fonts
            %adobe-fonts
            %apple-fonts
            %google-fonts
            %fira-fonts
            %iosevka-fonts
            %microsoft-fonts
            %monospace-fonts
            %sans-fonts
            %serif-fonts
            %cjk-fonts
            %unicode-fonts))

(define %core-packages
  (list clamav
        coreutils
        binutils
        findutils
        pciutils
        tree
        grep
        sed
        plocate
        shepherd-run
        parallel
        screen
        tmux
        alacritty
        fontconfig
        gnupg
        pinentry
        password-store
        htop
        sysstat
        procps
        atop
        inxi))

(define %monitoring-packages
  (list smartmontools
        hdparm
        blktrace
        nvidia-system-monitor
        nvidia-settings
        nvidia-htop
        python-nvidia-ml-py
        python-py3nvml
        gpustat))

(define %versioning-packages
  (list git git-lfs git-tools git-crypt pass-git-helper))

(define %compression-packages
  (list tar
        zip
        gzip
        zstd
        bzip2
        xz
        unzip
        unrar
        p7zip
        lz4
        lrzip))

(define %network-packages
  (list net-tools
        nmap
        tcpdump
        wireshark
        traceroute
        speedtest-cli
        whois
        openssl
        libnatpmp))

(define %desktop-packages
  (list firefox
        google-chrome-stable
        icedove
        solaar
        gnome-tweaks
        gnome-boxes
        signal-desktop
        zoom
        anytype
        zotero))

(define %audio-packages
  (list pulseaudio
        pavucontrol
        pulsemixer
        alsa-plugins
        pipewire
        wireplumber))

(define %bluetooth-packages
  (list bluez-alsa bluez blueman sbc))

(define %basic-filesystem-packages
  (list parted
        fdisk
        dosfstools
        e2fsprogs
        exfat-utils
        nvme-cli
        bcache-tools
        lsscsi))

(define %advanced-filesystem-packages
  (list xfsprogs
        btrfs-progs
        jfsutils
        f2fs-tools
        ntfs-3g
        ceph
        lvm2
        nfs-utils
        mdadm))

(define %remote-filesystem-packages
  (list filezilla
        sshfs
        davfs2
        cifs-utils
        samba
        wsdd
        ncftp
        vsftpd
        lftp))

(define %file-transfer-packages
  (list wget
        curl
        megacmd
        s3cmd
        rsync
        rclone
        borg
        borgmatic
        axel
        qbittorrent-enhanced
        yt-dlp
        aria2
        nicotine+
        miniupnpc))

(define %document-conversion-packages
  (list pandoc
        img2pdf
        pdfgrep
        pdf2svg
        pdf2djvu
        djvu2pdf
        weasyprint))

(define %document-production-packages
  (list texlive
        scribus
        hunspell
        hunspell-dict-en-us
        hunspell-dict-en-gb-ize
        qpdf
        mupdf
        pdfarranger
        stapler
        diffpdf
        perl-image-exiftool
        pdfposter
        poppler
        calibre))

(define %media-packages
  (list vlc audacious ephoto))

(define %graphics-packages
  (list gimp inkscape imagemagick graphicsmagick))

(define %audio-conversion-packages
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

(define %audio-production-packages
  (list ardour audacity ecasound soundtouch))

(define %video-conversion-packages
  (list handbrake brasero asunder mkvtoolnix))

(define %video-production-packages
  (list blender kdenlive obs))

(define %development-packages
  (list node
        openjdk
        gcc-toolchain
        jemalloc
        llvm-with-bolt
        gdb
        patchelf
        strace
        ltrace
        cmake
        autoconf
        automake
        libtool
        pkg-config))

(define %cuda-packages
  (list cuda-toolkit-12.4
        cudnn-9.5
        cusparselt
        cutlass-headers-3.4
        cutlass-tools-3.4
        cutensor
        nccl
        libfabric-cuda
        psm2-cuda
        tensorpipe-cuda
        magma-cuda
        ffmpeg-cuda
        mpv-cuda
        python-torch-cuda
        python-torchvision-cuda
        whisper-cpp-cuda
        llama-cpp-cuda))

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

(define %guile-packages
  (list guile-3.0 guile-readline guile-colorized guile-lib guile-ares-rs))

(define %rust-packages
  (list rust rust-analyzer rust-cargo rust-clippy))

(define %python-packages
  (list python python-jedi python-jedi-language-server python-flake8
        python-black))

(define %perl-packages
  (list perl perl-critic perltidy))

(define %search-packages
  (list xapian
        omega
        tocc
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

(define %opencog-packages
  (list agi-bio
        opencog
        cogutil
        cogserver
        attention
        atomspace))

(define %general-fonts
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

(define %adobe-fonts
  (list font-adobe-source-han-sans font-adobe-source-code-pro
        font-adobe-source-sans-pro font-adobe-source-serif-pro))

(define %apple-fonts
  (list font-apple-sf-pro
        font-apple-sf-compact
        font-apple-sf-mono
        font-apple-sf-arabic
        font-apple-new-york
        font-apple-sf-symbols
        font-apple-color-emoji
        font-apple-symbols))

(define %google-fonts
  (list font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        font-google-noto-serif-cjk
        font-google-roboto
        font-google-material-design-icons))

(define %fira-fonts
  (list font-fira-sans font-fira-mono font-fira-go font-fira-code))

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

(define %microsoft-fonts
  (list font-microsoft-andale-mono
        font-microsoft-arial
        font-microsoft-arial-black
        font-microsoft-comic-sans-ms
        font-microsoft-courier-new
        font-microsoft-georgia
        font-microsoft-impact
        font-microsoft-times-new-roman
        font-microsoft-trebuchet-ms
        font-microsoft-verdana
        font-microsoft-webdings))

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

(define %sans-fonts
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
