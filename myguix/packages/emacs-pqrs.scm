(define-module (myguix packages emacs-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages emacs-xyz))

(define-public my-emacs
  (package
    (inherit emacs-pgtk)
    (name "my-emacs")
    (synopsis "Emacs text editor with @code{pgtk} support")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-pgtk)
       ((#:configure-flags flags
         #~'())
        #~(cons* "--with-tree-sitter" "--with-json" "--with-threads"
                 #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'optimize-flags
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((clang (assoc-ref inputs "clang"))
                      (llvm (assoc-ref inputs "llvm")))
                  (setenv "CC"
                          (string-append clang "/bin/clang"))
                  (setenv "CXX"
                          (string-append clang "/bin/clang++"))
                  (setenv "CFLAGS" "-O3 -march=native -mtune=native")
                  (setenv "CXXFLAGS" "-O3 -march=native -mtune=native"))))))))
    (inputs (modify-inputs (package-inputs emacs-pgtk)
              (prepend clang-17)))))

(define-public emacs-mjolnir-mode
  (package
    (name "emacs-mjolnir-mode")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/mjolnir-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19s6l04kk9sgix23gggx8kmh8rl82lxiany9w9zmmsy96zjii8hq"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/b-vitamins/mjolnir-mode")
    (synopsis
     "Whosoever holds this hammer, if he be worthy, shall possess the power of Thor.")
    (description
     "When wielding Mjölnir, nothing shall come in the way of your buffers as they thunder through your windows. Instead of moving over to the window holding the buffer worthy of your attention, summon it into the window you're already in. However, you deem not all buffers as worthy - let be them smitten under the might of Mjölnir - and they shall stay their ground.")
    (license license:gpl3+)))

(define-public emacs-nerd-icons-dired
  (package
    (name "emacs-nerd-icons-dired")
    (version "c1c7348")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rainstormstudio/nerd-icons-dired")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ln73ii7c3chl4lvarwiwrdmx49q528wc0h6a7xbl68pc2pyyvq2"))))
    (build-system emacs-build-system)
    (inputs (list emacs-nerd-icons))
    (home-page "https://github.com/rainstormstudio/nerd-icons-dired")
    (synopsis "Icon enhancements for Dired using Nerd Fonts")
    (description
     "This package provides Nerd Font icon enhancements for Emacs' Dired mode, offering visually distinct file icons to improve navigation and aesthetics.")
    (license license:gpl3+)))

(define-public emacs-nerd-icons-ibuffer
  (package
    (name "emacs-nerd-icons-ibuffer")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seagle0128/nerd-icons-ibuffer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wj6kcgvh700maj9i5pmgzc48lbj0dbxx849a8w519m4anr7b23s"))))
    (build-system emacs-build-system)
    (inputs (list emacs-nerd-icons))
    (home-page "https://github.com/seagle0128/nerd-icons-ibuffer")
    (synopsis "Nerd Fonts icon theme for Emacs iBuffer")
    (description
     "Integrates Nerd Fonts into Emacs iBuffer, enhancing its interface by displaying appropriate icons for different file types and buffers.")
    (license license:gpl3+)))

(define-public emacs-nerd-icons-completion
  (package
    (name "emacs-nerd-icons-completion")
    (version "c2db855")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rainstormstudio/nerd-icons-completion")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10ll0dj6ym5prrkv6smj0ac2ail4b3rqcrh1lyr61y3cj422vn9z"))))
    (inputs (list emacs-nerd-icons))
    (build-system emacs-build-system)
    (home-page "https://github.com/rainstormstudio/nerd-icons-completion")
    (synopsis "Icon completion using Nerd Fonts in Emacs")
    (description
     "This package uses Nerd Fonts to provide icon completion features within Emacs, helping users visually identify file types and other items quickly.")
    (license license:gpl3+)))

(define-public emacs-nerd-icons-corfu
  (package
    (name "emacs-nerd-icons-corfu")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LuigiPiucco/nerd-icons-corfu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hnq6yv0xcisk5vkdzjz2sdzn4cayirf3zyz40xj1pzf33lra4r"))))
    (inputs (list emacs-nerd-icons))
    (build-system emacs-build-system)
    (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
    (synopsis "Nerd Fonts integration with Emacs Corfu completion framework")
    (description
     "Adds Nerd Font icons to the Corfu completion framework in Emacs, enhancing the visual experience by decorating completions with contextually relevant icons.")
    (license license:gpl3+)))

(define-public emacs-flycheck-inline
  (package
    (name "emacs-flycheck-inline")
    (version "8e00b4c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flycheck/flycheck-inline")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s505lk5rdc3p40w5g4cpzviaksclvfdisl457gpwjpjv0m7fwd4"))))
    (build-system emacs-build-system)
    (inputs (list emacs-flycheck))
    (home-page "https://github.com/flycheck/flycheck-inline")
    (synopsis "Flycheck errors using overlays.")
    (description
     "An extension for Flycheck. It implements a minor-mode for displaying errors from Flycheck right below their reporting location, using overlays.")
    (license license:gpl3+)))

(define-public emacs-lsp-jedi
  (package
    (name "emacs-lsp-jedi")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fredcamps/lsp-jedi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kax988drrzsx103v71bz4vczh4bg0n80arrzm4r6sgrk01219j0"))))
    (build-system emacs-build-system)
    (inputs (list emacs-lsp-mode))
    (home-page "https://github.com/fredcamps/lsp-jedi")
    (synopsis
     "A Emacs client for Python Jedi Language Server (LSP client plugin for lsp-mode Emacs extension).")
    (description
     "A Emacs client for Python Jedi Language Server. Supported features:
@itemize
@item Renaming/Refactoring
@item Auto-completion
@item Definitions
@item References
@item Document Highlight
@item Document Symbols
@item Hover Request
@item Publish Diagnostics
@item Signature Help
@item Symbols
@end itemize")
    (license license:gpl3+)))

(define-public emacs-lsp-haskell
  (package
    (name "emacs-lsp-haskell")
    (version "47a1878")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-haskell")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13b4ikn392dm0d1f4i3lzb0gibljww2npvsbyxdkrn58gf1q4qfy"))))
    (build-system emacs-build-system)
    (inputs (list emacs-lsp-mode emacs-haskell-mode))
    (home-page "https://github.com/emacs-lsp/lsp-haskell")
    (synopsis "A Emacs client for @code{haskell-language-server}.")
    (description
     "An Emacs Lisp library for interacting with a Haskell language server such as @code{haskell-language-server} using Microsoft's Language Server Protocol. The library acts as a client for @code{lsp-mode}.")
    (license license:gpl3+)))
