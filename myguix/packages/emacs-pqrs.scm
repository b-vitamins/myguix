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
    (synopsis "Emacs text editor with and @code{pgtk} support")
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

(define-public emacs-citar-1.3
  (package
    (name "emacs-citar-1.3")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bdarcus/citar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12chdrmkggnpci1kdkkrz4a2bnsbzc8pra318zbnn3qxinlpngyy"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #t
      #:test-command #~(list "emacs"
                             "--batch"
                             "-L"
                             "."
                             "-l"
                             "test/citar-test.el"
                             "-l"
                             "test/citar-file-test.el"
                             "-l"
                             "test/citar-format-test.el"
                             "-f"
                             "ert-run-tests-batch-and-exit")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'set-home
                     (lambda _
                       (setenv "HOME" "/tmp")))
                   ;; XXX: The following phase reports bogus errors. Suppress
                   ;; it for now.
                   (delete 'validate-compiled-autoloads))))
    (propagated-inputs (list emacs-auctex
                             emacs-citeproc-el
                             emacs-embark
                             emacs-org
                             emacs-parsebib
                             emacs-s))
    (home-page "https://github.com/bdarcus/citar")
    (synopsis "Emacs package to quickly find and act on bibliographic entries")
    (description
     "This package provides a completing-read front-end to browse and
act on BibTeX, BibLaTeX, and CSL JSON bibliographic data, and LaTeX,
markdown, and Org cite editing support.

When used with Vertico (or Selectrum), Embark, and Marginalia, it
provides similar functionality to helm-bibtex and ivy-bibtex: quick
filtering and selecting of bibliographic entries from the minibuffer,
and the option to run different commands against them.

With Embark, it also makes available at-point actions in Org
citations.")
    (license license:gpl3+)))

(define-public emacs-citar-org-roam-0.5
  (package
    (name "emacs-citar-org-roam-0.5")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-citar/citar-org-roam")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iwhwfllbcd938qkvh5m5cn6s8pn01xb02yjbv1hl4jpiayianqa"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-citar emacs-org-roam))
    (home-page "https://github.com/emacs-citar/citar-org-roam")
    (synopsis
     "Emacs package to provide tighter Citar and Org-Roam integration")
    (description
     "Out-of-box, Citar provides default support for file-per-note bibliographic
notes that are compatible with Org-Roam v2.  This package integrates directly
with the Org-Roam database, and provides the following additional features to
Citar note support:
@itemize
@item multiple references per note
@item multiple reference notes per file
@item ability to query note citations by reference
@item ``live'' updating of Citar UI for presence of notes
@end itemize")
    (license license:gpl3+)))

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
    (synopsis "nerd-icons-dired is inspired by all-the-icons-dired.")
    (description "")
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
    (synopsis
     "nerd-icons theme for treemacs. It is inspired by treemacs-all-the-icons, vim-devicons and nvim-web-devicons. It can be used inside GUI or terminal.")
    (description "")
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
    (synopsis
     "nerd-icons theme for treemacs. It is inspired by treemacs-all-the-icons, vim-devicons and nvim-web-devicons. It can be used inside GUI or terminal.")
    (description "")
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
    (build-system emacs-build-system)
    (inputs (list emacs-nerd-icons))
    (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
    (synopsis
     "nerd-icons theme for treemacs. It is inspired by treemacs-all-the-icons, vim-devicons and nvim-web-devicons. It can be used inside GUI or terminal.")
    (description "")
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

my-emacs
