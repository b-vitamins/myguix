(define-module (myguix packages emacs-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tex)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils))

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

(define-public emacs-arxiv-mode
  (package
    (name "emacs-arxiv-mode")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fizban007/arxiv-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dbsi5343zw7dn6qq8226iamb12nma8k9lqbvd9qlhq8k62pswas"))))
    (build-system emacs-build-system)
    (inputs (list emacs-hydra))
    (home-page "https://github.com/fizban007/arxiv-mode")
    (synopsis
     "arxiv-mode is an Emacs major mode for viewing updates on arXiv.org.")
    (description
     "@code{arxiv-mode} provides many functions for accessing arXiv.org. To browse the daily new submissions list in a category, run @code{M-x arxiv-read-new}. To browse the recent (weekly) submissions, run @code{M-x arxiv-read-recent}. Use @{M-x arxiv-read-author} to search for specific author(s). Use @code{M-x arxiv-search} to perform a simple search on the arXiv database.

For more complicated searches, use @code{M-x arxiv-complex-search}. This command allows user to dynamically refine and modify search conditions. You can also use `r' to refine search condition in the abstract list obtained from a search.

In the article list, use `n' and `p' to navigate the article list. Press `SPC' to toggle visibility of the abstract window. Press `RET' to open the entry in a web browser. Press `d' to download the pdf. Press `b' to export the bibtex entry of current paper to your specified .bib file. Press B to export the bibtex entry to a new buffer. Press `e' to download pdf and add a bibtex entry with a link to the actual pdf file.

All available commands are listed in a hydra help menu accessible by pressing `?' whenever you are in the article list.")
    (license license:gpl3+)))

(define-public emacs-org-roam-ui
  (package
    (name "emacs-org-roam-ui")
    (version "5ac7496")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/org-roam/org-roam-ui")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yic5rgp4f1rmi979if79kva7wn3rqnky423mqgf7sdw310h8akl"))))
    (build-system emacs-build-system)
    (inputs (list emacs-org-roam emacs-simple-httpd emacs-websocket))
    (home-page "https://github.com/org-roam/org-roam-ui")
    (synopsis
     "a graphical frontend for your @url{https://github.com/org-roam/org-roam, org-roam} Zettelkasten.")
    (description
     "Org-Roam-UI is a frontend for exploring and interacting with your @url{https://github.com/org-roam/org-roam, org-roam} notes. It is meant to be a successor of @url{https://github.com/org-roam/org-roam-server, org-roam-server} that extends functionality of org-roam with a Web app that runs side-by-side with Emacs.")
    (license license:gpl3+)))

(define-public emacs-latex-snippets
  (package
    (name "emacs-latex-snippets")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/b-vitamins/latex-snippets")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7g8gw1yikgm5j1zwz67z1612kxsbh8cknvxg63p6ljyjbkqq62"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'install-snippets
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Define the destination directory for the snippets
                      (let ((snippet-dir (string-append (assoc-ref outputs
                                                                   "out")
                                          "/share/emacs/site-lisp/latex-snippets-"
                                          ,version "/org-mode")))
                        ;; Create the directory
                        (mkdir-p snippet-dir)
                        ;; Copy the org-mode directory from the source to the destination
                        (copy-recursively "org-mode" snippet-dir) #t))))))
    (inputs (list emacs-yasnippet))
    (home-page "https://github.com/b-vitamins/latex-snippets")
    (synopsis "LaTeX YASnippet collection")
    (description
     "LaTeX YASnippet collection (not only) following the 'Short Math Guide for LaTeX' by Michael Downes and Barbara Beeton.")
    (license license:gpl3+)))

(define-public emacs-org-next
  (let* ((commit "cd2269ddb64bda7203acf2ee2e26188237a578ea")
         (revision "1")
         (version (git-version "9.8.0" revision commit)))
    (package
      (inherit emacs-org)
      (name "emacs-org-next")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.tecosaur.net/tec/org-mode.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13mfs7nqqia9j2d27qfb14ag13niyw9azdhgirwc0j41glq23gkk"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-org)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'fix-tests 'fix-more-tests
                (lambda* (#:key inputs #:allow-other-keys)
                  (with-directory-excursion "testing/lisp"
                    ;; /usr/bin/env is absent in the build container
                    (substitute* "test-ob-shell.el"
                      (("cmdline .*" all)
                       (string-append all "  (skip-unless nil)\n")))
                    ;; printf corner‑cases that break under dash/BusyBox
                    (substitute* "test-org.el"
                      (("test-org/create-math-formula .*" all)
                       (string-append all "  (skip-unless nil)\n"))
                      (("test-org/format-latex-as-html .*" all)
                       (string-append all "  (skip-unless nil)\n"))))))))))
      (native-inputs (modify-inputs (package-native-inputs emacs-org)
                       (append texlive-scheme-basic texlive-latex-bin
                               texlive-beamer dvisvgm imagemagick)))
      (synopsis "Outline-based notes manager — development branch")
      (description
       "This package tracks Org mode’s development branch that contains the new asynchronous LaTeX preview engine and other unreleased features.")
      (home-page "https://git.tecosaur.net/tec/org-mode")
      (license license:gpl3+))))
