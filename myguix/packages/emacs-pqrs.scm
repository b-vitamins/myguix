(define-module (myguix packages emacs-pqrs)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

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
