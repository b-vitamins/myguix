;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((fill-column . 80)             ; Set maximum line width to 80 characters.
     (indent-tabs-mode . nil)       ; Use spaces instead of tabs.
     (tab-width . 2)                ; Set tab width to 2 spaces.
     (sentence-end-double-space . t) ; Require double spaces between sentences.

     ;; Ignore .go files in filename completion.
     (eval . (add-to-list 'completion-ignored-extensions ".go"))

     ;; Set guix-directory to the root of the Guix channel/project.
     (eval . (setq-local guix-directory
                         (locate-dominating-file default-directory
                                                 ".dir-locals.el")))

     ;; Geiser - Separate REPL per project.
     (geiser-repl-per-project-p . t)))

 (scheme-mode
  . ((indent-tabs-mode . nil)
     ;; Custom indentation rules for Scheme specific to Guix.
     (eval . (put 'eval-when 'scheme-indent-function 1))
     (eval . (put 'modify-phases 'scheme-indent-function 1))
     (eval . (put 'replace 'scheme-indent-function 1))
     (eval . (put 'modify-services 'scheme-indent-function 1))
     (eval . (put 'with-imported-modules 'scheme-indent-function 1))
     (eval . (put 'with-store 'scheme-indent-function 1))
     (eval . (put 'package 'scheme-indent-function 0))
     (eval . (put 'origin 'scheme-indent-function 0))
     (eval . (put 'build-system 'scheme-indent-function 0))
     (eval . (put 'operating-system 'scheme-indent-function 0))
     (eval . (put 'manifest-entry 'scheme-indent-function 0))
     (eval . (put 'substitute-keyword-arguments 'scheme-indent-function 1))
     (eval . (put 'with-monad 'scheme-indent-function 1))
     ;; Additional settings for Scheme mode.
     (eval . (modify-syntax-entry ?~ "'"))
     (eval . (modify-syntax-entry ?$ "'"))))

 (emacs-lisp-mode
  . ((indent-tabs-mode . nil)))      ; Use spaces for indentation in Emacs Lisp files.

 (texinfo-mode
  . ((indent-tabs-mode . nil)        ; Use spaces instead of tabs for Texinfo files.
     (fill-column . 72)))            ; Set fill-column for Texinfo to 72 characters.

 (c-mode
  . ((c-file-style . "gnu")          ; Use GNU coding style for C files.
     (tab-width . 8)                 ; Set tab width to 8 spaces for C files.
     (indent-tabs-mode . t))))       ; Use tabs for indentation in C files.

