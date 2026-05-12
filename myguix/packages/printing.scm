;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (myguix packages printing)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (myguix utils)
  #:use-module ((myguix licenses)
                #:prefix myguix-license:))

(define-public brother-hlt4000dw
  (let ((commit "6299dd0d9856be8afb198ec92a7d73563be77600"))
    (package
      (name "brother-hlt4000dw")
      (version "1.0.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/b-vitamins/brother-hlt4000dw-driver")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0m7m2b3j607c23x9vvbsw7nq578pjaa01n6cjrjxwxpk24841cj6"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:modules
        '((guix build gnu-build-system)
          (guix build utils)
          (ice-9 match)
          (ice-9 popen)
          (ice-9 rdelim)
          (srfi srfi-1))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'build)
            (replace 'install
              (lambda _
                (let* ((out #$output)
                       (vendor-src "src/opt/brother/Printers/hlt4000dw")
                       (opt-dir
                        (string-append out "/opt/brother/Printers/hlt4000dw"))
                       (bin-dir (string-append out "/bin"))
                       (cups-filter-dir
                        (string-append out "/lib/cups/filter"))
                       (cups-model-dir
                        (string-append out "/share/cups/model/Brother"))
                       (cups-wrapper
                        "cupswrapper/brother_lpdwrapper_hlt4000dw"))
                  (copy-recursively vendor-src opt-dir)
                  (for-each
                   delete-file
                   (list (string-append opt-dir
                                        "/cupswrapper/cupswrapperhlt4000dw")
                         (string-append opt-dir "/inf/setupPrintcapij")))

                  (mkdir-p bin-dir)
                  (install-file "src/usr/bin/brprintconf_hlt4000dw" bin-dir)
                  (chmod (string-append bin-dir "/brprintconf_hlt4000dw") #o755)

                  (mkdir-p cups-filter-dir)
                  (copy-file (string-append opt-dir "/" cups-wrapper)
                             (string-append cups-filter-dir
                                            "/brother_lpdwrapper_hlt4000dw"))
                  (chmod (string-append cups-filter-dir
                                        "/brother_lpdwrapper_hlt4000dw")
                         #o755)

                  (mkdir-p cups-model-dir)
                  (install-file
                   (string-append opt-dir
                                  "/cupswrapper/brother_hlt4000dw_printer_en.ppd")
                   cups-model-dir))))
            (add-after 'install 'patch-scripts
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((out #$output)
                       (opt-dir
                        (string-append out "/opt/brother/Printers/hlt4000dw"))
                       (cups-wrapper
                        (string-append out
                                       "/lib/cups/filter"
                                       "/brother_lpdwrapper_hlt4000dw"))
                       (vendor-wrapper
                        (string-append opt-dir
                                       "/cupswrapper"
                                       "/brother_lpdwrapper_hlt4000dw"))
                       (lpd-filter
                        (string-append opt-dir "/lpd/filter_hlt4000dw"))
                       (pdf2ps
                        (string-append (assoc-ref inputs "ghostscript")
                                       "/bin/pdf2ps"))
                       (path
                        (string-join
                         (list (string-append out "/bin")
                               (string-append
                                (assoc-ref inputs "coreutils") "/bin")
                               (string-append (assoc-ref inputs "file") "/bin")
                               (string-append
                                (assoc-ref inputs "ghostscript") "/bin")
                               (string-append (assoc-ref inputs "grep") "/bin")
                               (string-append (assoc-ref inputs "perl") "/bin")
                               (string-append (assoc-ref inputs "sed") "/bin")
                               (string-append (assoc-ref inputs "which") "/bin"))
                         ":")))
                  (for-each
                   (lambda (wrapper)
                     (substitute* wrapper
                       (("my \\$basedir = `readlink \\$0`;")
                        "my $basedir = \"/opt/brother/Printers/hlt4000dw/\";")
                       (("if \\( \\$basedir eq '' \\)\\{")
                        "if (0) {")
                       (("\\$LPDCONFIGEXE=\"brprintconf_\";")
                        (string-append "$LPDCONFIGEXE=\"" out
                                       "/bin/brprintconf_\";"))
                       (("\\$CUPSINPUT=\\$ARGV\\[7\\];")
                        "$CUPSINPUT=$ARGV[5];")))
                   (list cups-wrapper vendor-wrapper))

                  (substitute* lpd-filter
                    (("/usr/bin/pdf2ps") pdf2ps))

                  (for-each patch-shebang
                            (list cups-wrapper vendor-wrapper lpd-filter))

                  (for-each
                   (lambda (program)
                     (wrap-program program
                       `("PATH" ":" prefix (,path))))
                   (list cups-wrapper vendor-wrapper lpd-filter)))))
            (add-after 'patch-scripts 'patch-elf-binaries
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((out #$output)
                       (libc32 (assoc-ref inputs "glibc32"))
                       (libstdc++32 (assoc-ref inputs "libstdc++32"))
                       (ld.so (string-append libc32 "/lib/ld-linux.so.2"))
                       (libstdc++-rpath
                        (let* ((port
                                (open-pipe* OPEN_READ
                                            "patchelf"
                                            "--print-rpath"
                                            (string-append libstdc++32
                                                           "/lib/libstdc++.so")))
                               (line (read-line port))
                               (status (close-pipe port)))
                          (unless (zero? status)
                            (error "failed to read libstdc++ RUNPATH"))
                          line))
                       (rpath (string-join
                               (list (string-append libc32 "/lib")
                                     (string-append libstdc++32 "/lib")
                                     libstdc++-rpath)
                               ":")))
                  (for-each
                   (lambda (binary)
                     (invoke "patchelf" "--set-interpreter" ld.so binary)
                     (invoke "patchelf" "--set-rpath" rpath binary))
                   (list
                    (string-append out "/bin/brprintconf_hlt4000dw")
                    (string-append out
                                   "/opt/brother/Printers/hlt4000dw"
                                   "/lpd/brhlt4000dwfilter"))))))
            (add-after 'patch-elf-binaries 'validate-install
              (lambda _
                (let ((out #$output))
                  (for-each
                   (lambda (file)
                     (unless (file-exists? file)
                       (error "missing Brother HL-T4000DW driver file" file)))
                   (list
                    (string-append out "/bin/brprintconf_hlt4000dw")
                    (string-append out
                                   "/lib/cups/filter"
                                   "/brother_lpdwrapper_hlt4000dw")
                    (string-append out
                                   "/opt/brother/Printers/hlt4000dw"
                                   "/lpd/filter_hlt4000dw")
                    (string-append out
                                   "/opt/brother/Printers/hlt4000dw"
                                   "/lpd/brhlt4000dwfilter")
                    (string-append out
                                   "/share/cups/model/Brother"
                                   "/brother_hlt4000dw_printer_en.ppd")))))))))
      (native-inputs
       (list patchelf-0.16))
      (inputs
       `(("bash" ,bash-minimal)
         ("coreutils" ,coreutils)
         ("file" ,file)
         ("ghostscript" ,ghostscript)
         ("glibc32" ,(to32 glibc))
         ("grep" ,grep)
         ("libstdc++32" ,(to32 (make-libstdc++ gcc)))
         ("perl" ,perl)
         ("sed" ,sed)
         ("which" ,which)))
      (supported-systems '("x86_64-linux" "i686-linux"))
      (home-page "https://github.com/b-vitamins/brother-hlt4000dw-driver")
      (synopsis "Brother HL-T4000DW CUPS printer driver")
      (description
       "This package provides the proprietary Brother HL-T4000DW CUPS printer
driver.  It installs the vendor CUPS wrapper, PPD, LPD filter, configuration
files, and 32-bit printer conversion binaries while preserving Brother's
expected @file{/opt/brother/Printers/hlt4000dw} layout.")
      (license
       (list license:gpl2+
             (myguix-license:nonfree
              "file://src/opt/brother/Printers/hlt4000dw/LICENSE_ENG.txt"
              "Brother printer driver license."))))))
