(define-module (myguix packages nvidia-containers)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((myguix packages rust-crates-pqrs)
                #:select (lookup-myguix-cargo-inputs)))

(define-public pathrs
  (package
    (name "pathrs")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cyphar/libpathrs/archive/refs/tags/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j8qj1dgl460ir9g73ncf4ka72ckmix744ph84bhnv3wxmwz4yq1"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f                  ;Upstream tests require privileged access.
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-workspace-members
            (lambda _
              (invoke "sed" "-i" "/^\\[workspace\\]/,$d" "Cargo.toml")))
          (add-after 'drop-workspace-members 'disable-broken-symbol-version-script
            (lambda _
              (substitute* "build.rs"
                ((".*cargo:rustc-cdylib-link-arg=-Wl,--version-script=.*")
                 ""))))
          (replace 'build
            (lambda _
              (invoke "cargo" "rustc" "--offline" "--release"
                      "--crate-type=cdylib"
                      "--features=capi"
                      "--" "-C" "panic=abort")
              (invoke "cargo" "rustc" "--offline" "--release"
                      "--crate-type=staticlib"
                      "--features=capi"
                      "--" "-C" "panic=abort")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "bash" "./install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--exec-prefix=" out)
                        (string-append "--includedir=" out "/include")
                        (string-append "--libdir=" out "/lib")
                        (string-append "--pkgconfigdir=" out "/lib/pkgconfig"))))))))
    (native-inputs (list util-linux))
    (inputs (lookup-myguix-cargo-inputs 'pathrs))
    (home-page "https://github.com/cyphar/libpathrs")
    (synopsis "Safe path resolution library for Linux")
    (description
     "pathrs provides a Rust library and C-compatible API for performing
safer path resolution operations on Linux.")
    (license (list license:mpl2.0 license:lgpl3+))))
