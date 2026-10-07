;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020, 2026 Alexey Abramov <levenson@mmer.org>

(define-module (myguix packages vpn)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((myguix licenses)
                #:prefix myguix-license:))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.102.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pkgs.tailscale.com/stable/tailscale_"
             version "_"
             (match (or (%current-target-system)
                        (%current-system))
               ("x86_64-linux" "amd64")
               ("aarch64-linux" "arm64")
               (system (error "unsupported system for tailscale" system)))
             ".tgz"))
       (file-name (string-append name "-" version ".tgz"))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "04ld2hf0wn9n8nq59bwpp1f7k5layf91x8n241ywiqfpkbqxgrk5")
                  ("aarch64-linux"
                   "12cbl4hrzy9ax2zyhmq33kx2irbq9qdizp3aqqc7629xwc4h3mk0")
                  (_ "0000000000000000000000000000000000000000000000000000"))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./tailscale" "version")
                (invoke "./tailscaled" "--version"))))
          (replace 'install
            (lambda _
              (let* ((out #$output)
                     (bin (string-append out "/bin"))
                     (sbin (string-append out "/sbin"))
                     (systemd (string-append out "/share/tailscale/systemd")))
                (install-file "tailscale" bin)
                (install-file "tailscaled" sbin)
                (copy-recursively "systemd" systemd)))))))
    (home-page "https://tailscale.com")
    (synopsis "Mesh VPN client")
    (description
     "Tailscale is a WireGuard-based mesh VPN client.  This package installs
the @command{tailscale} CLI and the @command{tailscaled} daemon from
Tailscale's upstream static Linux tarballs.")
    (license license:bsd-3)))

(define-public zerotier
  (package
    (name "zerotier")
    (version "1.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zerotier/ZeroTierOne")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gsc1dbbwa2z5qydavdn6xx7wdshbjzlxaz30iqp26iyrgs9icx1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "ZT_SSO_SUPPORTED=0") ;We don't need SSO/OIDC
       #:phases (modify-phases %standard-phases
                  ;; There is no ./configure
                  (delete 'configure)
                  (replace 'check
                    (lambda* (#:key make-flags #:allow-other-keys)
                      (apply invoke "make" "selftest" make-flags)
                      (invoke "./zerotier-selftest")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (sbin (string-append out "/sbin"))
                             (lib (string-append out "/lib"))
                             (man (string-append out "/share/man"))
                             (zerotier-one-lib (string-append lib
                                                              "/zerotier-one")))
                        (mkdir-p sbin)
                        (install-file "zerotier-one" sbin)
                        (with-directory-excursion sbin
                          (symlink (string-append sbin "/zerotier-one")
                                   "zerotier-cli")
                          (symlink (string-append sbin "/zerotier-one")
                                   "zerotier-idtool"))

                        (mkdir-p zerotier-one-lib)
                        (with-directory-excursion zerotier-one-lib
                          (symlink (string-append sbin "/zerotier-one")
                                   "zerotier-one")
                          (symlink (string-append sbin "/zerotier-one")
                                   "zerotier-cli")
                          (symlink (string-append sbin "/zerotier-one")
                                   "zerotier-idtool"))

                        (mkdir-p (string-append man "/man8"))
                        (install-file "doc/zerotier-one.8"
                                      (string-append man "/man8"))

                        (mkdir-p (string-append man "/man1"))
                        (for-each (lambda (man-page)
                                    (install-file man-page
                                                  (string-append man "/man1")))
                                  (list "doc/zerotier-cli.1"
                                        "doc/zerotier-idtool.1"))
                        #t))))))
    (home-page "https://github.com/zerotier/ZeroTierOne")
    (synopsis "Smart programmable Ethernet switch for planet Earth")
    (description
     "It allows all networked devices, virtual machines,
containers, and applications to communicate as if they all reside in the same
physical data center or cloud region.

This is accomplished by combining a cryptographically addressed and secure
peer to peer network (termed VL1) with an Ethernet emulation layer somewhat
similar to VXLAN (termed VL2).  Our VL2 Ethernet virtualization layer includes
advanced enterprise SDN features like fine grained access control rules for
network micro-segmentation and security monitoring.")
    (license (myguix-license:nonfree "https://mariadb.com/bsl11/"))))
