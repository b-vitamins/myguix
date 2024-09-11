;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Ayan Das <ayan@iisc.ac.in>
;;
;; Generate a bookable image (e.g. for USB sticks, etc.) with:
;;
;; $ guix system image --image-type=iso9660 myguix/system/install.scm

(define-module (myguix system install)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (myguix packages linux)
  #:export (%my-channels %my-authorized-keys %my-substitute-urls
                         my-installation-os))

(define %my-channels
  (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (branch "master")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
          (name 'myguix)
          (url "https://github.com/b-vitamins/myguix.git")
          (branch "master")
          (introduction
           (make-channel-introduction
            "85d58b09dc71e9dc9834b666b658f79d2e212d65"
            (openpgp-fingerprint
             "883B CA6B D275 A5F2 673C  C5DD 2AD3 2FC0 2A50 01F7"))))))

(define %my-authorized-keys
  (append (list (local-file "keys/substitutes.myguix.bvits.in.pub"))
          %default-authorized-guix-keys))

(define %my-substitute-urls
  (append (list "https://substitutes.myguix.bvits.in")
          %default-substitute-urls))

(define my-installation-os
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (kernel-arguments (list "modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"))
    (kernel-loadable-modules (list broadcom-sta))
    (firmware (list linux-firmware))
    (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                      #:options '("ctrl:nocaps"
                                                  "altwin:swap_alt_win")))
    (packages (append (list git curl coreutils emacs-no-x-toolkit)
                      (operating-system-packages installation-os)))
    (services
     (modify-services (operating-system-user-services installation-os)
       (guix-service-type config =>
                          (guix-configuration (inherit config)
                                              (guix (guix-for-channels
                                                     %my-channels))
                                              (channels %my-channels)
                                              (authorized-keys
                                               %my-authorized-keys)
                                              (substitute-urls
                                               %my-substitute-urls)))))))

my-installation-os
