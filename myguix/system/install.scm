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
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (myguix packages linux)
  #:export (my-installation-os))

(define my-installation-os
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (kernel-loadable-modules (list broadcom-sta))
    (kernel-arguments '("modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"))
    (firmware (list linux-firmware broadcom-bt-firmware iwlwifi-firmware))
    (packages (append (list git curl coreutils emacs-no-x-toolkit)
                      (operating-system-packages installation-os)))
    (services
     (cons* (simple-service 'channel-file etc-service-type
                            (list `("channels.scm" ,(local-file
                                                     "examples/channels.tmpl"))))
            (modify-services (operating-system-user-services installation-os)
              (guix-service-type config =>
                                 (guix-configuration (inherit config)
                                                     (discover? #t)
                                                     (authorized-keys (append (list
                                                                               (local-file
                                                                                "keys/ci.guix.gnu.org.pub")

                                                                               
                                                                               (local-file
                                                                                "keys/bordeaux.guix.gnu.org.pub"))
                                                                       %default-authorized-guix-keys))
                                                     (substitute-urls (append (list
                                                                               "https://ci.guix.gnu.org.pub"
                                                                               "https://bordeaux.guix.gnu.org.pub"))))))))))

my-installation-os
