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
  #:use-module (gnu packages base)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (myguix packages linux)
  #:use-module (myguix system linux-initrd)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu system keyboard)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (myguix packages base)
  #:use-module (myguix packages linux)
  #:use-module (myguix system linux-initrd)
  #:use-module (ice-9 textual-ports)
  #:export (%my-channels 
            my-installation-os))

(define %my-channels
  (cons* (channel
           (name 'myguix)
           (url "https://github.com/b-vitamins/myguix.git")
           (introduction
            (make-channel-introduction
             "85d58b09dc71e9dc9834b666b658f79d2e212d65"
             (openpgp-fingerprint
              "883B CA6B D275 A5F2 673C  C5DD 2AD3 2FC0 2A50 01F7"))))
         %default-channels))


(define my-installation-os
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (kernel-arguments %default-kernel-arguments)
    (firmware (list linux-firmware sof-firmware))
    (initrd microcode-initrd)
    (keyboard-layout (keyboard-layout "us"
                                      "altgr-intl"
                                      #:model "thinkpad"
                                      #:options '("ctrl:nocaps"
                                                  "altwin:swap_alt_win")))
    (packages (append %core-minimal
                      %core-extended
                      %shell-modern
                      %terminal-essentials
                      %text-editors
                      %security-essentials
                      %version-control
                      %compression-tools
                      %network-core
                      %network-diagnostics
                      %network-performance
                      ;; File Management
                      %filesystem-core
                      %filesystem-advanced
                      %cloud-sync
                      %backup-tools
                      %file-sharing
                      %download-tools
                      ;; Fonts
                      %fonts-essential
                      %fonts-programming
                      %fonts-document
                      %fonts-international
                      ;; Installation utilities
                      (list parted
                            gptfdisk
                            ddrescue
                            lvm2-static
                            cryptsetup
                            mdadm
                            dosfstools
                            btrfs-progs
                            e2fsprogs
                            f2fs-tools
                            jfsutils
                            xfsprogs)
                      (operating-system-packages installation-os)))
    (services
     (cons* (simple-service 'myguix-config-templates
                           special-files-service-type
                           `(("/etc/configuration/workstation.scm" 
                              ,(plain-file "workstation.scm"
                                          (call-with-input-file
                                            (string-append (dirname (current-filename))
                                                           "/examples/workstation.tmpl")
                                            get-string-all)))
                             ("/etc/configuration/laptop.scm"
                              ,(plain-file "laptop.scm"
                                          (call-with-input-file
                                            (string-append (dirname (current-filename))
                                                           "/examples/laptop.tmpl")
                                            get-string-all)))))
            (modify-services (operating-system-user-services installation-os)
              (guix-service-type config =>
                                 (guix-configuration (inherit config)
                                                     (channels %my-channels))))))))

my-installation-os
