(define-module (myguix services base)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu system keyboard)
  #:use-module (myguix system install)
  #:export (%my-base-services))

(define %my-base-services
  (list
   ;; Base Services - Compatibility symlinks
   (service special-files-service-type
            `( ;Shell interpreters
               ("/bin/sh" ,(file-append bash-minimal "/bin/sh"))
              ("/bin/bash" ,(file-append bash "/bin/bash"))
              ("/bin/zsh" ,(file-append zsh "/bin/zsh"))
              ("/usr/bin/env" ,(file-append coreutils "/bin/env"))

              ;; Scripting languages
              ("/bin/perl" ,(file-append perl "/bin/perl"))
              ("/usr/bin/perl" ,(file-append perl "/bin/perl"))
              ("/bin/python" ,(file-append python "/bin/python3"))
              ("/bin/python3" ,(file-append python "/bin/python3"))
              ("/usr/bin/python" ,(file-append python "/bin/python3"))
              ("/usr/bin/python3" ,(file-append python "/bin/python3"))))

   ;; Console font service for better readability
   (service console-font-service-type
            (map (lambda (tty)
                   (cons tty
                         (file-append font-terminus
                                      "/share/consolefonts/ter-v16n.psf.gz")))
                 '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

   ;; Log Management
   (service log-rotation-service-type
            (log-rotation-configuration (external-log-files '("/var/log/messages"
                                                              "/var/log/secure"
                                                              "/var/log/debug"
                                                              "/var/log/maillog"))
                                        (calendar-event #~"weekly")
                                        (compression 'gzip)
                                        (expiry (* 4 7 24 3600))))

   (service log-cleanup-service-type
            (log-cleanup-configuration (directory "/var/log/guix/drvs")
                                       (expiry (* 30 24 3600)))) ;30 days
   
   ;; Core Shepherd services
   (service shepherd-system-log-service-type)
   (service shepherd-timer-service-type)
   (service shepherd-transient-service-type)

   ;; Login and terminal services
   (service login-service-type)
   (service virtual-terminal-service-type)

   ;; Standard getty service
   (service agetty-service-type
            (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                  (term "linux")
                                  (tty #f))) ;automatic
   
   ;; Skip kmscon for tty1-3 if using graphical desktop
   ;; Only enable for emergency/recovery consoles
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty7")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps")))))

   ;; Name Service Cache Daemon with custom configuration
   (service nscd-service-type
            (nscd-configuration (caches (list (nscd-cache (database 'hosts)
                                                          (positive-time-to-live
                                                           (* 3600 12))
                                                          (negative-time-to-live
                                                           3600)
                                                          (persistent? #t))
                                              (nscd-cache (database 'services)
                                                          (positive-time-to-live
                                                           (* 3600 24))
                                                          (negative-time-to-live
                                                           3600)
                                                          (persistent? #t))))))

   ;; Guix daemon configuration
   (service guix-service-type
            (guix-configuration (build-accounts 16)
                                (use-substitutes? #t)
                                (tmpdir "/tmp")
                                (extra-options '("--max-jobs=8" "--cores=4"))))

   ;; Enhanced udev configuration
   (service udev-service-type
            (udev-configuration (rules (list lvm2
                                             fuse
                                             alsa-utils
                                             crda
                                             libmtp
                                             pipewire
                                             brightnessctl
                                             android-udev-rules))))

   ;; Core system services
   (service urandom-seed-service-type)
   (service gpm-service-type) ;Console mouse support
   
   ;; Networking
   (service static-networking-service-type
            (list %loopback-static-networking))

   ;; Guix publish for sharing substitutes
   (service guix-publish-service-type
            (guix-publish-configuration (host "localhost")
                                        (port 8080)
                                        (compression '(("zstd" 19)))
                                        (cache "/var/cache/publish")
                                        (workers 4)))

   ;; System resource limits
   (service pam-limits-service-type
            (list
             ;; Real-time audio group
             (pam-limits-entry "@realtime"
                               'both
                               'rtprio 99)
             (pam-limits-entry "@realtime"
                               'both
                               'nice -19)
             (pam-limits-entry "@realtime"
                               'both
                               'memlock
                               'unlimited)

             ;; Audio group for PulseAudio/PipeWire
             (pam-limits-entry "@audio"
                               'both
                               'rtprio 95)
             (pam-limits-entry "@audio"
                               'both
                               'nice -15)
             (pam-limits-entry "@audio"
                               'both
                               'memlock
                               'unlimited)

             ;; General limits
             (pam-limits-entry "*"
                               'both
                               'nofile 524288) ;512K file descriptors
             (pam-limits-entry "*"
                               'hard
                               'nproc 4096) ;Process limit
             (pam-limits-entry "*"
                               'soft
                               'nproc 4096)))))
