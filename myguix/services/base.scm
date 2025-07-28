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

(define %my-keyboard-layout
  (keyboard-layout "us" "altgr-intl"
                   #:options '("ctrl:nocaps")))

(define %my-base-services
  (list
   ;; Core services matching upstream order
   (service login-service-type)

   (service virtual-terminal-service-type)
   (service console-font-service-type
            (map (lambda (tty)
                   (cons tty %default-console-font))
                 '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

   (service shepherd-system-log-service-type)
   (service agetty-service-type
            (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                  (term "vt100")
                                  (tty #f) ;automatic
                                  (shepherd-requirement '(syslogd))))

   ;; Using kmscon instead of mingetty for enhanced console support
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty1")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty2")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty3")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty4")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty5")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty6")
                                  (hardware-acceleration? #t)
                                  (font-size 14)
                                  (keyboard-layout %my-keyboard-layout)))

   ;; Extra Bash configuration including Bash completion and aliases.
   (service etc-bashrc-d-service-type)

   (service static-networking-service-type
            (list %loopback-static-networking))
   (service urandom-seed-service-type)
   (service guix-service-type)
   (service nscd-service-type)
   (service log-rotation-service-type)

   ;; Convenient services brought by the Shepherd.
   (service shepherd-timer-service-type)
   (service shepherd-transient-service-type)

   ;; Periodically delete old build logs.
   (service log-cleanup-service-type
            (log-cleanup-configuration (directory "/var/log/guix/drvs")))

   ;; The LVM2 rules are needed as soon as LVM2 or the device-mapper is
   ;; used, so enable them by default.  The FUSE and ALSA rules are
   ;; less critical, but handy.
   (service udev-service-type
            (udev-configuration (rules (list lvm2
                                             fuse
                                             alsa-utils
                                             crda
                                             ;; Extra rules for enhanced hardware support
                                             libmtp
                                             pipewire
                                             brightnessctl
                                             android-udev-rules))))

   ;; sysctl is deliberately not included
   
   (service special-files-service-type
            `(("/bin/sh" ,(file-append bash "/bin/sh"))
              ("/usr/bin/env" ,(file-append coreutils "/bin/env"))
              ;; Extra compatibility symlinks
              ("/bin/bash" ,(file-append bash "/bin/bash"))
              ("/bin/zsh" ,(file-append zsh "/bin/zsh"))
              ("/bin/perl" ,(file-append perl "/bin/perl"))
              ("/usr/bin/perl" ,(file-append perl "/bin/perl"))
              ("/bin/python" ,(file-append python "/bin/python3"))
              ("/bin/python3" ,(file-append python "/bin/python3"))
              ("/usr/bin/python" ,(file-append python "/bin/python3"))
              ("/usr/bin/python3" ,(file-append python "/bin/python3"))))

   ;; Extra services not in upstream %base-services
   (service gpm-service-type) ;Console mouse support
   
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
