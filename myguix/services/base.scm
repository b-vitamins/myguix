(define-module (myguix services base)
  #:use-module (gnu)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages package-management)
  #:use-module (myguix system install)
  #:export (%my-base-services))

(define %my-base-services
  (list
   ;; Base Services
   (service special-files-service-type
            `(("/bin/sh" ,(file-append (specification->package "bash")
                                       "/bin/sh"))
              ("/bin/bash" ,(file-append (specification->package "bash")
                                         "/bin/bash"))
              ("/bin/zsh" ,(file-append (specification->package "zsh")
                                        "/bin/zsh"))
              ("/bin/perl" ,(file-append (specification->package "perl")
                                         "/bin/perl"))
              ("/bin/python" ,(file-append (specification->package "python")
                                           "/bin/python3"))
              ("/bin/python3" ,(file-append (specification->package "python")
                                            "/bin/python3"))
              ("/usr/bin/env" ,(file-append (specification->package
                                             "coreutils") "/bin/env"))))
   (service console-font-service-type
            (map (lambda (tty)
                   (cons tty %default-console-font))
                 '("tty1" "tty2" "tty3")))

   ;; Log Rotation
   (service log-rotation-service-type)
   (service log-cleanup-service-type
            (log-cleanup-configuration (directory "/var/log/guix/drvs")))

   ;; Convenient services brought by the Shepherd.
   (service shepherd-system-log-service-type)
   (service shepherd-timer-service-type)
   (service shepherd-transient-service-type)

   (service login-service-type)
   (service virtual-terminal-service-type)
   (service agetty-service-type
            (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                  (term "vt100")
                                  (tty #f) ;automatic
                                  (shepherd-requirement '(syslogd))))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty1")
                                  (hardware-acceleration? #t)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps")))))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty2")
                                  (hardware-acceleration? #t)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps")))))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty3")
                                  (hardware-acceleration? #t)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps")))))
   (service nscd-service-type)
   (service guix-service-type
            (guix-configuration (build-accounts 12)
                                (tmpdir "/tmp")))
   (service udev-service-type
            (udev-configuration (rules (list (specification->package "lvm2")
                                             (specification->package "fuse")
                                             (specification->package
                                              "alsa-utils")
                                             (specification->package "crda")
                                             (specification->package "libmtp")
                                             (specification->package
                                              "pipewire")
                                             (specification->package
                                              "brightnessctl")))))
   (udev-rules-service 'android
                       (specification->package "android-udev-rules")
                       #:groups '("adbusers"))
   (service urandom-seed-service-type)
   (service gpm-service-type)
   (service static-networking-service-type
            (list %loopback-static-networking))
   (service guix-publish-service-type
            (guix-publish-configuration (compression '(("zstd" 19)))
                                        (cache "/var/cache/publish")))
   (service pam-limits-service-type
            (list (pam-limits-entry "@realtime"
                                    'both
                                    'rtprio 99)
                  (pam-limits-entry "@realtime"
                                    'both
                                    'nice -19)
                  (pam-limits-entry "@realtime"
                                    'both
                                    'memlock
                                    'unlimited)
                  (pam-limits-entry "*"
                                    'both
                                    'nofile 500000)))))
