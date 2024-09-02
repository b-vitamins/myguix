(define-module (myguix services base)
  #:use-module (gnu)
  #:use-module (gnu services base)
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
                   (cons tty
                         (file-append (specification->package "font-terminus")
                                      "/share/consolefonts/ter-132n")))
                 '("tty1" "tty2" "tty3")))
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
                                                    #:options '("ctrl:nocaps"
                                                                "altwin:swap_alt_win")))))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty2")
                                  (hardware-acceleration? #t)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps"
                                                                "altwin:swap_alt_win")))))
   (service kmscon-service-type
            (kmscon-configuration (virtual-terminal "tty3")
                                  (hardware-acceleration? #t)
                                  (keyboard-layout (keyboard-layout "us"
                                                    "altgr-intl"
                                                    #:options '("ctrl:nocaps"
                                                                "altwin:swap_alt_win")))))
   (service nscd-service-type)
   (service syslog-service-type)
   (service guix-service-type
            (guix-configuration (guix (guix-for-channels %my-channels))
                                (channels %my-channels)
                                (build-accounts 16)
                                (authorized-keys %my-authorized-keys)
                                (substitute-urls %my-substitute-urls)
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
