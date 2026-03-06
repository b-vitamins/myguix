(define-module (myguix home services emacs-daemon)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (emacs-daemon-configuration
            emacs-daemon-configuration?
            emacs-daemon-configuration-package
            emacs-daemon-configuration-server-name
            emacs-daemon-configuration-extra-options
            emacs-daemon-configuration-environment-variables
            emacs-daemon-configuration-respawn?
            my-home-emacs-daemon-service-type
            home-emacs-daemon-service-type
            my-home-emacs-daemon-service))

(define-configuration/no-serialization emacs-daemon-configuration
  (package
    (file-like emacs-pgtk)
    "The Emacs package to run as daemon.")
  (server-name
    (string "server")
    "Name of the Emacs server socket.")
  (extra-options
    (list-of-strings '("--no-splash"))
    "Additional command-line options passed to Emacs.")
  (environment-variables
    (list-of-strings '())
    "Extra environment variables appended to the daemon environment.")
  (respawn?
    (boolean #t)
    "Whether Shepherd should respawn Emacs when it exits unexpectedly."))

(define (emacs-daemon-shepherd-service config)
  "Return a Shepherd service running Emacs in foreground-daemon mode."
  (let ((package (emacs-daemon-configuration-package config))
        (server-name (emacs-daemon-configuration-server-name config))
        (extra-options (emacs-daemon-configuration-extra-options config))
        (environment-variables
         (emacs-daemon-configuration-environment-variables config))
        (respawn? (emacs-daemon-configuration-respawn? config)))
    (list
     (shepherd-service
      (provision (list (string->symbol (string-append "emacs-" server-name))))
      (documentation
       (string-append "Run Emacs in daemon mode (server " server-name ")."))
      (requirement '())
      (modules '((guix build utils)))
      (respawn? respawn?)
      (start #~(let* ((home (or (getenv "HOME") "/tmp"))
                      (user (or (getenv "USER") "unknown"))
                      (path (or (getenv "PATH")
                                "/run/current-system/profile/bin"))
                      (xdg-config-home
                       (or (getenv "XDG_CONFIG_HOME")
                           (string-append home "/.config")))
                      (xdg-data-home
                       (or (getenv "XDG_DATA_HOME")
                           (string-append home "/.local/share")))
                      (xdg-cache-home
                       (or (getenv "XDG_CACHE_HOME")
                           (string-append home "/.cache")))
                      (xdg-state-home
                       (or (getenv "XDG_STATE_HOME")
                           (string-append home "/.local/state")))
                      (log-dir (string-append xdg-state-home "/log"))
                      (log-file (string-append log-dir "/emacs-"
                                               #$server-name ".log")))
                 (mkdir-p log-dir)
                 (make-forkexec-constructor
                   (append
                    (list #$(file-append package "/bin/emacs")
                          (string-append "--fg-daemon=" #$server-name))
                    '#$extra-options)
                   #:directory home
                   #:log-file log-file
                   #:environment-variables
                   (append
                    (list (string-append "HOME=" home)
                          (string-append "USER=" user)
                          (string-append "LOGNAME=" user)
                          (string-append "PATH=" path)
                          (string-append "XDG_CONFIG_HOME=" xdg-config-home)
                          (string-append "XDG_DATA_HOME=" xdg-data-home)
                          (string-append "XDG_CACHE_HOME=" xdg-cache-home)
                          (string-append "XDG_STATE_HOME=" xdg-state-home)
                          "SSL_CERT_DIR=/etc/ssl/certs"
                          "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                    (if (getenv "DISPLAY")
                        (list (string-append "DISPLAY=" (getenv "DISPLAY")))
                        '())
                    (if (getenv "WAYLAND_DISPLAY")
                        (list (string-append "WAYLAND_DISPLAY="
                                             (getenv "WAYLAND_DISPLAY")))
                        '())
                    (if (getenv "XDG_RUNTIME_DIR")
                        (list (string-append "XDG_RUNTIME_DIR="
                                             (getenv "XDG_RUNTIME_DIR")))
                        '())
                    '#$environment-variables))))
      (stop #~(make-kill-destructor))))))

(define my-home-emacs-daemon-service-type
  (service-type
   (name 'home-emacs-daemon)
   (description "Run Emacs as a persistent Home daemon.")
   (extensions
    (list (service-extension home-shepherd-service-type
                             emacs-daemon-shepherd-service)))
   (default-value (emacs-daemon-configuration))))

;; Backward-compatible alias.
(define home-emacs-daemon-service-type my-home-emacs-daemon-service-type)

(define* (my-home-emacs-daemon-service #:key
                                       (package emacs-pgtk)
                                       (server-name "server")
                                       (extra-options '("--no-splash"))
                                       (environment-variables '())
                                       (respawn? #t))
  "Convenience constructor for `home-emacs-daemon-service-type'."
  (service my-home-emacs-daemon-service-type
           (emacs-daemon-configuration
            (package package)
            (server-name server-name)
            (extra-options extra-options)
            (environment-variables environment-variables)
            (respawn? respawn?))))
