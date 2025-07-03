(define-module (myguix home services emacs-daemon)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (emacs-daemon-configuration emacs-daemon-configuration?
            emacs-daemon-configuration-package
            emacs-daemon-configuration-server-name
            emacs-daemon-configuration-extra-options
            home-emacs-daemon-service-type))

(define-configuration/no-serialization emacs-daemon-configuration
                                       (package
                                         (file-like emacs)
                                         "The Emacs package to use.")

                                       (server-name (string "server")
                                        "Name of the Emacs server socket.")

                                       (extra-options (list '())
                                        "List of additional command-line options."))

(define (emacs-daemon-shepherd-service config)
  "Return a shepherd service configuration for running Emacs in daemon mode."
  (let ((package
          (emacs-daemon-configuration-package config))
        (server-name (emacs-daemon-configuration-server-name config))
        (extra-options (emacs-daemon-configuration-extra-options config)))
    (list (shepherd-service (provision (list (string->symbol (string-append
                                                              "emacs-"
                                                              server-name))))
                            (documentation (string-append
                                            "Run Emacs in daemon mode with server name '"
                                            server-name "'."))
                            (requirement '())
                            (respawn? #t)
                            (start #~(make-forkexec-constructor (append (list #$
                                                                              (file-append
                                                                               package
                                                                               "/bin/emacs")

                                                                              
                                                                              (string-append
                                                                               "--daemon="
                                                                               #$server-name))
                                                                        #$@extra-options)
                                                                #:log-file (string-append
                                                                            (or
                                                                             (getenv
                                                                              "XDG_STATE_HOME")
                                                                             (string-append
                                                                              (getenv
                                                                               "HOME")
                                                                              "/.local/state"))
                                                                            "/log/emacs-"
                                                                            #$server-name
                                                                            ".log")
                                                                #:environment-variables
                                                                (append (list (string-append
                                                                               "HOME="

                                                                               
                                                                               (getenv
                                                                                "HOME"))

                                                                              
                                                                              (string-append
                                                                               "USER="

                                                                               
                                                                               (getenv
                                                                                "USER"))

                                                                              
                                                                              (string-append
                                                                               "PATH="

                                                                               
                                                                               (getenv
                                                                                "PATH")))
                                                                        ;; Include display variables if they exist
                                                                        (if (getenv
                                                                             "DISPLAY")
                                                                            (list
                                                                             (string-append
                                                                              "DISPLAY="

                                                                              
                                                                              (getenv
                                                                               "DISPLAY")))
                                                                            '())
                                                                        (if (getenv
                                                                             "WAYLAND_DISPLAY")
                                                                            (list
                                                                             (string-append
                                                                              "WAYLAND_DISPLAY="

                                                                              
                                                                              (getenv
                                                                               "WAYLAND_DISPLAY")))
                                                                            '())
                                                                        (if (getenv
                                                                             "XDG_RUNTIME_DIR")
                                                                            (list
                                                                             (string-append
                                                                              "XDG_RUNTIME_DIR="

                                                                              
                                                                              (getenv
                                                                               "XDG_RUNTIME_DIR")))
                                                                            '()))))
                            (stop #~(make-kill-destructor))))))

(define home-emacs-daemon-service-type
  (service-type (name 'home-emacs-daemon)
                (description "Run Emacs as a daemon service.")
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   emacs-daemon-shepherd-service)))
                (default-value (emacs-daemon-configuration))))
