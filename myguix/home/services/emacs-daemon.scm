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
                                         "The Emacs package to use. Defaults to @code{emacs}.")

                                       (server-name (string "server")
                                        "Name of the Emacs server socket. Multiple Emacs daemons can run
with different server names.")

                                       (extra-options (list '())
                                        "List of additional command-line options to pass to Emacs daemon.
For example: @code{(list \"--debug-init\" \"--no-site-file\")}."))

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
                                                                              "XDG_LOG_HOME")
                                                                             (string-append
                                                                              (getenv
                                                                               "HOME")
                                                                              "/.local/var/log"))
                                                                            "/emacs-"
                                                                            #$server-name
                                                                            ".log")
                                                                #:environment-variables
                                                                (list (string-append
                                                                       "PATH="
                                                                       (getenv
                                                                        "PATH"))
                                                                      (string-append
                                                                       "HOME="
                                                                       (getenv
                                                                        "HOME"))
                                                                      (string-append
                                                                       "USER="
                                                                       (getenv
                                                                        "USER"))
                                                                      (string-append
                                                                       "DISPLAY="
                                                                       (or (getenv
                                                                            "DISPLAY")
                                                                        ":0"))
                                                                      (string-append
                                                                       "XAUTHORITY="
                                                                       (or (getenv
                                                                            "XAUTHORITY")
                                                                           (string-append
                                                                            (getenv
                                                                             "HOME")
                                                                            "/.Xauthority")))
                                                                      (string-append
                                                                       "XDG_RUNTIME_DIR="
                                                                       (or (getenv
                                                                            "XDG_RUNTIME_DIR")
                                                                           (string-append
                                                                            "/run/user/"
                                                                            (number->string
                                                                             (getuid))))))))
                            (stop #~(lambda _
                                      (zero? (system* #$(file-append package
                                                         "/bin/emacsclient")
                                                      "-s"
                                                      #$server-name "-e"
                                                      "(kill-emacs)"))))
                            (actions (list (shepherd-action (name 'status)
                                                            (documentation
                                                             "Check if Emacs daemon is running and display info.")
                                                            (procedure #~(lambda (running . args)
                                                                           (if
                                                                            running
                                                                            (begin
                                                                              
                                                                              (format
                                                                               #t
                                                                               "Emacs daemon '~a' is running~%"
                                                                               #$server-name)
                                                                              
                                                                              (format
                                                                               #t
                                                                               "Connect with: emacsclient -s ~a -c~%"
                                                                               #$server-name))
                                                                            (format
                                                                             #t
                                                                             "Emacs daemon '~a' is not running~%"
                                                                             #$server-name))
                                                                           running)))
                                           (shepherd-action (name 'restart-gracefully)
                                                            (documentation
                                                             "Gracefully restart Emacs daemon.")
                                                            (procedure #~(lambda (running . args)
                                                                           (if
                                                                            running
                                                                            (begin
                                                                              
                                                                              (system* #$
                                                                               (file-append
                                                                                package
                                                                                "/bin/emacsclient")
                                                                               "-s"
                                                                               #$server-name
                                                                               "-e"
                                                                               "(save-buffers-kill-emacs)")
                                                                              
                                                                              (sleep
                                                                               2))
                                                                            (format
                                                                             #t
                                                                             "Emacs daemon not running, starting fresh...~%"))
                                                                           #t)))))))))

(define home-emacs-daemon-service-type
  (service-type (name 'home-emacs-daemon)
                (description
                 "Run Emacs as a daemon service for instant frame spawning.")
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   emacs-daemon-shepherd-service)))
                (default-value (emacs-daemon-configuration))))
