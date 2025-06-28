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
                            ;; Automatically restart on failure
                            (respawn? #t)
                            (respawn-delay 1) ;Wait 1 second before restart
                            (start #~(lambda _
                                       ;; Clean up any stale socket before starting
                                       (let ((socket-file (string-append (or (getenv
                                                                              "XDG_RUNTIME_DIR")
                                                                             (string-append
                                                                              "/run/user/"
                                                                              
                                                                              (number->string
                                                                               (getuid))))
                                                           "/emacs/"
                                                           #$server-name)))
                                         (when (file-exists? socket-file)
                                           ;; Check if daemon is actually dead
                                           (unless (zero? (system* #$(file-append
                                                                      package
                                                                      "/bin/emacsclient")
                                                           "-s"
                                                           #$server-name
                                                           "-e"
                                                           "nil"
                                                           ">/dev/null"
                                                           "2>&1"))
                                             ;; Socket exists but daemon is dead, remove it
                                             (delete-file socket-file))))
                                       ;; Start the daemon
                                       ((make-forkexec-constructor (append (list #$
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
                                                                   #:environment-variables (let 
                                                                                                (
                                                                                                 (home
                                                                                                  (getenv
                                                                                                   "HOME"))
                                                                                                 
                                                                                                 (user
                                                                                                  (getenv
                                                                                                   "USER"))
                                                                                                 
                                                                                                 (path
                                                                                                  (getenv
                                                                                                   "PATH"))
                                                                                                 
                                                                                                 (display
                                                                                                  (getenv
                                                                                                   "DISPLAY"))
                                                                                                 
                                                                                                 (wayland
                                                                                                  (getenv
                                                                                                   "WAYLAND_DISPLAY"))
                                                                                                 
                                                                                                 (xauth
                                                                                                  (getenv
                                                                                                   "XAUTHORITY"))
                                                                                                 
                                                                                                 (runtime-dir
                                                                                                  (or
                                                                                                   (getenv
                                                                                                    "XDG_RUNTIME_DIR")
                                                                                                   
                                                                                                   (string-append
                                                                                                    "/run/user/"
                                                                                                    
                                                                                                    (number->string
                                                                                                     (getuid))))))
                                                                                             
                                                                                             (filter (lambda 
                                                                                                             (x)
                                                                                                       x)
                                                                                              
                                                                                              (list
                                                                                               (and
                                                                                                home
                                                                                                
                                                                                                (string-append
                                                                                                 "HOME="
                                                                                                 home))
                                                                                               
                                                                                               (and
                                                                                                user
                                                                                                
                                                                                                (string-append
                                                                                                 "USER="
                                                                                                 user))
                                                                                               
                                                                                               (and
                                                                                                path
                                                                                                
                                                                                                (string-append
                                                                                                 "PATH="
                                                                                                 path))
                                                                                               
                                                                                               (string-append
                                                                                                "XDG_RUNTIME_DIR="
                                                                                                runtime-dir)
                                                                                               ;; Display variables
                                                                                               
                                                                                               (and
                                                                                                display
                                                                                                
                                                                                                (string-append
                                                                                                 "DISPLAY="
                                                                                                 display))
                                                                                               
                                                                                               (and
                                                                                                wayland
                                                                                                
                                                                                                (string-append
                                                                                                 "WAYLAND_DISPLAY="
                                                                                                 wayland))
                                                                                               
                                                                                               (and
                                                                                                xauth
                                                                                                
                                                                                                (string-append
                                                                                                 "XAUTHORITY="
                                                                                                 xauth))
                                                                                               ;; Default display if none set
                                                                                               
                                                                                               (and
                                                                                                (not
                                                                                                 display)
                                                                                                
                                                                                                (not
                                                                                                 wayland)
                                                                                                "DISPLAY=:0"))))))))
                            (stop #~(lambda (pid)
                                      ;; Try graceful shutdown first
                                      (let ((result (system* #$(file-append
                                                                package
                                                                "/bin/emacsclient")
                                                             "-s"
                                                             #$server-name
                                                             "-e"
                                                             "(kill-emacs)")))
                                        (if (zero? result)
                                            ;; Wait a moment for clean shutdown
                                            (begin
                                              (sleep 0.5) #t)
                                            ;; If graceful shutdown failed, kill the process
                                            (begin
                                              (when pid
                                                (kill pid SIGTERM)
                                                (sleep 0.5)
                                                ;; Force kill if still running
                                                (when (file-exists? (format #f
                                                                            "/proc/~a"
                                                                            pid))
                                                  (kill pid SIGKILL))) #t)))))
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
                                                                           (when running
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
                                                                           #t)))
                                           (shepherd-action (name 'reload)
                                                            (documentation
                                                             "Reload Emacs configuration.")
                                                            (procedure #~(lambda (running . args)
                                                                           (if
                                                                            running
                                                                            (zero?
                                                                             (system* #$
                                                                              (file-append
                                                                               package
                                                                               "/bin/emacsclient")
                                                                              "-s"
                                                                              #$server-name
                                                                              "-e"
                                                                              "(load-file user-init-file)"))
                                                                            (begin
                                                                              
                                                                              (format
                                                                               #t
                                                                               "Emacs daemon not running~%")
                                                                              #f)))))))))))

(define home-emacs-daemon-service-type
  (service-type (name 'home-emacs-daemon)
                (description
                 "Run Emacs as a daemon service for instant frame spawning.")
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   emacs-daemon-shepherd-service)))
                (default-value (emacs-daemon-configuration))))
