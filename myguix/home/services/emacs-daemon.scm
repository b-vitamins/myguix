(define-module (myguix home services emacs-daemon)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (myguix home services emacs)
  #:export (emacs-daemon-configuration
            emacs-daemon-configuration?
            emacs-daemon-configuration-package
            emacs-daemon-configuration-server-name
            emacs-daemon-configuration-extra-options
            emacs-daemon-configuration-environment-variables
            emacs-daemon-configuration-inherit-display-environment?
            emacs-daemon-configuration-respawn?
            my-home-emacs-daemon-service-type
            home-emacs-daemon-service-type
            my-home-emacs-daemon-service))

(define-configuration/no-serialization emacs-daemon-configuration
  (package
    (file-like %my-home-emacs-package)
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
  (inherit-display-environment?
    (boolean #f)
    "Whether to forward display-specific variables to the daemon.
Disabled by default so PGTK Emacs starts headless and survives display
disconnects.")
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
        (inherit-display-environment?
         (emacs-daemon-configuration-inherit-display-environment? config))
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
                      (xdg-state-home
                       (or (getenv "XDG_STATE_HOME")
                           (string-append home "/.local/state")))
                      (log-dir (string-append xdg-state-home "/log"))
                      (log-file (string-append log-dir "/emacs-"
                                               #$server-name ".log"))
                      (display-environment-variables
                       (if #$inherit-display-environment?
                           (let loop ((names '("DISPLAY"
                                               "WAYLAND_DISPLAY"
                                               "WAYLAND_SOCKET"
                                               "XAUTHORITY"
                                               "GDK_BACKEND"))
                                      (result '()))
                             (if (null? names)
                                 (reverse result)
                                 (let* ((name (car names))
                                        (value (getenv name)))
                                   (loop (cdr names)
                                         (if value
                                             (cons (string-append name "=" value)
                                                   result)
                                             result)))))
                           '()))
                      (override-environment-variables
                       (append display-environment-variables
                               '#$environment-variables))
                      (environment-variable-name
                       (lambda (entry)
                         (let ((separator (string-index entry #\=)))
                           (and separator
                                (string-take entry separator)))))
                      (override-names
                       (let loop ((entries override-environment-variables)
                                  (result '()))
                         (if (null? entries)
                             (reverse result)
                             (let ((name
                                    (environment-variable-name
                                     (car entries))))
                               (loop (cdr entries)
                                     (if name
                                         (cons name result)
                                         result))))))
                      (effective-environment-variables
                       (append
                        (let loop ((entries (environ))
                                   (result '()))
                          (if (null? entries)
                              (reverse result)
                              (let* ((entry (car entries))
                                     (name
                                      (environment-variable-name
                                       entry)))
                                (loop (cdr entries)
                                      (if (and name
                                               (member name override-names))
                                          result
                                          (cons entry result))))))
                        override-environment-variables)))
                 (mkdir-p log-dir)
                 (make-forkexec-constructor
                   (append
                    (list #$(file-append package "/bin/emacs")
                          (string-append "--fg-daemon=" #$server-name))
                    '#$extra-options)
                   #:directory home
                   #:log-file log-file
                   #:environment-variables
                   effective-environment-variables)))
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
                                       (package %my-home-emacs-package)
                                       (server-name "server")
                                       (extra-options '("--no-splash"))
                                       (environment-variables '())
                                       (inherit-display-environment? #f)
                                       (respawn? #t))
  "Convenience constructor for `home-emacs-daemon-service-type'."
  (service my-home-emacs-daemon-service-type
           (emacs-daemon-configuration
            (package package)
            (server-name server-name)
            (extra-options extra-options)
            (environment-variables environment-variables)
            (inherit-display-environment?
             inherit-display-environment?)
            (respawn? respawn?))))
