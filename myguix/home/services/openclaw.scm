(define-module (myguix home services openclaw)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (myguix packages node-pqrs)
  #:export (openclaw-gateway-configuration
            openclaw-gateway-configuration?
            openclaw-gateway-configuration-package
            openclaw-gateway-configuration-provision
            openclaw-gateway-configuration-requirement
            openclaw-gateway-configuration-auto-start?
            openclaw-gateway-configuration-state-directory
            openclaw-gateway-configuration-config-file
            openclaw-gateway-configuration-workspace-directory
            openclaw-gateway-configuration-log-file
            openclaw-gateway-configuration-port
            openclaw-gateway-configuration-bind
            openclaw-gateway-configuration-auth-mode
            openclaw-gateway-configuration-password-file
            openclaw-gateway-configuration-tailscale-mode
            openclaw-gateway-configuration-tailscale-reset-on-exit?
            openclaw-gateway-configuration-ws-log
            openclaw-gateway-configuration-force?
            openclaw-gateway-configuration-verbose?
            openclaw-gateway-configuration-auto-onboard?
            openclaw-gateway-configuration-onboard-extra-options
            openclaw-gateway-configuration-extra-options
            openclaw-gateway-configuration-environment-variables
            openclaw-gateway-configuration-respawn?
            openclaw-gateway-configuration-stop-grace-period
            home-openclaw-gateway-service-type
            home-openclaw-service-type
            home-openclaw-gateway-service
            home-openclaw-service))

(define (openclaw-port? value)
  (and (integer? value)
       (<= 1 value 65535)))

(define (openclaw-non-negative-integer? value)
  (and (integer? value)
       (not (negative? value))))

(define (openclaw-bind-mode? value)
  (and (string? value)
       (if (member value '("loopback" "lan" "tailnet" "auto" "custom"))
           #t
           #f)))

(define (openclaw-auth-mode? value)
  (and (string? value)
       (if (member value '("none" "token" "password" "trusted-proxy"))
           #t
           #f)))

(define (openclaw-tailscale-mode? value)
  (and (string? value)
       (if (member value '("off" "serve" "funnel"))
           #t
           #f)))

(define (openclaw-ws-log-style? value)
  (and (string? value)
       (if (member value '("auto" "full" "compact"))
           #t
           #f)))

(define (optional-string? value)
  (or (not value)
      (string? value)))

(define-configuration/no-serialization openclaw-gateway-configuration
  (package
    (file-like node-openclaw)
    "The OpenClaw package to run.")
  (provision
    (list-of-symbols '(openclaw-gateway))
    "Shepherd provision symbols for the gateway service.")
  (requirement
    (list-of-symbols '())
    "Shepherd service requirements.")
  (auto-start?
    (boolean #t)
    "Whether Shepherd should start the gateway automatically.")
  (state-directory
    (string "$HOME/.openclaw")
    "OpenClaw state directory exported as OPENCLAW_STATE_DIR.")
  (config-file
    (string "$HOME/.openclaw/openclaw.json")
    "OpenClaw configuration file exported as OPENCLAW_CONFIG_PATH.")
  (workspace-directory
    (string "$HOME/.openclaw/workspace")
    "Default agent workspace exported as OPENCLAW_WORKSPACE_DIR.")
  (log-file
    (string "$XDG_STATE_HOME/log/openclaw-gateway.log")
    "Shepherd stdout/stderr log file for the gateway process.")
  (port
    (openclaw-port 18789)
    "Gateway WebSocket and HTTP port.")
  (bind
    (openclaw-bind-mode "loopback")
    "Gateway bind mode.")
  (auth-mode
    (openclaw-auth-mode "token")
    "Gateway authentication mode.")
  (password-file
    (optional-string #f)
    "Optional file containing the gateway password for password auth.")
  (tailscale-mode
    (openclaw-tailscale-mode "off")
    "Tailscale exposure mode.")
  (tailscale-reset-on-exit?
    (boolean #f)
    "Whether OpenClaw should reset Tailscale serve/funnel state on shutdown.")
  (ws-log
    (openclaw-ws-log-style "auto")
    "WebSocket log style.")
  (force?
    (boolean #f)
    "Whether OpenClaw should kill an existing listener on the configured port.")
  (verbose?
    (boolean #f)
    "Whether OpenClaw should log verbosely to stdout/stderr.")
  (auto-onboard?
    (boolean #t)
    "Whether activation should create the initial token-auth local config and
workspace when the configured config file does not exist.  Existing configs are
left alone.")
  (onboard-extra-options
    (list-of-strings '())
    "Additional options passed to the one-time non-interactive onboarding run.")
  (extra-options
    (list-of-strings '())
    "Additional options passed to `openclaw gateway run'.")
  (environment-variables
    (list-of-strings '())
    "Additional NAME=VALUE environment variables for the gateway process.")
  (respawn?
    (boolean #t)
    "Whether Shepherd should respawn the gateway when it exits unexpectedly.")
  (stop-grace-period
    (openclaw-non-negative-integer 30)
    "Seconds to wait for clean shutdown before Shepherd sends SIGKILL."))

(define (openclaw-shepherd-service-name config)
  (let ((provision (openclaw-gateway-configuration-provision config)))
    (if (pair? provision)
        (symbol->string (car provision))
        "openclaw-gateway")))

(define (openclaw-core-environment-variables config)
  "Return the shell-visible OpenClaw environment for CONFIG."
  (let ((port (number->string (openclaw-gateway-configuration-port config)))
        (service-name (openclaw-shepherd-service-name config)))
    `(("OPENCLAW_STATE_DIR" .
       ,(openclaw-gateway-configuration-state-directory config))
      ("OPENCLAW_CONFIG_PATH" .
       ,(openclaw-gateway-configuration-config-file config))
      ("OPENCLAW_WORKSPACE_DIR" .
       ,(openclaw-gateway-configuration-workspace-directory config))
      ("OPENCLAW_GATEWAY_PORT" . ,port)
      ("OPENCLAW_GATEWAY_SERVICE_MANAGER" . "guix-home-shepherd")
      ("OPENCLAW_GATEWAY_SHEPHERD_SERVICE" . ,service-name))))

(define (openclaw-profile-service config)
  (list (openclaw-gateway-configuration-package config)))

(define (openclaw-environment-service config)
  (openclaw-core-environment-variables config))

(define (openclaw-activation config)
  "Return an activation gexp preparing OpenClaw state for CONFIG."
  (let ((package (openclaw-gateway-configuration-package config))
        (state-directory
         (openclaw-gateway-configuration-state-directory config))
        (config-file
         (openclaw-gateway-configuration-config-file config))
        (workspace-directory
         (openclaw-gateway-configuration-workspace-directory config))
        (log-file (openclaw-gateway-configuration-log-file config))
        (port (openclaw-gateway-configuration-port config))
        (bind (openclaw-gateway-configuration-bind config))
        (auth-mode (openclaw-gateway-configuration-auth-mode config))
        (tailscale-mode
         (openclaw-gateway-configuration-tailscale-mode config))
        (tailscale-reset-on-exit?
         (openclaw-gateway-configuration-tailscale-reset-on-exit? config))
        (auto-onboard?
         (openclaw-gateway-configuration-auto-onboard? config))
        (onboard-extra-options
         (openclaw-gateway-configuration-onboard-extra-options config))
        (service-name (openclaw-shepherd-service-name config)))
    #~(begin
        (use-modules (guix build utils)
                     (srfi srfi-13))

        (define home
          (or (getenv "HOME") "/tmp"))

        (define xdg-state-home
          (or (getenv "XDG_STATE_HOME")
              (string-append home "/.local/state")))

        (define xdg-config-home
          (or (getenv "XDG_CONFIG_HOME")
              (string-append home "/.config")))

        (define xdg-cache-home
          (or (getenv "XDG_CACHE_HOME")
              (string-append home "/.cache")))

        (define (replace-prefix path prefix replacement)
          (and (string-prefix? prefix path)
               (string-append replacement
                              (substring path (string-length prefix)))))

        (define (expand-path path)
          (or (and (string=? path "~") home)
              (replace-prefix path "~/" (string-append home "/"))
              (and (string=? path "$HOME") home)
              (replace-prefix path "$HOME/" (string-append home "/"))
              (and (string=? path "${HOME}") home)
              (replace-prefix path "${HOME}/" (string-append home "/"))
              (and (string=? path "$XDG_STATE_HOME") xdg-state-home)
              (replace-prefix path "$XDG_STATE_HOME/"
                              (string-append xdg-state-home "/"))
              (and (string=? path "${XDG_STATE_HOME}") xdg-state-home)
              (replace-prefix path "${XDG_STATE_HOME}/"
                              (string-append xdg-state-home "/"))
              (and (string=? path "$XDG_CONFIG_HOME") xdg-config-home)
              (replace-prefix path "$XDG_CONFIG_HOME/"
                              (string-append xdg-config-home "/"))
              (and (string=? path "${XDG_CONFIG_HOME}") xdg-config-home)
              (replace-prefix path "${XDG_CONFIG_HOME}/"
                              (string-append xdg-config-home "/"))
              (and (string=? path "$XDG_CACHE_HOME") xdg-cache-home)
              (replace-prefix path "$XDG_CACHE_HOME/"
                              (string-append xdg-cache-home "/"))
              (and (string=? path "${XDG_CACHE_HOME}") xdg-cache-home)
              (replace-prefix path "${XDG_CACHE_HOME}/"
                              (string-append xdg-cache-home "/"))
              path))

        (define (parent-directory path)
          (let ((index (string-rindex path #\/)))
            (cond
             ((not index) ".")
             ((zero? index) "/")
             (else (substring path 0 index)))))

        (define (call-with-temporary-environment variables thunk)
          (let ((old (map (lambda (entry)
                            (let ((name (car entry)))
                              (cons name (getenv name))))
                          variables)))
            (dynamic-wind
              (lambda ()
                (for-each (lambda (entry)
                            (setenv (car entry) (cdr entry)))
                          variables))
              thunk
              (lambda ()
                (for-each (lambda (entry)
                            (let ((name (car entry))
                                  (value (cdr entry)))
                              (if value
                                  (setenv name value)
                                  (unsetenv name))))
                          old)))))

        (let* ((state-directory (expand-path #$state-directory))
               (config-file (expand-path #$config-file))
               (workspace-directory (expand-path #$workspace-directory))
               (log-file (expand-path #$log-file))
               (log-directory (parent-directory log-file))
               (port #$(number->string port)))
          (mkdir-p state-directory)
          (mkdir-p (parent-directory config-file))
          (mkdir-p workspace-directory)
          (mkdir-p log-directory)
          (when #$auto-onboard?
            (unless (file-exists? config-file)
              (unless (string=? #$auth-mode "token")
                (error
                 (string-append
                  "OpenClaw initial onboarding only supports token auth; "
                  "create the config manually or disable auto-onboard?")
                 config-file))
              (format #t "Creating initial OpenClaw config: ~a~%" config-file)
              (force-output)
              (call-with-temporary-environment
               `(("HOME" . ,home)
                 ("XDG_STATE_HOME" . ,xdg-state-home)
                 ("XDG_CONFIG_HOME" . ,xdg-config-home)
                 ("XDG_CACHE_HOME" . ,xdg-cache-home)
                 ("OPENCLAW_STATE_DIR" . ,state-directory)
                 ("OPENCLAW_CONFIG_PATH" . ,config-file)
                 ("OPENCLAW_WORKSPACE_DIR" . ,workspace-directory)
                 ("OPENCLAW_GATEWAY_PORT" . ,port)
                 ("OPENCLAW_GATEWAY_SERVICE_MANAGER" .
                  "guix-home-shepherd")
                 ("OPENCLAW_GATEWAY_SHEPHERD_SERVICE" .
                  #$service-name))
               (lambda ()
                 (let ((status
                        (apply system*
                               #$(file-append package "/bin/openclaw")
                               (append
                                (list "onboard"
                                      "--non-interactive"
                                      "--accept-risk"
                                      "--mode" "local"
                                      "--auth-choice" "skip"
                                      "--skip-channels"
                                      "--skip-health"
                                      "--skip-search"
                                      "--skip-skills"
                                      "--skip-ui"
                                      "--skip-daemon"
                                      "--no-install-daemon"
                                      "--gateway-port" port
                                      "--gateway-bind" #$bind
                                      "--gateway-auth" "token"
                                      "--tailscale" #$tailscale-mode
                                      "--workspace" workspace-directory)
                                (if #$tailscale-reset-on-exit?
                                    '("--tailscale-reset-on-exit")
                                    '())
                                '#$onboard-extra-options))))
                   (unless (zero? status)
                     (error "OpenClaw onboarding failed" status)))))))))))

(define (openclaw-shepherd-service config)
  "Return a Shepherd service running the OpenClaw gateway in the foreground."
  (let ((package (openclaw-gateway-configuration-package config))
        (provision (openclaw-gateway-configuration-provision config))
        (requirement (openclaw-gateway-configuration-requirement config))
        (auto-start? (openclaw-gateway-configuration-auto-start? config))
        (state-directory
         (openclaw-gateway-configuration-state-directory config))
        (config-file
         (openclaw-gateway-configuration-config-file config))
        (workspace-directory
         (openclaw-gateway-configuration-workspace-directory config))
        (log-file (openclaw-gateway-configuration-log-file config))
        (port (openclaw-gateway-configuration-port config))
        (bind (openclaw-gateway-configuration-bind config))
        (auth-mode (openclaw-gateway-configuration-auth-mode config))
        (password-file
         (openclaw-gateway-configuration-password-file config))
        (tailscale-mode
         (openclaw-gateway-configuration-tailscale-mode config))
        (tailscale-reset-on-exit?
         (openclaw-gateway-configuration-tailscale-reset-on-exit? config))
        (ws-log (openclaw-gateway-configuration-ws-log config))
        (force? (openclaw-gateway-configuration-force? config))
        (verbose? (openclaw-gateway-configuration-verbose? config))
        (extra-options
         (openclaw-gateway-configuration-extra-options config))
        (environment-variables
         (openclaw-gateway-configuration-environment-variables config))
        (respawn? (openclaw-gateway-configuration-respawn? config))
        (stop-grace-period
         (openclaw-gateway-configuration-stop-grace-period config))
        (service-name (openclaw-shepherd-service-name config)))
    (list
     (shepherd-service
      (provision provision)
      (documentation "Run the OpenClaw Gateway as a persistent Home service.")
      (requirement requirement)
      (modules '((guix build utils)
                 (srfi srfi-13)))
      (auto-start? auto-start?)
      (respawn? respawn?)
      (start #~(let* ((home (or (getenv "HOME") "/tmp"))
                      (xdg-state-home
                       (or (getenv "XDG_STATE_HOME")
                           (string-append home "/.local/state")))
                      (xdg-config-home
                       (or (getenv "XDG_CONFIG_HOME")
                           (string-append home "/.config")))
                      (xdg-cache-home
                       (or (getenv "XDG_CACHE_HOME")
                           (string-append home "/.cache")))
                      (replace-prefix
                       (lambda (path prefix replacement)
                         (and (string-prefix? prefix path)
                              (string-append
                               replacement
                               (substring path (string-length prefix))))))
                      (expand-path
                       (lambda (path)
                         (or (and (string=? path "~") home)
                             (replace-prefix path "~/"
                                             (string-append home "/"))
                             (and (string=? path "$HOME") home)
                             (replace-prefix path "$HOME/"
                                             (string-append home "/"))
                             (and (string=? path "${HOME}") home)
                             (replace-prefix path "${HOME}/"
                                             (string-append home "/"))
                             (and (string=? path "$XDG_STATE_HOME")
                                  xdg-state-home)
                             (replace-prefix path "$XDG_STATE_HOME/"
                                             (string-append xdg-state-home
                                                            "/"))
                             (and (string=? path "${XDG_STATE_HOME}")
                                  xdg-state-home)
                             (replace-prefix path "${XDG_STATE_HOME}/"
                                             (string-append xdg-state-home
                                                            "/"))
                             (and (string=? path "$XDG_CONFIG_HOME")
                                  xdg-config-home)
                             (replace-prefix path "$XDG_CONFIG_HOME/"
                                             (string-append xdg-config-home
                                                            "/"))
                             (and (string=? path "${XDG_CONFIG_HOME}")
                                  xdg-config-home)
                             (replace-prefix path "${XDG_CONFIG_HOME}/"
                                             (string-append xdg-config-home
                                                            "/"))
                             (and (string=? path "$XDG_CACHE_HOME")
                                  xdg-cache-home)
                             (replace-prefix path "$XDG_CACHE_HOME/"
                                             (string-append xdg-cache-home
                                                            "/"))
                             (and (string=? path "${XDG_CACHE_HOME}")
                                  xdg-cache-home)
                             (replace-prefix path "${XDG_CACHE_HOME}/"
                                             (string-append xdg-cache-home
                                                            "/"))
                             path)))
                      (parent-directory
                       (lambda (path)
                         (let ((index (string-rindex path #\/)))
                           (cond
                            ((not index) ".")
                            ((zero? index) "/")
                            (else (substring path 0 index))))))
                      (state-directory (expand-path #$state-directory))
                      (config-file (expand-path #$config-file))
                      (workspace-directory (expand-path #$workspace-directory))
                      (log-file (expand-path #$log-file))
                      (password-file
                       #$(and password-file #~(expand-path #$password-file)))
                      (environment-variable-name
                       (lambda (entry)
                         (let ((separator (string-index entry #\=)))
                           (and separator
                                (string-take entry separator)))))
                      (core-environment-variables
                       (list
                        (string-append "HOME=" home)
                        (string-append "XDG_STATE_HOME=" xdg-state-home)
                        (string-append "XDG_CONFIG_HOME=" xdg-config-home)
                        (string-append "XDG_CACHE_HOME=" xdg-cache-home)
                        (string-append "OPENCLAW_STATE_DIR="
                                       state-directory)
                        (string-append "OPENCLAW_CONFIG_PATH="
                                       config-file)
                        (string-append "OPENCLAW_WORKSPACE_DIR="
                                       workspace-directory)
                        (string-append "OPENCLAW_GATEWAY_PORT="
                                       #$(number->string port))
                        "OPENCLAW_GATEWAY_SERVICE_MANAGER=guix-home-shepherd"
                        (string-append "OPENCLAW_GATEWAY_SHEPHERD_SERVICE="
                                       #$service-name)
                        "OPENCLAW_SERVICE_MARKER=openclaw"
                        "OPENCLAW_SERVICE_KIND=gateway"))
                      (core-environment-variable-names
                       (map environment-variable-name
                            core-environment-variables))
                      (extra-environment-variables
                       (let loop ((entries '#$environment-variables)
                                  (result '()))
                         (if (null? entries)
                             (reverse result)
                             (let* ((entry (car entries))
                                    (name
                                     (environment-variable-name entry)))
                               (loop (cdr entries)
                                     (if (and name
                                              (member
                                               name
                                               core-environment-variable-names))
                                         result
                                         (cons entry result)))))))
                      (override-environment-variables
                       (append core-environment-variables
                               extra-environment-variables))
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
                        override-environment-variables))
                      (command
                       (append
                        (list #$(file-append package "/bin/openclaw")
                              "gateway" "run"
                              "--port" #$(number->string port)
                              "--bind" #$bind
                              "--auth" #$auth-mode
                              "--tailscale" #$tailscale-mode
                              "--ws-log" #$ws-log)
                        (if password-file
                            (list "--password-file" password-file)
                            '())
                        (if #$tailscale-reset-on-exit?
                            '("--tailscale-reset-on-exit")
                            '())
                        (if #$force? '("--force") '())
                        (if #$verbose? '("--verbose") '())
                        '#$extra-options)))
                 (mkdir-p state-directory)
                 (mkdir-p (parent-directory config-file))
                 (mkdir-p workspace-directory)
                 (mkdir-p (parent-directory log-file))
                 (make-forkexec-constructor
                   command
                   #:directory home
                   #:log-file log-file
                   #:environment-variables effective-environment-variables)))
      (stop #~(make-kill-destructor 15
                                    #:grace-period #$stop-grace-period))))))

(define home-openclaw-gateway-service-type
  (service-type
   (name 'home-openclaw-gateway)
   (description
    "Run OpenClaw Gateway under the Home Shepherd.")
   (extensions
    (list (service-extension home-profile-service-type
                             openclaw-profile-service)
          (service-extension home-environment-variables-service-type
                             openclaw-environment-service)
          (service-extension home-activation-service-type
                             openclaw-activation)
          (service-extension home-shepherd-service-type
                             openclaw-shepherd-service)))
   (default-value (openclaw-gateway-configuration))))

(define home-openclaw-service-type home-openclaw-gateway-service-type)

(define* (home-openclaw-gateway-service
          #:key
          (package node-openclaw)
          (provision '(openclaw-gateway))
          (requirement '())
          (auto-start? #t)
          (state-directory "$HOME/.openclaw")
          (config-file "$HOME/.openclaw/openclaw.json")
          (workspace-directory "$HOME/.openclaw/workspace")
          (log-file "$XDG_STATE_HOME/log/openclaw-gateway.log")
          (port 18789)
          (bind "loopback")
          (auth-mode "token")
          (password-file #f)
          (tailscale-mode "off")
          (tailscale-reset-on-exit? #f)
          (ws-log "auto")
          (force? #f)
          (verbose? #f)
          (auto-onboard? #t)
          (onboard-extra-options '())
          (extra-options '())
          (environment-variables '())
          (respawn? #t)
          (stop-grace-period 30))
  "Convenience constructor for `home-openclaw-gateway-service-type'."
  (service home-openclaw-gateway-service-type
           (openclaw-gateway-configuration
            (package package)
            (provision provision)
            (requirement requirement)
            (auto-start? auto-start?)
            (state-directory state-directory)
            (config-file config-file)
            (workspace-directory workspace-directory)
            (log-file log-file)
            (port port)
            (bind bind)
            (auth-mode auth-mode)
            (password-file password-file)
            (tailscale-mode tailscale-mode)
            (tailscale-reset-on-exit? tailscale-reset-on-exit?)
            (ws-log ws-log)
            (force? force?)
            (verbose? verbose?)
            (auto-onboard? auto-onboard?)
            (onboard-extra-options onboard-extra-options)
            (extra-options extra-options)
            (environment-variables environment-variables)
            (respawn? respawn?)
            (stop-grace-period stop-grace-period))))

(define home-openclaw-service home-openclaw-gateway-service)
