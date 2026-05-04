(define-module (myguix services authentication)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:export (fprintd-service-type))

(define-configuration/no-serialization fprintd-configuration
                                       (fprintd (file-like (specification->package
                                                            "fprintd"))
                                                "The fprintd package.")

                                       (unlock-gdm? (boolean #t)
                                        "Generate PAM configuration that unlocks GDM (GNOME Display Manager) with fprintd.")

                                       (unlock-other (list '("polkit-1" "sddm"
                                                             "login"
                                                             "su"
                                                             "sudo"
                                                             "system-local-login"
                                                             "gdm"))
                                        "List of other PAM services that can be unlocked with fprintd.

This depends on your desktop configuration. For example, if you want GNOME prompts to be unlocked by fingerprint, add `polkit-1` to this list. This is enabled by default."))

(define (fprintd-pam-entry fprintd-module)
  (pam-entry (control "sufficient")
             (module fprintd-module)))

(define (fprintd-pam-transformer config fprintd-module)
  (let ((service-names
         (append (fprintd-configuration-unlock-other config)
                 (if (fprintd-configuration-unlock-gdm? config)
                     '("gdm-password")
                     '()))))
    (pam-extension
     (transformer
      (lambda (pam)
        (if (member (pam-service-name pam) service-names)
            (pam-service (inherit pam)
                         (auth (cons (fprintd-pam-entry fprintd-module)
                                     (pam-service-auth pam))))
            pam))))))

(define (fprintd-gdm-fingerprint-pam-service fprintd-module)
  ;; GDM may look for a dedicated "gdm-fingerprint" service.  Provide it
  ;; explicitly, but transform existing GDM PAM services in place to avoid
  ;; colliding with Guix's own gdm-password and gdm-launch-environment files.
  (pam-service (inherit (unix-pam-service "gdm-fingerprint"
                                          #:login-uid? #t))
               (auth (list (pam-entry (control "required")
                                      (module fprintd-module))))))

(define (fprintd-pam-extensions config)
  (let ((fprintd-module (file-append (fprintd-configuration-fprintd config)
                                     "/lib/security/pam_fprintd.so")))
    (append (list (fprintd-pam-transformer config fprintd-module))
            (if (fprintd-configuration-unlock-gdm? config)
                (list (fprintd-gdm-fingerprint-pam-service fprintd-module))
                '()))))

(define (fprintd-dbus-service config)
  (list (fprintd-configuration-fprintd config)))

(define fprintd-service-type
  (service-type (name 'fprintd)
                (extensions (list (service-extension dbus-root-service-type
                                                     fprintd-dbus-service)
                                  (service-extension polkit-service-type
                                                     fprintd-dbus-service)
                                  (service-extension pam-root-service-type
                                                     fprintd-pam-extensions)))
                (default-value (fprintd-configuration))
                (description
                 "Run fprintd, a fingerprint management daemon, with optional support for unlocking GDM and other PAM services using fingerprint authentication.")))
