(define-module (myguix services authentication)
  #:use-module (gnu)
  #:use-module (gnu services dbus)
  #:use-module (gnu services configuration)
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

(define (fprintd-unix-pam-entry)
  (pam-entry (control "sufficient")
             (module "pam_unix.so")
             (arguments '("try_first_pass" "likeauth" "nullok"))))

(define (fprintd-pam-other-services config fprintd-module)
  (lambda (pam)
    (if (member (pam-service-name pam)
                (fprintd-configuration-unlock-other config))
        (let ((fprintd-entry (fprintd-pam-entry fprintd-module))
              (unix-entry (fprintd-unix-pam-entry)))
          (pam-service (inherit pam)
                       (auth (cons fprintd-entry
                                   (cons unix-entry
                                         (pam-service-auth pam)))))) pam)))

(define (fprintd-pam-gdm-services fprintd-module)
  (list
   ;; Modify gdm-fingerprint, if used (this is less common)
   (pam-service (inherit (unix-pam-service "gdm-fingerprint"
                                           #:login-uid? #t))
                (auth (list (pam-entry (control "required")
                                       (module fprintd-module)))))
   ;; Modify gdm-password for fingerprint authentication
   (pam-service (inherit (unix-pam-service "gdm-password"
                                           #:login-uid? #t))
                (auth (list (fprintd-pam-entry fprintd-module)
                            (fprintd-unix-pam-entry)
                            (pam-entry (control "include")
                                       (module "system-login")))))
   ;; Optionally, modify gdm-launch-environment
   (pam-service (inherit (unix-pam-service "gdm-launch-environment"))
                (auth (list (fprintd-pam-entry fprintd-module)
                            (fprintd-unix-pam-entry)
                            (pam-entry (control "include")
                                       (module "system-login")))))))

(define (fprintd-pam-services config)
  (let ((fprintd-module (file-append (fprintd-configuration-fprintd config)
                                     "/lib/security/pam_fprintd.so")))
    (cons (fprintd-pam-other-services config fprintd-module)
          (if (fprintd-configuration-unlock-gdm? config)
              (fprintd-pam-gdm-services fprintd-module)
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
                                                     fprintd-pam-services)))
                (default-value (fprintd-configuration))
                (description
                 "Run fprintd, a fingerprint management daemon, with optional support for unlocking GDM and other PAM services using fingerprint authentication.")))

