(define-module (myguix services desktop)
  #:use-module (gnu)
  #:use-module (gnu services mcron)
  #:use-module (gnu services admin)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services sound)
  #:use-module (myguix services base)
  #:export (%my-gnome-shell-assets %my-desktop-services))

(define garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  #~(job "5 0 * * *" "guix gc -F 1G"))

(define %my-gnome-shell-assets
  (list (specification->package "pinentry")
        (specification->package "gnome-tweaks")
        (specification->package "gnome-shell-extensions")
        (specification->package "gnome-shell-extension-dash-to-dock")
        (specification->package "gnome-shell-extension-vitals")
        (specification->package "gnome-shell-extension-gsconnect")
        (specification->package "gnome-shell-extension-just-perfection")
        (specification->package "adwaita-icon-theme")
        (specification->package "font-abattis-cantarell")
        (specification->package "flat-remix-icon-theme")
        (specification->package "flat-remix-gtk-theme")
        (specification->package "flat-remix-gnome-theme")
        (specification->package "bibata-cursor-theme")))

(define %my-desktop-services
  (append (list
           ;; Scheduled Job Execution
           (service mcron-service-type
                    (mcron-configuration (jobs (list garbage-collector-job))))

           ;; Log Rotation
           (service rottlog-service-type)
           (service log-cleanup-service-type
                    (log-cleanup-configuration (directory "/var/log/guix/drvs")))

           ;; X Window
           (service gdm-service-type
                    (gdm-configuration (gnome-shell-assets
                                        %my-gnome-shell-assets)))
           gdm-file-system-service
           fontconfig-file-system-service
           (service x11-socket-directory-service-type)
           (service screen-locker-service-type
                    (screen-locker-configuration (name "xlockmore")
                                                 (program (file-append (specification->package
                                                                        "xlockmore")
                                                           "/bin/xlockmore"))
                                                 (using-pam? #t)
                                                 (using-setuid? #f)))

           ;; Printing Services
           (service cups-pk-helper-service-type)

           ;; Desktop Services
           (service gnome-desktop-service-type)
           (service dbus-root-service-type)
           (service elogind-service-type)
           (service accountsservice-service-type)
           (service polkit-service-type)
           polkit-wheel-service
           (service upower-service-type)
           (service udisks-service-type)
           (service gvfs-service-type)
           (service colord-service-type)
           (service sane-service-type)
           (service geoclue-service-type)
           (service bluetooth-service-type
                    (bluetooth-configuration (auto-enable? #t)))
           (service gnome-keyring-service-type)

           ;; Sound Services
           (service alsa-service-type)
           (service pulseaudio-service-type)

           ;; File Search Services
           (service file-database-service-type)
           (service package-database-service-type)) %my-base-services))
