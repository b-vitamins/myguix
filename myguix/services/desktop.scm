(define-module (myguix services desktop)
  #:use-module (gnu)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dbus)
  #:use-module (gnu services sound)
  #:use-module (myguix services base)
  #:export (%my-gnome-shell-assets %my-desktop-services))

(define %my-gnome-shell-assets
  (list gnome-tweaks
        gnome-shell-extensions
        gnome-shell-extension-dash-to-dock
        gnome-shell-extension-vitals
        gnome-shell-extension-just-perfection
        gnome-shell-extension-unite-shell
        adwaita-icon-theme
        font-abattis-cantarell
        flat-remix-icon-theme
        flat-remix-gtk-theme
        flat-remix-gnome-theme
        bibata-cursor-theme))

(define %my-desktop-services
  (append (list
           ;; Log Rotation
           (service log-rotation-service-type)
           (service log-cleanup-service-type
                    (log-cleanup-configuration (directory "/var/log/guix/drvs")))

           ;; X Window
           (service gdm-service-type
                    (gdm-configuration (gnome-shell-assets
                                        %my-gnome-shell-assets)))
           gdm-file-system-service
           fontconfig-file-system-service
           (service x11-socket-directory-service-type)

           ;; Screen lockers
           (service screen-locker-service-type
                    (screen-locker-configuration (name "slock")
                                                 (program (file-append slock
                                                           "/bin/slock"))))
           (service screen-locker-service-type
                    (screen-locker-configuration (name "xlock")
                                                 (program (file-append
                                                           xlockmore
                                                           "/bin/xlock"))))

           ;; Networking Setup
           (service network-manager-service-type
                    (network-manager-configuration (vpn-plugins (list
                                                                 network-manager-openvpn
                                                                 network-manager-openconnect))))
           (service wpa-supplicant-service-type)
           (service modem-manager-service-type)
           (service usb-modeswitch-service-type)
           (service ntp-service-type)

           ;; Printing Services
           (service cups-pk-helper-service-type)

           ;; Desktop Services
           (service dbus-root-service-type)
           (service elogind-service-type)
           (service accountsservice-service-type)
           (service polkit-service-type)
           polkit-wheel-service
           (service upower-service-type)
           (service udisks-service-type)
           (service avahi-service-type)
           (service gvfs-service-type)
           (service colord-service-type)
           (service sane-service-type)
           (service geoclue-service-type)
           (service bluetooth-service-type
                    (bluetooth-configuration (auto-enable? #t)))

           ;; Sound Services
           (service alsa-service-type)
           (service pulseaudio-service-type)

           ;; File Search Services
           (service file-database-service-type)
           (service package-database-service-type)) %my-base-services))
