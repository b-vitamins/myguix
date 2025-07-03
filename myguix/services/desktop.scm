(define-module (myguix services desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
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
  #:use-module (gnu services pm)
  #:use-module (myguix services base)
  #:export (%my-gnome-shell-assets %my-desktop-services))

(define %my-gnome-shell-assets
  (list
   ;; Core GNOME tools
   gnome-tweaks
   ;; Essential extensions
   gnome-shell-extension-dash-to-dock
   gnome-shell-extension-vitals
   gnome-shell-extension-appindicator
   gnome-shell-extension-gsconnect
   ;; Theme components
   adwaita-icon-theme
   font-abattis-cantarell
   bibata-cursor-theme))
; Modern cursor theme

(define %my-desktop-services
  (append (list
           ;; Display Manager and X11/Wayland
           (service gdm-service-type)

           ;; Screen lockers (for X11 sessions)
           (service screen-locker-service-type
                    (screen-locker-configuration (name "slock")
                                                 (program (file-append slock
                                                           "/bin/slock"))))

           ;; Scanner support (matches upstream 'sane-service-type)
           (service sane-service-type)

           ;; Add polkit rules, so that non-root users in the wheel group can
           ;; perform administrative tasks (similar to "sudo").
           polkit-wheel-service

           ;; File system services for desktop
           gdm-file-system-service

           ;; Provides a nicer experience for VTE-using terminal emulators such
           ;; as GNOME Console, Xfce Terminal, etc.
           (service vte-integration-service-type)

           ;; The global fontconfig cache directory
           fontconfig-file-system-service

           ;; NetworkManager and its applet.
           (service network-manager-service-type
                    (network-manager-configuration (vpn-plugins (list
                                                                 network-manager-openvpn
                                                                 network-manager-openconnect))))
           (service wpa-supplicant-service-type) ;needed by NetworkManager
           (service modem-manager-service-type)
           (service usb-modeswitch-service-type)

           ;; The D-Bus clique.
           (service avahi-service-type)
           (service udisks-service-type)
           (service upower-service-type)
           (service accountsservice-service-type)
           (service cups-pk-helper-service-type)
           (service colord-service-type)
           (service geoclue-service-type)
           (service polkit-service-type)
           (service elogind-service-type)
           (service dbus-root-service-type)

           (service ntp-service-type)

           (service x11-socket-directory-service-type)

           (service pulseaudio-service-type)
           (service alsa-service-type)

           ;; Extra services
           (service bluetooth-service-type
                    (bluetooth-configuration (auto-enable? #t)
                                             (fast-connectable? #t)
                                             (just-works-repairing 'confirm)
                                             (privacy 'device)
                                             (experimental #f)))

           (service thermald-service-type) ;Intel thermal management
           
           ;; File access
           (service gvfs-service-type)

           ;; File indexing and search
           (service file-database-service-type)

           ;; Package database for command-not-found functionality
           (service package-database-service-type))

          ;; Append base services
          %my-base-services))
