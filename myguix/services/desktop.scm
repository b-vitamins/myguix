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
   gnome-shell-extensions

   ;; Essential extensions
   gnome-shell-extension-dash-to-dock ;Better dock
   gnome-shell-extension-vitals ;System monitoring
   gnome-shell-extension-appindicator ;System tray support
   gnome-shell-extension-blur-my-shell ;Aesthetic blur effects (Wayland-friendly)
   gnome-shell-extension-gsconnect ;KDE Connect for GNOME
   
   ;; Theme components
   adwaita-icon-theme ;Stock GNOME icons
   font-abattis-cantarell ;GNOME default font
   bibata-cursor-theme))
; Modern cursor theme

(define %my-desktop-services
  (append (list
           ;; Display Manager and X11/Wayland
           (service gdm-service-type
                    (gdm-configuration (wayland? #t)
                                       (debug? #f)))

           ;; File system services for desktop
           gdm-file-system-service
           fontconfig-file-system-service

           ;; X11 socket directory (still needed for XWayland)
           (service x11-socket-directory-service-type)

           ;; Screen lockers (for X11 sessions)
           (service screen-locker-service-type
                    (screen-locker-configuration (name "slock")
                                                 (program (file-append slock
                                                           "/bin/slock"))))

           ;; Networking
           (service network-manager-service-type
                    (network-manager-configuration (vpn-plugins (list
                                                                 network-manager-openvpn
                                                                 network-manager-openconnect))
                                                   (dns "default"))) ;Use systemd-resolved or default
           
           (service wpa-supplicant-service-type)
           (service modem-manager-service-type)
           (service usb-modeswitch-service-type)

           ;; Time synchronization
           (service ntp-service-type
                    (ntp-configuration (servers '("0.pool.ntp.org"
                                                  "1.pool.ntp.org"
                                                  "2.pool.ntp.org"
                                                  "3.pool.ntp.org"))))

           ;; Printing
           (service cups-pk-helper-service-type)

           ;; Core Desktop Services
           (service dbus-root-service-type)

           (service elogind-service-type
                    (elogind-configuration (suspend-state '("mem"))
                                           (suspend-mode '("deep"))
                                           (hibernate-state '("disk"))
                                           (hibernate-mode '("platform"))
                                           (hybrid-sleep-state '("disk"))
                                           (hybrid-sleep-mode '("suspend"
                                                                "platform"))
                                           (handle-lid-switch 'suspend)
                                           (handle-lid-switch-docked 'ignore)
                                           (handle-lid-switch-external-power 'suspend)
                                           (idle-action 'ignore)
                                           (idle-action-seconds 1800)))

           (service accountsservice-service-type)
           (service polkit-service-type)
           polkit-wheel-service

           ;; Power management
           (service upower-service-type
                    (upower-configuration (use-percentage-for-policy? #t)
                                          (percentage-low 20)
                                          (percentage-critical 10)
                                          (percentage-action 5)
                                          (critical-power-action 'hybrid-sleep)))

           (service thermald-service-type) ;Intel thermal management
           
           ;; Storage management
           (service udisks-service-type)

           ;; Network discovery
           (service avahi-service-type
                    (avahi-configuration (wide-area? #t)
                                         (publish? #t)
                                         (publish-workstation? #t)))

           ;; File access
           (service gvfs-service-type)

           ;; Color management
           (service colord-service-type)

           ;; Scanner support
           (service sane-service-type)

           ;; Location services
           (service geoclue-service-type
                    (geoclue-configuration (applications (list (geoclue-application
                                                                "gnome-datetime-panel.desktop"
                                                                #:allowed? #t)
                                                               (geoclue-application
                                                                "org.gnome.Weather"
                                                                #:allowed? #t)
                                                               (geoclue-application
                                                                "org.gnome.Maps"
                                                                #:allowed? #t)))))

           ;; Bluetooth
           (service bluetooth-service-type
                    (bluetooth-configuration (auto-enable? #t)
                                             (fast-connectable? #t)
                                             (just-works-repairing 'confirm)
                                             (privacy 'device)
                                             (experimental #f)))

           ;; Sound Services
           (service alsa-service-type)

           ;; Note: PipeWire would need to be configured at user/home level
           ;; not as a system service
           
           ;; File indexing and search
           (service file-database-service-type
                    (file-database-configuration (schedule "0 3 * * *"))) ;Run at 3 AM daily
           
           ;; Package database for command-not-found functionality
           (service package-database-service-type))

          ;; Append base services
          %my-base-services))
