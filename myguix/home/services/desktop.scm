(define-module (myguix home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (myguix home services base)
  #:export (;; Environment variables
            %wayland-environment-variables
            
            ;; XDG directories
            %desktop-xdg-directories
            
            ;; MIME applications
            %gnome-mimeapps
            %kde-mimeapps
            
            ;; Scheduled jobs
            %garbage-collector-job
            
            ;; Main service bundle
            %my-desktop-home-services))

;;; Environment variables

;; Wayland-specific variables
(define-public %wayland-environment-variables
  `(("MOZ_ENABLE_WAYLAND" . "1")
    ("QT_QPA_PLATFORM" . "wayland;xcb")
    ("_JAVA_AWT_WM_NONREPARENTING" . #t)
    ;; Additional Wayland hints
    ("SDL_VIDEODRIVER" . "wayland")
    ("CLUTTER_BACKEND" . "wayland")
    ("GDK_BACKEND" . "wayland,x11")))

;;; XDG Directories

;; Desktop-specific directories
(define-public %desktop-xdg-directories
  '(;; Desktop integration
    "~/.local/share/Trash"
    "~/.local/share/desktop-directories"
    "~/.local/share/keyrings"
    
    ;; Browser/web
    "~/.cache/mozilla"
    "~/.cache/chromium"
    
    ;; Media
    "~/.cache/mesa_shader_cache"
    "~/.cache/gstreamer-1.0"
    "~/.cache/thumbnails"
    
    ;; Security
    "~/.local/share/password-store"
    
    ;; Desktop environment specific
    "~/.cache/dconf"
    "~/.config/dconf"
    "~/.config/gtk-3.0"
    "~/.config/gtk-4.0"))

;;; MIME applications

(define-public %gnome-mimeapps
  (plain-file "mimeapps.list"
              "[Default Applications]
text/html=firefox.desktop
x-scheme-handler/http=firefox.desktop
x-scheme-handler/https=firefox.desktop
x-scheme-handler/about=firefox.desktop
x-scheme-handler/unknown=firefox.desktop
application/pdf=org.gnome.Evince.desktop
image/png=org.gnome.eog.desktop
image/jpeg=org.gnome.eog.desktop
image/gif=org.gnome.eog.desktop
image/webp=org.gnome.eog.desktop
video/mp4=mpv.desktop
video/x-matroska=mpv.desktop
video/webm=mpv.desktop
audio/mpeg=org.gnome.Rhythmbox3.desktop
audio/flac=org.gnome.Rhythmbox3.desktop
audio/mp3=org.gnome.Rhythmbox3.desktop
text/plain=org.gnome.TextEditor.desktop
text/x-c=org.gnome.TextEditor.desktop
text/x-python=org.gnome.TextEditor.desktop
application/x-shellscript=org.gnome.TextEditor.desktop
inode/directory=org.gnome.Nautilus.desktop

[Added Associations]
text/html=firefox.desktop;
x-scheme-handler/http=firefox.desktop;
x-scheme-handler/https=firefox.desktop;
application/pdf=org.gnome.Evince.desktop;
image/png=org.gnome.eog.desktop;
image/jpeg=org.gnome.eog.desktop;"))

(define-public %kde-mimeapps
  (plain-file "mimeapps.list"
              "[Default Applications]
text/html=firefox.desktop
x-scheme-handler/http=firefox.desktop
x-scheme-handler/https=firefox.desktop
x-scheme-handler/about=firefox.desktop
x-scheme-handler/unknown=firefox.desktop
application/pdf=okular.desktop
image/png=gwenview.desktop
image/jpeg=gwenview.desktop
image/gif=gwenview.desktop
image/webp=gwenview.desktop
video/mp4=mpv.desktop
video/x-matroska=mpv.desktop
video/webm=mpv.desktop
audio/mpeg=elisa.desktop
audio/flac=elisa.desktop
audio/mp3=elisa.desktop
text/plain=kate.desktop
text/x-c=kate.desktop
text/x-python=kate.desktop
application/x-shellscript=kate.desktop
inode/directory=dolphin.desktop

[Added Associations]
text/html=firefox.desktop;
x-scheme-handler/http=firefox.desktop;
x-scheme-handler/https=firefox.desktop;
application/pdf=okular.desktop;
image/png=gwenview.desktop;
image/jpeg=gwenview.desktop;"))

;;; Scheduled jobs

(define-public %garbage-collector-job
  #~(job "0 0 * * 0"  ; Weekly on Sunday at midnight
         (string-append #$(file-append guix "/bin/guix") " gc --optimize")))

;;; Desktop home services

(define-public %my-desktop-home-services
  (append
   ;; Start with base services
   %my-base-home-services
   
   (list
    ;; Desktop bus
    (service home-dbus-service-type)
    
    ;; Sound
    (service home-pipewire-service-type)
    
    ;; Desktop directories
    (xdg-directory-service %desktop-xdg-directories)
    
    ;; Wayland environment variables
    (simple-service 'wayland-environment
                    home-environment-variables-service-type
                    %wayland-environment-variables)
    
    ;; Scheduled maintenance
    (service home-mcron-service-type
             (home-mcron-configuration
              (jobs (list %garbage-collector-job)))))))