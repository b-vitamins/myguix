(define-module (myguix home services desktop)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (myguix home services base)
  #:export (;; Environment variables
            %wayland-environment-variables
            
            ;; XDG directories
            %desktop-xdg-directories
            
            ;; MIME applications
            mimeapps-list
            mimeapps-associations
            desktop-mimeapps-defaults
            home-mimeapps-service
            %gnome-mimeapps
            %kde-mimeapps
            
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

(define %browser-mime-types
  '("text/html"
    "x-scheme-handler/http"
    "x-scheme-handler/https"
    "x-scheme-handler/about"
    "x-scheme-handler/unknown"))

(define %image-mime-types
  '("image/png"
    "image/jpeg"
    "image/gif"
    "image/webp"))

(define %video-mime-types
  '("video/mp4"
    "video/x-matroska"
    "video/webm"))

(define %audio-mime-types
  '("audio/mpeg"
    "audio/flac"
    "audio/mp3"))

(define %text-mime-types
  '("text/plain"
    "text/x-c"
    "text/x-python"
    "application/x-shellscript"))

(define (mimeapps-associations desktop-entry mime-types)
  "Return MIME association pairs mapping MIME-TYPES to DESKTOP-ENTRY."
  (map (lambda (mime-type)
         (cons mime-type desktop-entry))
       mime-types))

(define (maybe-mimeapps-associations desktop-entry mime-types)
  (if desktop-entry
      (mimeapps-associations desktop-entry mime-types)
      '()))

(define* (mimeapps-entry-value value #:key (terminal-semicolon? #f))
  (string-append
   (string-join (if (list? value) value (list value)) ";")
   (if terminal-semicolon? ";" "")))

(define* (mimeapps-section title entries #:key (terminal-semicolon? #f))
  (if (null? entries)
      ""
      (string-append
       "[" title "]\n"
       (apply string-append
              (map (lambda (entry)
                     (string-append (car entry)
                                    "="
                                    (mimeapps-entry-value
                                     (cdr entry)
                                     #:terminal-semicolon? terminal-semicolon?)
                                    "\n"))
                   entries))
       "\n")))

(define* (mimeapps-list #:key
                        (name "mimeapps.list")
                        (defaults '())
                        (added-associations '())
                        (removed-associations '()))
  "Return a @file{mimeapps.list} file for the supplied XDG associations."
  (plain-file name
              (string-append
               (mimeapps-section "Default Applications" defaults)
               (mimeapps-section "Added Associations" added-associations
                                 #:terminal-semicolon? #t)
               (mimeapps-section "Removed Associations" removed-associations
                                 #:terminal-semicolon? #t))))

(define* (desktop-mimeapps-defaults #:key
                                    (browser #f)
                                    (pdf-viewer #f)
                                    (image-viewer #f)
                                    (video-player #f)
                                    (audio-player #f)
                                    (text-editor #f)
                                    (file-manager #f)
                                    (extra-defaults '()))
  "Return common desktop MIME defaults using caller-supplied desktop entries."
  (append
   (maybe-mimeapps-associations browser %browser-mime-types)
   (if pdf-viewer
       `(("application/pdf" . ,pdf-viewer))
       '())
   (maybe-mimeapps-associations image-viewer %image-mime-types)
   (maybe-mimeapps-associations video-player %video-mime-types)
   (maybe-mimeapps-associations audio-player %audio-mime-types)
   (maybe-mimeapps-associations text-editor %text-mime-types)
   (if file-manager
       `(("inode/directory" . ,file-manager))
       '())
   extra-defaults))

(define* (home-mimeapps-service #:key
                                (service-name 'mimeapps)
                                (defaults '())
                                (added-associations '())
                                (removed-associations '())
                                (file #f))
  "Return a Home service installing @file{mimeapps.list}.

When FILE is provided it is installed directly.  Otherwise it is generated
from DEFAULTS, ADDED-ASSOCIATIONS, and REMOVED-ASSOCIATIONS."
  (simple-service service-name
                  home-xdg-configuration-files-service-type
                  `(("mimeapps.list"
                     ,(or file
                          (mimeapps-list
                           #:defaults defaults
                           #:added-associations added-associations
                           #:removed-associations removed-associations))))))

(define-public %gnome-mimeapps
  (mimeapps-list
   #:defaults
   (desktop-mimeapps-defaults
    #:browser "firefox.desktop"
    #:pdf-viewer "org.gnome.Evince.desktop"
    #:image-viewer "org.gnome.Loupe.desktop"
    #:video-player "mpv.desktop"
    #:audio-player "org.gnome.Rhythmbox3.desktop"
    #:text-editor "org.gnome.TextEditor.desktop"
    #:file-manager "org.gnome.Nautilus.desktop")
   #:added-associations
   (append
    (mimeapps-associations "firefox.desktop"
                           '("text/html"
                             "x-scheme-handler/http"
                             "x-scheme-handler/https"))
    '(("application/pdf" . "org.gnome.Evince.desktop")
      ("image/png" . "org.gnome.Loupe.desktop")
      ("image/jpeg" . "org.gnome.Loupe.desktop")))))

(define-public %kde-mimeapps
  (mimeapps-list
   #:defaults
   (desktop-mimeapps-defaults
    #:browser "firefox.desktop"
    #:pdf-viewer "okular.desktop"
    #:image-viewer "gwenview.desktop"
    #:video-player "mpv.desktop"
    #:audio-player "elisa.desktop"
    #:text-editor "kate.desktop"
    #:file-manager "dolphin.desktop")
   #:added-associations
   (append
    (mimeapps-associations "firefox.desktop"
                           '("text/html"
                             "x-scheme-handler/http"
                             "x-scheme-handler/https"))
    '(("application/pdf" . "okular.desktop")
      ("image/png" . "gwenview.desktop")
      ("image/jpeg" . "gwenview.desktop")))))


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
                    %wayland-environment-variables))))
