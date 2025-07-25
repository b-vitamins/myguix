;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2023 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2023, 2024 altadil <Altadil@protonmail.com>
;;; Copyright © 2024 jgart <jgart@dismail.de>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (myguix packages pantheon)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; TODO: Missing Pantheon Desktop Shell Components (Critical)
;; - gala (window manager/compositor)
;; - wingpanel (top panel)
;; - plank (dock)
;; - pantheon-applications-menu (app launcher)
;; - pantheon-settings-daemon (settings daemon)
;; - pantheon-system-settings/switchboard (settings app)
;; - pantheon-session (session manager)
;; - pantheon-desktop (desktop files/settings)
;;
;; TODO: Missing Wingpanel Indicators (Essential for panel functionality)
;; - wingpanel-indicator-datetime
;; - wingpanel-indicator-session
;; - wingpanel-indicator-power
;; - wingpanel-indicator-sound
;; - wingpanel-indicator-network
;; - wingpanel-indicator-notifications
;; - wingpanel-indicator-keyboard
;;
;; TODO: Missing Switchboard Plugs (Settings modules)
;; - switchboard-plug-display
;; - switchboard-plug-keyboard
;; - switchboard-plug-mouse-touchpad
;; - switchboard-plug-power
;; - switchboard-plug-sound
;; - switchboard-plug-network
;; - switchboard-plug-desktop
;; - switchboard-plug-user-accounts
;; - switchboard-plug-datetime
;; - switchboard-plug-about
;; - switchboard-plug-pantheon-shell
;;
;; TODO: Missing Elementary OS Applications
;; - pantheon-code (code editor)
;; - pantheon-files (file manager)
;; - pantheon-mail (email client)
;; - pantheon-music (music player)
;; - pantheon-videos (video player)
;; - pantheon-camera (camera app)
;; - pantheon-tasks (task manager)
;; - elementary-appcenter (app store)
;;
;; TODO: Missing Pantheon Platform Components
;; - contractor (service for context actions)
;; - pantheon-agent-polkit (authentication dialogs)
;; - capnet-assist (captive portal assistant)
;; - pantheon-flatpak-platform (Flatpak runtime/SDK)

(define-public granite
  (package
    (name "granite")
    (version "7.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/granite")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pyvkif2kin5dskh7adadsh4r96mvx12y7cs6gnm0ml733q548dj"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-icon-cache
                    (lambda _
                      (setenv "DESTDIR" "/"))))))
    (inputs (list sassc))
    (propagated-inputs (list glib libgee gtk)) ;required in .pc file
    (native-inputs (list gettext-minimal
                         `(,glib "bin")
                         gobject-introspection
                         pkg-config
                         python
                         vala))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description
     "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))

;; This is required for pantheon apps that have not been ported to GTK4 yet.
(define-public granite-6
  (package
    (inherit granite)
    (version "6.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/granite")
             (commit version)))
       (file-name (git-file-name "granite" version))
       (sha256
        (base32 "0ilslmg63hh2x7h5rvs3mhzw1y9ixhhkqnn1j1lzwm12v2iidkaq"))))
    (propagated-inputs (list glib libgee gtk+))))

(define-public pantheon-calculator
  (package
    (name "pantheon-calculator")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calculator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1as5rxd0b6z3lnh8my36szr056rxxqwkjzvaiylspx5g2kg3qjs0"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-schema-cache-generation
            (lambda _
              (setenv "DESTDIR" "/"))))))
    (inputs (list granite glib gtk libgee libhandy))
    (native-inputs (list cmake
                         `(,glib "bin") ;for glib-compile-schemas
                         gettext-minimal
                         pkg-config
                         vala))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description
     "Calculator is an application for performing simple
arithmetic.  It is the default calculator application in the Pantheon
desktop.")
    (license license:gpl3)))

(define-public pantheon-calendar
  (package
    (name "pantheon-calendar")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calendar")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bynv5gnfs4sdr5ngd1c8jh42fkiw4gl5064fb579hws2jniy540"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; Tests involve checking environment variable against particular TZ.
      #:tests? #f
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'set-environment-variables
            (lambda _
              ;; Disable compiling schemas and updating desktop databases
              (setenv "DESTDIR" "/")))
          (add-after 'install 'install-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin (string-append #$output
                                         "/bin/io.elementary.calendar"))
                     (link (string-append #$output "/bin/pantheon-calendar")))
                (symlink bin link)))))))
    (inputs (list clutter
                  evolution-data-server-3.44
                  folks-with-libsoup2
                  geoclue
                  geocode-glib-with-libsoup2
                  granite-6
                  glib
                  gtk
                  libchamplain
                  libgee
                  libhandy
                  libical
                  libportal))
    (native-inputs (list cmake
                         `(,glib "bin") ;for glib-compile-schemas
                         gettext-minimal
                         pkg-config
                         vala))
    (home-page "https://github.com/elementary/calendar")
    (synopsis "Desktop calendar")
    (description "This package provides a desktop calendar app designed for
elementary OS.")
    (license license:gpl3+)))

(define-public pantheon-icons
  (package
    (name "pantheon-icons")
    (version "8.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/icons")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yvjisvcjdpgibnc5l5cm16rw53zffinp1pvknlllz8wcdylqnss"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dvolume_icons=false" "-Dpalettes=false")))
    (native-inputs (list gettext-minimal ;for msgfmt
                         librsvg xcursorgen))
    (propagated-inputs (list hicolor-icon-theme))
    (synopsis "Named, vector icons for the pantheon desktop")
    (description "pantheon-icons is an original set of vector icons designed
for elementary OS and its desktop environment: Pantheon.")
    (home-page "https://elementary.io/open-source")
    (license license:gpl3)))

(define-public pantheon-photos
  (package
    (name "pantheon-photos")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/photos")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z3b582y093x6pb3bl7zs4w97vg88rflyhwxfaycxw0rv8pcshhi"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-schema-cache-generation
            (lambda _
              (setenv "DESTDIR" "/")))
          (add-after 'install 'install-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin/io.elementary.photos"))
                     (link (string-append #$output "/bin/pantheon-photos")))
                (symlink bin link)))))))
    (native-inputs (list desktop-file-utils
                         `(,glib "bin")
                         intltool
                         pkg-config
                         python
                         vala))
    (inputs (list geocode-glib
                  gexiv2
                  granite-6
                  gst-plugins-bad
                  gst-plugins-base
                  gst-plugins-good
                  gst-plugins-ugly
                  gstreamer
                  gtk+
                  libexif
                  libgee
                  libgphoto2
                  libgudev
                  libhandy
                  libportal
                  libraw
                  libwebp
                  sqlite))
    (synopsis "Photo viewer and organizer designed for the Pantheon desktop")
    (description
     "Photos is an image viewer and organizer.  It originally comes
from elementary OS and is designed for the Pantheon desktop environment (but can
also be used on others.")
    (home-page "https://elementary.io/open-source")
    (license license:lgpl2.1+)))

(define-public pantheon-screenshot
  (package
    (name "pantheon-screenshot")
    (version "8.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/screenshot")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h3xv0pckkkgvqkk6fxssydq9gmncapaf1hx4n7j19jcvhwx65da"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-schema-cache-generation
            (lambda _
              (setenv "DESTDIR" "/")))
          (add-after 'install 'install-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin (string-append #$output
                                         "/bin/io.elementary.screenshot"))
                     (link (string-append #$output "/bin/pantheon-screenshot")))
                (symlink bin link)))))))
    (native-inputs (list desktop-file-utils
                         gettext-minimal ;for msgfmt
                         `(,glib "bin")
                         pkg-config
                         vala))
    (inputs (list granite
                  gtk
                  libcanberra
                  libgee
                  libportal
                  libhandy))
    (propagated-inputs (list glib))
    (synopsis "Screenshot tool")
    (description "pantheon-screenshot is a screenshot tool designed for
the Pantheon desktop environment.")
    (home-page "https://elementary.io/open-source")
    (license license:lgpl3)))

(define-public pantheon-stylesheet
  (package
    (name "pantheon-stylesheet")
    (version "8.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/stylesheet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h8k75m9jrqxwq0py8hrqrsc83myrwpmnrwx05aljdyr6sys06k6"))))
    (build-system meson-build-system)
    (native-inputs (list gettext-minimal)) ;for msgfmt
    (inputs (list sassc))
    (synopsis "GTK stylesheet for the Pantheon desktop")
    (description "pantheon-stylesheet is the GTK Stylesheet for the Pantheon
desktop environment (originally from elementary OS).")
    (home-page "https://elementary.io/open-source")
    (license license:gpl3+)))

(define-public pantheon-terminal
  (package
    (name "pantheon-terminal")
    (version "6.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/terminal")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "142nwx2jc7ks529dk8dqhgs39gdqh6bc7gv9b10qdfm81bwqjkjv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f ;Tests involve launching the terminal.
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'set-environment-variables
            (lambda _
              ;; Disable compiling schemas and updating desktop databases
              (setenv "DESTDIR" "/")))
          (add-after 'install 'install-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((bin (string-append #$output
                                         "/bin/io.elementary.terminal"))
                     (link (string-append #$output "/bin/pantheon-terminal")))
                (symlink bin link)))))))
    (native-inputs (list appstream
                         desktop-file-utils ;required for tests
                         gettext-minimal ;for msgfmt
                         `(,glib "bin") ;for glib-compile-resources
                         gobject-introspection
                         pkg-config
                         vala
                         xvfb-run))
    (inputs (list granite-6
                  gtk+
                  libgee
                  libhandy
                  pcre2
                  vte/gtk+-3))
    (synopsis "Terminal emulator from elementaryOS")
    (description
     "pantheon-terminal is a lightweight, beautiful and simple
terminal.  It comes with sane defaults, browser-class tabs, sudo paste
protection, smart copy/paste, and little to no configuration.  It is the default
terminal in the Pantheon desktop.")
    (home-page "https://elementary.io/open-source")
    (license license:lgpl3)))

(define-public sideload
  (package
    (name "sideload")
    (version "6.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/sideload")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vrj91899f13cvzpycqy3y74hmixsffjbzsj29da7n370fa3ci86"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'set-environment-variables
                    (lambda _
                      ;; Disable compiling schemas and updating desktop databases
                      (setenv "DESTDIR" "/")))
                  (add-after 'install 'install-symlinks
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out
                                   "/bin/io.elementary.sideload"))
                             (link (string-append out "/bin/sideload")))
                        (symlink bin link)))))))
    (inputs `(("flatpak" ,flatpak)
              ("glib" ,glib)
              ("granite" ,granite)
              ("gtk" ,gtk+)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("libgee" ,libgee)
              ("libhandy" ,libhandy)
              ("libostree" ,libostree)
              ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; Sideload needs these in the environment to fetch data securely from
     ;; Flatpak remotes.
     (list gnupg gpgme))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")
                     ("gobject-introspection" ,gobject-introspection)
                     ("pkg-config" ,pkg-config)
                     ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description
     "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3+)))

(define-public pantheon-wallpapers
  (package
    (name "pantheon-wallpapers")
    (version "7.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/wallpapers/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0km3h52kapbm8ymwxdxasz80qbgzkfni7981pdyf740wjp7linwb"))))
    (build-system meson-build-system)
    (native-inputs (list gettext-minimal)) ;for msgfmt
    (inputs (list libexif))
    (synopsis "Wallpapers for the Pantheon desktop")
    (description "This package provides wallpapers for the Pantheon desktop.")
    (home-page "https://github.com/elementary/wallpapers")
    (license (list license:cc-by-sa4.0 license:cc0
                   (license:non-copyleft "https://unsplash.com/license")
                   (license:non-copyleft "https://www.pexels.com/license/")))))
