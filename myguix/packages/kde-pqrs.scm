;;; Copyright © 2025 Ayan Das <bitsayan@gmail.com>

(define-module (myguix packages kde-pqrs)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public latte-dock
  (package
    (name "latte-dock")
    (version "0.10.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/latte-dock/"
                                  "latte-dock-" version ".tar.xz"))
              (sha256
               (base32
                "0zj818wpxdiqpzivvwrgbzj26lcmmv49zw8206v4shcms1afbl9j"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  qtx11extras
                  kactivities
                  karchive-5
                  kcoreaddons-5
                  kcrash-5
                  kdbusaddons-5
                  kdeclarative-5
                  kglobalaccel-5
                  kguiaddons-5
                  ki18n-5
                  kiconthemes-5
                  kio-5
                  kirigami-5
                  knewstuff-5
                  knotifications-5
                  kwayland-5
                  kwindowsystem-5
                  kxmlgui-5
                  plasma-framework
                  libsm
                  xcb-util))
    (synopsis "Latte is a dock based on plasma frameworks")
    (description
     "Latte is a dock based on plasma frameworks that provides
an elegant and intuitive experience for your tasks and plasmoids.")
    (home-page "https://github.com/KDE/latte-dock")
    (license license:gpl2+)))