;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Ayan Das <bvits@riseup.net>

(define-module (myguix services nvidia-containers)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (myguix packages nvidia-containers)
  #:export (nvidia-container-toolkit-configuration
            nvidia-container-toolkit-configuration?
            nvidia-container-toolkit-configuration-record?
            nvidia-container-toolkit-service-type))

(define-record-type* <nvidia-container-toolkit-configuration>
  nvidia-container-toolkit-configuration
  make-nvidia-container-toolkit-configuration
  nvidia-container-toolkit-configuration?
  (libnvidia-container
   nvidia-container-toolkit-configuration-libnvidia-container
   (default libnvidia-container))        ;file-like
  (toolkit-base
   nvidia-container-toolkit-configuration-toolkit-base
   (default nvidia-container-toolkit-base)) ;file-like
  (toolkit
   nvidia-container-toolkit-configuration-toolkit
   (default nvidia-container-toolkit))   ;file-like
  (glibc
   nvidia-container-toolkit-configuration-glibc
   (default glibc))                      ;file-like
  (docker-runtime-name
   nvidia-container-toolkit-configuration-docker-runtime-name
   (default "nvidia"))
  (docker-set-as-default?
   nvidia-container-toolkit-configuration-docker-set-as-default?
   (default #f))
  (runtime-mode
   nvidia-container-toolkit-configuration-runtime-mode
   (default "legacy")))

(define (nvidia-container-toolkit-profile config)
  (list (nvidia-container-toolkit-configuration-libnvidia-container config)
        (nvidia-container-toolkit-configuration-toolkit-base config)
        (nvidia-container-toolkit-configuration-toolkit config)))

;; Provide the FHS paths that Docker and NVIDIA's helper tools still assume.
(define (nvidia-container-toolkit-special-files _)
  '(("/usr/bin/nvidia-container-cli"
     "/run/current-system/profile/bin/nvidia-container-cli")
    ("/usr/bin/nvidia-container-runtime"
     "/run/current-system/profile/bin/nvidia-container-runtime")
    ("/usr/bin/nvidia-container-runtime-hook"
     "/run/current-system/profile/bin/nvidia-container-runtime-hook")
    ("/usr/bin/nvidia-container-toolkit"
     "/run/current-system/profile/bin/nvidia-container-toolkit")
    ("/usr/bin/nvidia-ctk"
     "/run/current-system/profile/bin/nvidia-ctk")
    ("/usr/bin/nvidia-cdi-hook"
     "/run/current-system/profile/bin/nvidia-cdi-hook")))

(define (nvidia-container-toolkit-docker-config-file config)
  (let ((runtime-name
         (nvidia-container-toolkit-configuration-docker-runtime-name config))
        (set-as-default?
         (nvidia-container-toolkit-configuration-docker-set-as-default? config)))
    (mixed-text-file
     "daemon.json"
     "{\n"
     "  \"runtimes\": {\n"
     "    \"" runtime-name "\": {\n"
     "      \"path\": \"/usr/bin/nvidia-container-runtime\",\n"
     "      \"args\": []\n"
     "    }\n"
     "  }"
     (if set-as-default?
         (string-append ",\n  \"default-runtime\": \"" runtime-name "\"\n")
         "\n")
     "}\n")))

(define (nvidia-container-toolkit-runtime-config-file config)
  (let ((runtime-mode
         (nvidia-container-toolkit-configuration-runtime-mode config))
        (ldconfig
         (file-append (nvidia-container-toolkit-configuration-glibc config)
                      "/sbin/ldconfig")))
    (mixed-text-file
     "nvidia-container-runtime-config.toml"
     "[nvidia-container-cli]\n"
     "path = \"/usr/bin/nvidia-container-cli\"\n"
     "ldconfig = \"@" ldconfig "\"\n\n"
     "[nvidia-container-runtime]\n"
     "mode = \"" runtime-mode "\"\n"
     "runtimes = [\"/run/current-system/profile/bin/crun\", "
     "\"/run/current-system/profile/bin/runc\", \"crun\", \"runc\"]\n\n"
     "[nvidia-container-runtime-hook]\n"
     "path = \"/usr/bin/nvidia-container-runtime-hook\"\n\n"
     "[nvidia-ctk]\n"
     "path = \"/usr/bin/nvidia-ctk\"\n")))

(define (nvidia-container-toolkit-etc-files config)
  (list `("docker/daemon.json"
          ,(nvidia-container-toolkit-docker-config-file config))
        `("nvidia-container-runtime/config.toml"
          ,(nvidia-container-toolkit-runtime-config-file config))))

(define nvidia-container-toolkit-service-type
  (service-type
   (name 'nvidia-container-toolkit)
   (extensions
    (list (service-extension profile-service-type
                             nvidia-container-toolkit-profile)
          (service-extension special-files-service-type
                             nvidia-container-toolkit-special-files)
          (service-extension etc-service-type
                             nvidia-container-toolkit-etc-files)))
   (default-value (nvidia-container-toolkit-configuration))
   (description
    "Install and configure the NVIDIA container toolkit for Docker-based
GPU workloads.")))
