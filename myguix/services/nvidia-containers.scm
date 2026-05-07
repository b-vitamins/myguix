;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Ayan Das <bvits@riseup.net>

(define-module (myguix services nvidia-containers)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
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
        (nvidia-container-toolkit-configuration-toolkit config)
        docker-cli
        containerd))

(define %nvidia-container-toolkit-docker-environment
  ;; Docker registers its --gpus NVIDIA device driver only when dockerd can
  ;; find nvidia-container-runtime-hook at daemon startup.
  '("PATH=/run/current-system/profile/bin"))

(define %nvidia-container-toolkit-ldcache-file
  "/etc/nvidia-container-runtime/ld.so.cache")

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
     "environment = [\"LD_LIBRARY_PATH=/run/current-system/profile/lib\"]\n"
     "ldcache = \"" %nvidia-container-toolkit-ldcache-file "\"\n"
     "ldconfig = \"@" ldconfig "\"\n\n"
     "[nvidia-container-runtime]\n"
     "mode = \"" runtime-mode "\"\n"
     "runtimes = [\"/run/current-system/profile/bin/crun\", "
     "\"/run/current-system/profile/bin/runc\", \"crun\", \"runc\"]\n\n"
     "[nvidia-container-runtime-hook]\n"
     "path = \"/usr/bin/nvidia-container-runtime-hook\"\n\n"
     "[nvidia-ctk]\n"
     "path = \"/usr/bin/nvidia-ctk\"\n")))

(define (nvidia-container-toolkit-activation config)
  (let ((docker-config
         (nvidia-container-toolkit-docker-config-file config))
        (runtime-config
         (nvidia-container-toolkit-runtime-config-file config))
        (ldconfig
         (file-append (nvidia-container-toolkit-configuration-glibc config)
                      "/sbin/ldconfig")))
    #~(begin
        (use-modules (guix build utils))

        (define (install-managed-file directory target source)
          (mkdir-p directory)
          (when (file-exists? target)
            (if (file-is-directory? target)
                (error "refusing to overwrite directory" target)
                (delete-file target)))
          (copy-file source target)
          (chmod target #o644))

        ;; Preserve existing directories such as /etc/docker, which may
        ;; contain Docker-managed state like key.json.
        (install-managed-file "/etc/docker"
                              "/etc/docker/daemon.json"
                              #$docker-config)
        (install-managed-file "/etc/nvidia-container-runtime"
                              "/etc/nvidia-container-runtime/config.toml"
                              #$runtime-config)
        (invoke #$ldconfig
                "-C" #$%nvidia-container-toolkit-ldcache-file
                "/run/current-system/profile/lib"
                "/run/current-system/profile/lib64")
        (chmod #$%nvidia-container-toolkit-ldcache-file #o644)

        (mkdir-p "/var/lib/containerd")
        (mkdir-p "/var/lib/docker"))))

(define (nvidia-container-toolkit-containerd-shepherd-service _)
  (shepherd-service
   (documentation "containerd daemon.")
   (provision '(containerd))
   (requirement '(user-processes))
   (start #~(make-forkexec-constructor
             (list (string-append #$containerd "/bin/containerd"))
             ;; For finding containerd-shim binary.
             #:environment-variables
             (list (string-append "PATH=" #$containerd "/bin"))
             #:pid-file "/run/containerd/containerd.pid"
             #:pid-file-timeout 300
             #:log-file "/var/log/containerd.log"))
   (stop #~(make-kill-destructor))))

(define (nvidia-container-toolkit-dockerd-shepherd-service _)
  (shepherd-service
   (documentation "Docker daemon with NVIDIA container runtime support.")
   (provision '(dockerd))
   (requirement '(user-processes
                  containerd
                  dbus-system
                  elogind
                  file-system-/sys/fs/cgroup
                  networking
                  udev))
   (start #~(make-forkexec-constructor
             (list (string-append #$docker "/bin/dockerd")
                   "-p" "/var/run/docker.pid"
                   "--userland-proxy=true"
                   (string-append "--userland-proxy-path="
                                  #$docker-libnetwork-cmd-proxy
                                  "/bin/proxy")
                   "--iptables"
                   "--containerd" "/run/containerd/containerd.sock")
             #:environment-variables
             (list #$@%nvidia-container-toolkit-docker-environment)
             #:pid-file "/var/run/docker.pid"
             #:log-file "/var/log/docker.log"))
   (stop #~(make-kill-destructor))))

(define (nvidia-container-toolkit-shepherd-services config)
  (list (nvidia-container-toolkit-containerd-shepherd-service config)
        (nvidia-container-toolkit-dockerd-shepherd-service config)))

(define %nvidia-container-toolkit-accounts
  (list (user-group (name "docker") (system? #t))))

(define nvidia-container-toolkit-service-type
  (service-type
   (name 'nvidia-container-toolkit)
   (extensions
    (list (service-extension profile-service-type
                             nvidia-container-toolkit-profile)
          (service-extension special-files-service-type
                             nvidia-container-toolkit-special-files)
          (service-extension activation-service-type
                             nvidia-container-toolkit-activation)
          (service-extension account-service-type
                             (const %nvidia-container-toolkit-accounts))
          (service-extension shepherd-root-service-type
                             nvidia-container-toolkit-shepherd-services)))
   (default-value (nvidia-container-toolkit-configuration))
   (description
    "Install Docker, containerd, and the NVIDIA container toolkit for
Docker-based GPU workloads.")))
