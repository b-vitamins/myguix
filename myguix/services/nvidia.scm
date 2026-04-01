;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Ayan Das <bvits@riseup.net>

(define-module (myguix services nvidia)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu system privilege)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (myguix packages nvidia)
  #:export (nvidia-configuration nvidia-configuration?
                                 nvidia-configuration-record?
                                 nvidia-service-type))

(define-record-type* <nvidia-configuration> nvidia-configuration
                     make-nvidia-configuration
  nvidia-configuration?
  (driver nvidia-configuration-driver
          (default nvda)) ;file-like
  (firmware nvidia-configuration-firmware
            (default nvidia-firmware)) ;file-like
  (module nvidia-configuration-module
          (default nvidia-module)) ;file-like
  (modprobe nvidia-configuration-modprobe
            (default nvidia-modprobe)) ;file-like
  (settings nvidia-configuration-settings
            (default #f)) ;file-like or #f
  (non-admin-profiling? nvidia-configuration-non-admin-profiling?
                        (default #t)))

(define (nvidia-profile config)
  (let ((settings (nvidia-configuration-settings config)))
    (append (list (nvidia-configuration-driver config)
                  nvidia-prime)
            (if settings
                (list settings)
                '()))))

(define (nvidia-modprobe-configuration config)
  (if (nvidia-configuration-non-admin-profiling? config)
      (list `("modprobe.d/nvidia.conf"
              ,(plain-file
                "nvidia.conf"
                "options nvidia NVreg_RestrictProfilingToAdminUsers=0\n")))
      '()))

(define (nvidia-privileged-program config)
  (list (file-like->setuid-program
         (file-append (nvidia-configuration-modprobe config)
                      "/bin/nvidia-modprobe"))))

(define (nvidia-shepherd-service config)
  (let ((nvidia-driver (nvidia-configuration-driver config))
        (nvidia-modprobe (file-append (nvidia-configuration-modprobe config)
                                      "/bin/nvidia-modprobe")))
    (list
     (shepherd-service
      (documentation "Prepare system environment for NVIDIA driver.")
      (provision '(nvidia))
      (requirement '(udev))
      (modules '(((guix build utils) #:select (invoke/quiet))
                 ((rnrs io ports) #:select (get-line))))
      (start
       #~(lambda _
           (let ((modprobe
                  (call-with-input-file "/proc/sys/kernel/modprobe"
                    get-line))
                 (nvidia-smi
                  (string-append #$nvidia-driver "/bin/nvidia-smi")))
             ;; Make DRM/KMS and UVM available before display managers start,
             ;; and create device nodes in case the udev path did not run.
             (false-if-exception
              (invoke/quiet modprobe "--" "nvidia_drm"))
             (false-if-exception
              (invoke/quiet modprobe "--" "nvidia_uvm"))
             (false-if-exception
              (invoke/quiet #$nvidia-modprobe "-c0" "-u"))
             (false-if-exception
              (invoke/quiet nvidia-smi)))))
      (stop #~(const #f))))))

;; Create path hard-coded by some NVIDIA userspace components.
(define (nvidia-special-files _)
  '(("/usr/bin/nvidia-modprobe" "/run/privileged/bin/nvidia-modprobe")
    ("/usr/share/nvidia" "/run/booted-system/profile/share/nvidia")))

;; Adapted from:
;; <https://github.com/Frogging-Family/nvidia-all/blob/master/system/60-nvidia.rules>
(define (nvidia-udev-rule _)
  (list
   (udev-rule "90-nvidia.rules" "\
# Device nodes are created by nvidia-modprobe, which is called by the nvidia DDX.
# In case the DDX is not started, call nvidia-modprobe in udev rules to cover
# Wayland/EGLStream and compute setups without a started display.
ACTION==\"add|bind\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", \\
    DRIVER==\"nvidia\", TEST!=\"/dev/nvidia-uvm\", \\
    RUN+=\"/usr/bin/nvidia-modprobe\", \\
    RUN+=\"/usr/bin/nvidia-modprobe -c0 -u\"

# Allow non-root access to NVIDIA profiling capabilities devices (needed for Nsight Compute metrics).
KERNEL==\"nvidia-cap[0-9]*\", MODE=\"0666\"

# Enable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA Audio devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"auto\"

# Disable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA Audio devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"on\"
")))

(define nvidia-service-type
  (service-type (name 'nvidia)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   nvidia-shepherd-service)
                                  (service-extension profile-service-type
                                                     nvidia-profile)
                                  (service-extension
                                   privileged-program-service-type
                                   nvidia-privileged-program)
                                  (service-extension special-files-service-type
                                                     nvidia-special-files)
                                  (service-extension udev-service-type
                                                     nvidia-udev-rule)
                                  (service-extension firmware-service-type
                                                     (compose list
                                                      nvidia-configuration-firmware))
                                  (service-extension etc-service-type
                                                     nvidia-modprobe-configuration)
                                  (service-extension
                                   linux-loadable-module-service-type
                                   (compose list nvidia-configuration-module))
                                  ;; Start before display managers and other
                                  ;; user processes touch the graphics stack.
                                  (service-extension user-processes-service-type
                                                     (const '(nvidia)))))
                (default-value (nvidia-configuration))
                (description "Prepare system environment for NVIDIA driver.")))
