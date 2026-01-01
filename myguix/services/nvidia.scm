;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Ayan Das <bvits@riseup.net>

(define-module (myguix services nvidia)
  #:use-module (guix gexp)
  #:use-module (guix records)
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
  (non-admin-profiling? nvidia-configuration-non-admin-profiling?
                        (default #t)))

(define (nvidia-modprobe-configuration config)
  (if (nvidia-configuration-non-admin-profiling? config)
      (list `("modprobe.d/nvidia.conf"
              ,(plain-file
                "nvidia.conf"
                "options nvidia NVreg_RestrictProfilingToAdminUsers=0\n")))
      '()))

(define %nvidia-non-admin-profiling-udev-rules
  ;; On Guix, eudev loads kernel modules via libkmod, which does not consult
  ;; /etc/modprobe.d by default. As a result, NVIDIA can be auto-loaded without
  ;; NVreg_RestrictProfilingToAdminUsers=0, and Nsight Compute will fail with
  ;; ERR_NVGPUCTRPERM for non-root users.
  ;;
  ;; Work around this by preventing eudev's generic 80-drivers.rules from
  ;; auto-loading the module for NVIDIA display controllers, and instead load
  ;; it explicitly with the desired module parameter.
  (udev-rule
   "79-nvidia-non-admin-profiling.rules"
   (string-append
    "# Enable NVIDIA GPU performance counters for all users (Nsight/CUPTI).\n"
    "# This overrides the default security restriction (Rendered Insecure).\n"
    "ACTION==\"add\", SUBSYSTEM==\"pci\", "
    "ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", "
    "ENV{LINUX_MODULE_DIRECTORY}=\"/run/booted-system/kernel/lib/modules\", "
    "ENV{MODPROBE_OPTIONS}=\"-C /etc/modprobe.d\", "
    "ENV{MODALIAS}=\"\", "
    "RUN+=\"/run/booted-system/profile/bin/modprobe "
    "nvidia NVreg_RestrictProfilingToAdminUsers=0\"\n")))

(define (nvidia-udev-rules config)
  (let ((driver (nvidia-configuration-driver config)))
    (if (nvidia-configuration-non-admin-profiling? config)
        (list driver %nvidia-non-admin-profiling-udev-rules)
        (list driver))))

(define (nvidia-shepherd-service config)
  (let* ((nvidia-driver (nvidia-configuration-driver config))
         (nvidia-smi (file-append nvidia-driver "/bin/nvidia-smi")))
    (list (shepherd-service (documentation
                             "Prepare system environment for NVIDIA driver.")
                            (provision '(nvidia))
                            (requirement '(udev))
                            (modules '(((guix build utils)
                                        #:select (invoke/quiet))
                                       ((rnrs io ports)
                                        #:select (get-line))))
                            (start #~(lambda _
                                       (when (file-exists?
                                              "/proc/driver/nvidia")
                                         (let ((modprobe (call-with-input-file "/proc/sys/kernel/modprobe"
                                                           get-line)))
                                           (false-if-exception (begin
                                                                 (invoke/quiet
                                                                  modprobe
                                                                  "--"
                                                                  "nvidia_uvm")
                                                                 (invoke/quiet #$nvidia-smi)))))))
                            (stop #~(const #f))))))

(define nvidia-service-type
  (service-type (name 'nvidia)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   nvidia-shepherd-service)
                                  (service-extension profile-service-type
                                                     (compose list
                                                      nvidia-configuration-driver))
                                  (service-extension udev-service-type
                                                     nvidia-udev-rules)
                                  (service-extension firmware-service-type
                                                     (compose list
                                                      nvidia-configuration-firmware))
                                  (service-extension etc-service-type
                                                     nvidia-modprobe-configuration)
                                  (service-extension
                                   linux-loadable-module-service-type
                                   (compose list nvidia-configuration-module))
                                  ;; Start before other user processes, necessary for some display
                                  ;; managers.
                                  (service-extension user-processes-service-type
                                                     (const '(nvidia)))))
                (default-value (nvidia-configuration))
                (description "Prepare system environment for NVIDIA driver.")))
