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
                                       (let ((modprobe
                                              (call-with-input-file
                                                  "/proc/sys/kernel/modprobe"
                                                get-line)))
                                         ;; Ensure the NVIDIA DRM/KMS module is
                                         ;; available before display managers
                                         ;; (e.g. GDM) start, so monitors
                                         ;; connected to the dGPU remain active.
                                         (false-if-exception
                                          (invoke/quiet modprobe "--"
                                                       "nvidia_drm"))

                                         ;; Load UVM for CUDA userspace; ignore
                                         ;; errors on systems without it.
                                         (false-if-exception
                                          (invoke/quiet modprobe "--"
                                                       "nvidia_uvm"))

                                         (false-if-exception
                                          (invoke/quiet #$nvidia-smi)))))
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
                                                     (compose list
                                                      nvidia-configuration-driver))
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
