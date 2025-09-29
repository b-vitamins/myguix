;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2022, 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>

(define-module (myguix packages nvidia)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix build utils)
                #:hide (which delete))
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license-gnu:)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix utils)
  #:use-module (myguix packages linux)
  #:use-module (myguix packages python-pqrs)
  #:use-module (myguix packages machine-learning)
  #:use-module (myguix packages video)
  #:use-module (ice-9 match)
  #:export (replace-mesa))

(define-public %nvidia-environment-variable-regexps
  '("^__NV_" "^__GL_" ;NVIDIA OpenGL settings.
    "^__GLX_VENDOR_LIBRARY_NAME$" ;For GLVND.
    ;; NVIDIA PRIME Render Offload.
    "^__VK_LAYER_NV_optimus$"
    ;; NVIDIA NGX.
    "^__NGX_CONF_FILE$"
    "^PROTON_ENABLE_NGX_UPDATER$"
    ;; NVIDIA Smooth Motion.
    "^NVPRESENT_"
    ;; NVIDIA VDPAU settings.
    "^VDPAU_NVIDIA_"
    ;; GSYNC control for Vulkan direct-to-display applications.
    "^VKDirectGSYNC(Compatible)?Allowed$"))


;;;
;;; NVIDIA driver checkouts
;;;

(define nvidia-driver-snippet
  ;; Note: delay to cope with cyclic module imports at the top level.
  (delay #~(begin
             (use-modules (guix build utils)
                          (ice-9 ftw)
                          (srfi srfi-1))
             (set-path-environment-variable "PATH"
                                            '("bin")
                                            '#+(list bash-minimal
                                                     coreutils-minimal grep
                                                     tar zstd))
             (let* ((this-file (last (scandir (getcwd)))))
               (invoke "sh" this-file "--extract-only" "--target" "extractdir")
               (for-each delete-file
                         (find-files "extractdir"
                                     (string-join '( ;egl-gbm
                                                    

                                                    "libnvidia-egl-gbm\\.so\\."
                                                    ;; egl-wayland
                                                    "libnvidia-egl-wayland\\.so\\."
                                                    ;; libglvnd
                                                    "libEGL\\.so\\."
                                                    "libGL\\.so\\."
                                                    "libGLESv1_CM\\.so\\."
                                                    "libGLESv2\\.so\\."
                                                    "libGLX\\.so\\."
                                                    "libGLdispatch\\.so\\."
                                                    "libOpenGL\\.so\\."
                                                    ;; nvidia-settings
                                                    "libnvidia-gtk[23]\\.so\\."
                                                    ;; opencl-icd-loader
                                                    "libOpenCL\\.so\\.") "|")))
               (with-directory-excursion "extractdir"
                 (invoke "tar"
                         "cvfa"
                         (string-append this-file ".tar")
                         "--mtime=1"
                         "--owner=root:0"
                         "--group=root:0" ;determinism
                         "--sort=name"
                         ".")
                 (invoke "zstd"
                         (string-append this-file ".tar")))
               (rename-file (string-append "extractdir/" this-file ".tar.zst")
                            this-file)))))

(define (nvidia-source version hash)
  "Given VERSION of an NVIDIA driver installer, return an <origin> for
its unpacked checkout."
  (origin
    (method url-fetch)
    (uri (string-append "https://us.download.nvidia.com/XFree86/Linux-x86_64/"
          version "/NVIDIA-Linux-x86_64-" version ".run"))
    (file-name (string-append "NVIDIA-Linux-x86_64-" version))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet (force nvidia-driver-snippet))))


;;;
;;; NVIDIA drivers
;;;

(define %nvidia-script-create-device-nodes
  (program-file "create-device-nodes.scm"
                (with-imported-modules '((guix build utils))
                                       #~(begin
                                           (use-modules (ice-9 regex)
                                                        (rnrs io ports)
                                                        (srfi srfi-1)
                                                        (guix build utils))

                                           (define %nvidia-character-devices
                                             (call-with-input-file "/proc/devices"
                                               (lambda (port)
                                                 (filter-map (lambda (line)
                                                               (if (string-contains
                                                                    line
                                                                    "nvidia")
                                                                   (apply cons
                                                                    (reverse (string-tokenize
                                                                              line)))
                                                                   #f))
                                                             (string-split (get-string-all
                                                                            port)
                                                                           #\newline)))))

                                           (define %nvidia-driver-device-minors
                                             (let ((device-minor-regexp (make-regexp
                                                                         "^Device Minor: \t (.*)")))
                                               (append-map (lambda (file)
                                                             (call-with-input-file file
                                                               (lambda (port)
                                                                 (filter-map (lambda 
                                                                                     (line)
                                                                               (let 
                                                                                    (
                                                                                     (matched
                                                                                      (regexp-exec
                                                                                       device-minor-regexp
                                                                                       line)))
                                                                                 
                                                                                 
                                                                                 (if
                                                                                  matched

                                                                                  
                                                                                  (match:substring
                                                                                   matched
                                                                                   1)
                                                                                  #f)))
                                                                             (string-split
                                                                              (get-string-all
                                                                               port)
                                                                              #\newline)))))
                                                           (find-files
                                                            "/proc/driver/nvidia/gpus/"
                                                            "information$"))))

                                           (define (create-device-node path
                                                                       name
                                                                       minor)
                                             (let ((major (or (assoc-ref
                                                               %nvidia-character-devices
                                                               name)
                                                              (assoc-ref
                                                               %nvidia-character-devices
                                                               "nvidia-frontend")))
                                                   (mknod #$(file-append
                                                             coreutils
                                                             "/bin/mknod")))
                                               (system* mknod
                                                        "-Zm0666"
                                                        path
                                                        "c"
                                                        major
                                                        minor)))

                                           (define (main args)
                                             (case (string->symbol (first args))
                                               ((nvidia_modeset)
                                                (create-device-node
                                                 "/dev/nvidia-modeset"
                                                 "nvidia-modeset" "254"))
                                               ((nvidia_uvm)
                                                (begin
                                                  (create-device-node
                                                   "/dev/nvidia-uvm"
                                                   "nvidia-uvm" "0")
                                                  (create-device-node
                                                   "/dev/nvidia-uvm-tools"
                                                   "nvidia-uvm" "1")))
                                               ((nvidia)
                                                (begin
                                                  (create-device-node
                                                   "/dev/nvidiactl"
                                                   "nvidiactl" "255")
                                                  (for-each (lambda (minor)
                                                              (create-device-node
                                                               (string-append
                                                                "/dev/nvidia"
                                                                minor)
                                                               "nvidia" minor))
                                                   %nvidia-driver-device-minors)))))

                                           (main (cdr (command-line)))))))

;; Adapted from <https://github.com/Frogging-Family/nvidia-all/blob/master/60-nvidia.rules>
(define %nvidia-udev-rules
  (mixed-text-file "90-nvidia.rules"
   "# Make sure device nodes are present even when the DDX is not started for the Wayland/EGLStream case
KERNEL==\"nvidia\", RUN+=\""
   %nvidia-script-create-device-nodes
   " nvidia\"
KERNEL==\"nvidia_modeset\", RUN+=\""
   %nvidia-script-create-device-nodes
   " nvidia_modeset\"
KERNEL==\"nvidia_uvm\", RUN+=\""
   %nvidia-script-create-device-nodes
   " nvidia_uvm\"

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
"))

(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version "580.82.09")
    (source
     (nvidia-source version
                    "1dwmardvxb2w6mx7hich5wc06f50qz92jk63kbhf059fv8rgiv1y"))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (srfi srfi-26))
      #:install-plan
      #~`((#$(match (or (%current-target-system)
                        (%current-system))
               ("i686-linux" "32")
               ("x86_64-linux" ".")
               (_ "."))
           "lib/"
           #:include-regexp ("^./[^/]+\\.so"))
          ("." "lib/nvidia/wine/"
           #:include-regexp ("_?nvngx.*?\\.dll$"))
          ("." "share/nvidia/"
           #:include-regexp ("nvidia-application-profiles|nvoptix.bin"))
          ("." "share/egl/egl_external_platform.d/"
           #:include-regexp ("(gbm|wayland|xcb|xlib)\\.json"))
          ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
          ("90-nvidia.rules" "lib/udev/rules.d/")
          ("nvidia-drm-outputclass.conf" "share/X11/xorg.conf.d/")
          ("nvidia-dbus.conf" "share/dbus-1/system.d/")
          ("nvidia.icd" "etc/OpenCL/vendors/")
          ("nvidia_icd.json" "share/vulkan/icd.d/")
          ("nvidia_icd_vksc.json" "etc/vulkansc/icd.d/")
          ("nvidia_layers.json" "share/vulkan/implicit_layer.d/")
          ("sandboxutils-filelist.json" "share/nvidia/files.d/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "xvf" source)))
          (delete 'strip)
          (add-after 'unpack 'create-misc-files
            (lambda* (#:key inputs #:allow-other-keys)
              ;; EGL external platform configuraiton
              (substitute* '("10_nvidia_wayland.json" "15_nvidia_gbm.json"
                             "20_nvidia_xcb.json" "20_nvidia_xlib.json")
                (("libnvidia-egl-(wayland|gbm|xcb|xlib)\\.so\\.." all)
                 (search-input-file inputs
                                    (string-append "lib/" all))))

              ;; EGL vendor ICD configuration
              (substitute* "10_nvidia.json"
                (("libEGL_nvidia\\.so\\.." all)
                 (string-append #$output "/lib/" all)))

              ;; OpenCL vendor ICD configuration
              (substitute* "nvidia.icd"
                (("libnvidia-opencl\\.so\\.." all)
                 (string-append #$output "/lib/" all)))

              ;; Vulkan ICD & layer configuraiton
              (substitute* '("nvidia_icd.json" "nvidia_layers.json")
                (("libGLX_nvidia\\.so\\.." all)
                 (string-append #$output "/lib/" all))
                (("libnvidia-present\\.so\\.[0-9.]*" all)
                 (string-append #$output "/lib/" all)))

              ;; VulkanSC ICD configuration
              (substitute* "nvidia_icd_vksc.json"
                (("libnvidia-vksc-core\\.so\\.." all)
                 (string-append #$output "/lib/" all)))

              ;; Add udev rules
              (symlink #$%nvidia-udev-rules "90-nvidia.rules")))
          (add-after 'install 'add-architecture-to-filename
            (lambda _
              (for-each (lambda (path)
                          (let* ((out #$output)
                                 (system #$(or (%current-target-system)
                                               (%current-system)))
                                 (dash (string-index system #\-))
                                 (arch (string-take system dash))

                                 (dot (string-index-right path #\.))
                                 (base (string-take path dot))
                                 (ext (string-drop path
                                                   (+ 1 dot))))
                            ;; <...>/nvidia.icd -> <...>/nvidia.x86_64.icd
                            ;; <...>/nvidia_icd.json -> <...>/nvidia_icd.x86_64.json
                            (rename-file (string-append out path)
                                         (string-append out
                                                        base
                                                        "."
                                                        arch
                                                        "."
                                                        ext))))
                        '("/etc/OpenCL/vendors/nvidia.icd"
                          "/share/egl/egl_external_platform.d/10_nvidia_wayland.json"
                          "/share/egl/egl_external_platform.d/15_nvidia_gbm.json"
                          "/share/egl/egl_external_platform.d/20_nvidia_xcb.json"
                          "/share/egl/egl_external_platform.d/20_nvidia_xlib.json"
                          "/share/glvnd/egl_vendor.d/10_nvidia.json"
                          "/share/vulkan/icd.d/nvidia_icd.json"
                          "/share/vulkan/implicit_layer.d/nvidia_layers.json"))))
          (add-after 'install 'patch-elf
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((ld.so (search-input-file inputs
                                               #$(glibc-dynamic-linker)))
                     (rpath (string-join (cons* (dirname ld.so)
                                                (string-append #$output "/lib")
                                                (map (lambda (name)
                                                       (dirname (search-input-file
                                                                 inputs
                                                                 (string-append
                                                                  "lib/" name))))
                                                     '("libX11.so.6"
                                                       "libXext.so.6"
                                                       "libcrypto.so.1.1"
                                                       "libcrypto.so.3"
                                                       "libdrm.so.2"
                                                       "libgbm.so.1"
                                                       "libgcc_s.so.1"
                                                       "libwayland-client.so.0"
                                                       "libxcb.so.1"))) ":")))
                (define (patch-elf file)
                  (format #t "Patching ~a ..." file)
                  (unless (string-contains file ".so")
                    (invoke "patchelf" "--set-interpreter" ld.so file))
                  (invoke "patchelf" "--set-rpath" rpath file)
                  (display " done\n"))

                (for-each (lambda (file)
                            (when (elf-file? file)
                              (patch-elf file)))
                          (find-files #$output)))))
          (add-before 'patch-elf 'install-commands
            (lambda _
              (when (string-match "x86_64-linux"
                                  (or #$(%current-target-system)
                                      #$(%current-system)))
                (for-each (lambda (binary)
                            (let ((bindir (string-append #$output "/bin"))
                                  (manual (string-append binary ".1.gz"))
                                  (mandir (string-append #$output
                                                         "/share/man/man1")))
                              (install-file binary bindir)
                              (when (file-exists? manual)
                                (install-file manual mandir))))
                          '("nvidia-cuda-mps-control" "nvidia-cuda-mps-server"
                            "nvidia-pcc" "nvidia-smi")))))
          (add-before 'patch-elf 'relocate-libraries
            (lambda _
              (let* ((version #$(package-version this-package))
                     (libdir (string-append #$output "/lib"))
                     (gbmdir (string-append libdir "/gbm"))
                     (vdpaudir (string-append libdir "/vdpau"))
                     (xorgmoddir (string-append libdir "/xorg/modules"))
                     (xorgdrvdir (string-append xorgmoddir "/drivers"))
                     (xorgextdir (string-append xorgmoddir "/extensions"))
                     (move-to-dir (lambda (file dir)
                                    (install-file file dir)
                                    (delete-file file))))
                (for-each (lambda (file)
                            (mkdir-p gbmdir)
                            (with-directory-excursion gbmdir
                              (symlink file "nvidia-drm_gbm.so")))
                          (find-files libdir "libnvidia-allocator\\.so\\."))

                (for-each (cut move-to-dir <> vdpaudir)
                          (find-files libdir "libvdpau_nvidia\\.so\\."))

                (for-each (cut move-to-dir <> xorgdrvdir)
                          (find-files libdir "nvidia_drv\\.so$"))

                (for-each (lambda (file)
                            (move-to-dir file xorgextdir)
                            (with-directory-excursion xorgextdir
                              (symlink (basename file)
                                       "libglxserver_nvidia.so")))
                          (find-files libdir "libglxserver_nvidia\\.so\\.")))))
          (add-after 'patch-elf 'create-short-name-symlinks
            (lambda _
              (define (get-soname file)
                (when (elf-file? file)
                  (let* ((cmd (string-append "patchelf --print-soname " file))
                         (port (open-input-pipe cmd))
                         (soname (read-line port)))
                    (close-pipe port) soname)))
              (for-each (lambda (lib)
                          (let ((lib-soname (get-soname lib)))
                            (when (string? lib-soname)
                              (let* ((soname (string-append (dirname lib) "/"
                                                            lib-soname))
                                     (base (string-append (regexp-substitute
                                                                             #f
                                                                             (string-match
                                                                              "(.*)\\.so.*"
                                                                              soname)
                                                                             1)
                                                          ".so"))
                                     (source (basename lib)))
                                (for-each (lambda (target)
                                            (unless (file-exists? target)
                                              (format #t
                                               "Symlinking ~a -> ~a..." target
                                               source)
                                              (symlink source target)
                                              (display " done\n")))
                                          (list soname base))))))
                        (find-files #$output "\\.so\\.")))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs (list patchelf-0.16))
    (inputs (list egl-gbm
                  egl-wayland
                  egl-x11
                  `(,gcc "lib")
                  glibc
                  mesa-for-nvda
                  openssl
                  openssl-1.1
                  wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (libraries)")
    (description
     "This package provides libraries of the proprietary NVIDIA driver.  It's
mainly used as a dependency of other packages.  For user-facing purpose, use
@code{nvda} instead.")
    (license (license:nonfree (format #f
                               "file:///share/doc/nvidia-driver-~a/LICENSE"
                               version)))))

(define-public nvidia-driver-beta
  (package
    (inherit nvidia-driver)
    (name "nvidia-driver-beta")
    (version "570.86.16")
    (source
     (nvidia-source version
                    "1mfbc59g5v1c6dqissg1mfawvaknqrr7r985214py92lnr5ylqs5"))))

(define-public nvidia-libs
  (deprecated-package "nvidia-libs" nvidia-driver))


;;;
;;; NVIDIA firmwares
;;;

(define-public nvidia-firmware
  (let ((base nvidia-driver))
    (package
      (inherit base)
      (name "nvidia-firmware")
      (arguments
       (list
        #:install-plan
        #~'(("firmware" #$(string-append "lib/firmware/nvidia/"
                                         (package-version this-package))))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'strip)
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "xvf" source))))))
      (propagated-inputs '())
      (inputs '())
      (native-inputs '())
      (synopsis "Proprietary NVIDIA driver (GSP firmwares)")
      (description
       "This package provides firmwares for NVIDIA's GPU System Processor.
Firmware installation can be done with @code{nvidia-service-type}, however
whether GSP mode is enabled by default or not depends on the specific GPU
product.

To enable GSP mode manually, add @code{\"NVreg_EnableGpuFirmware=1\"} to
@code{kernel-arguments} field of the @code{operating-system} configuration."))))

(define-public nvidia-firmware-beta
  (package
    (inherit nvidia-firmware)
    (name "nvidia-firmware-beta")
    (version (package-version nvidia-driver-beta))
    (source
     (package-source nvidia-driver-beta))))


;;;
;;; NVIDIA kernel modules
;;;

(define-public nvidia-module
  (package
    (name "nvidia-module")
    (version (package-version nvidia-driver))
    (source
     (package-source nvidia-driver))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:linux linux
      #:source-directory "kernel"
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC="
                             #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "xvf" source)))
          (delete 'strip)
          (add-before 'configure 'fixpath
            (lambda* (#:key (source-directory ".") #:allow-other-keys)
              (substitute* (string-append source-directory "/Kbuild")
                (("/bin/sh")
                 (which "sh")))))
          (replace 'build
            (lambda* (#:key (make-flags '())
                      (parallel-build? #t)
                      (source-directory ".") inputs #:allow-other-keys)
              (apply invoke
                     "make"
                     "-C"
                     (canonicalize-path source-directory)
                     (string-append "SYSSRC="
                                    (search-input-directory inputs
                                     "/lib/modules/build"))
                     `(,@(if parallel-build?
                             `("-j" ,(number->string (parallel-job-count)))
                             '()) ,@make-flags)))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (kernel modules)")
    (description
     "This package provides kernel modules of the proprietary NVIDIA driver.
Module setup can be done with @code{nvidia-service-type}, to actually use these
modules, also add @code{modprobe.blacklist=nouveau} to @code{kernel-arguments}
field of the @code{operating-system} configuration.

If the NVIDIA card is not used for displaying, or on a Wayland environment,
add @code{nvidia_drm.modeset=1} to @code{kernel-arguments} as well.")
    (license (license:nonfree (format #f
                               "file:///share/doc/nvidia-driver-~a/LICENSE"
                               version)))))

(define-public nvidia-module-beta
  (package
    (inherit nvidia-module)
    (name "nvidia-module-beta")
    (version (package-version nvidia-driver-beta))
    (source
     (package-source nvidia-driver-beta))))

(define-public nvidia-module-open
  (let ((base nvidia-module))
    (package
      (inherit base)
      (name "nvidia-module-open")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; NOTE: Kernels compiled with CONFIG_LTO_CLANG_THIN would cause an
         ;; error here.  See also:
         ;; <https://github.com/NVIDIA/open-gpu-kernel-modules/issues/214>
         ;; <https://github.com/llvm/llvm-project/issues/55820>
         ((#:source-directory _)
          "kernel-open")))
      (home-page "https://github.com/NVIDIA/open-gpu-kernel-modules")
      (synopsis "Open source NVIDIA kernel modules")
      (description
       "This package provides open source NVIDIA kernel modules, however
proprietary firmware and libraries are still necessary, and these modules
require GPU System Processor to be present (Turing or later architectures) and
enabled (see also the description of @code{nvidia-firmware} package).

Module setup can be done with @code{nvidia-service-type} (with @code{module}
field of @code{nvidia-configuration} set to @code{nvidia-module-open}), to
actually use these modules, also add @code{modprobe.blacklist=nouveau} to
@code{kernel-arguments} field of the @code{operating-system} configuration.

If the NVIDIA card is not used for displaying, or on a Wayland environment,
add @code{nvidia_drm.modeset=1} to @code{kernel-arguments} as well.")
      (license license-gnu:gpl2))))

(define-public nvidia-module-open-beta
  (package
    (inherit nvidia-module-open)
    (name "nvidia-module-open-beta")
    (version (package-version nvidia-driver-beta))
    (source
     (package-source nvidia-driver-beta))))


;;;
;;; ‘nvidia-settings’ packages
;;;

(define (nvidia-settings-source name version hash)
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/NVIDIA/nvidia-settings")
                        (commit version)))
    (file-name (git-file-name name version))
    (modules '((guix build utils)))
    (snippet '(delete-file-recursively "src/jansson"))
    (sha256 (base32 hash))))

(define-public nvidia-settings
  (package
    (name "nvidia-settings")
    (version "580.82.09")
    (source
     (nvidia-settings-source name version
      "0sy3mrg3vmyba6m87nanzmpvv2hzhb6nqdckhlaxv8wppmr7fvms"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no test suite
      #:make-flags
      #~(list "NV_USE_BUNDLED_LIBJANSSON=0"
              (string-append "PREFIX="
                             #$output)
              (string-append "CC="
                             #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'fix-application-profile-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/gtk+-2.x/ctkappprofile.c"
                (("/usr")
                 "/run/booted-system/profile"))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (substitute* "doc/nvidia-settings.desktop"
                (("^Exec=.*")
                 "Exec=nvidia-settings\n")
                (("__NVIDIA_SETTINGS_DESKTOP_CATEGORIES__")
                 "Settings"))
              (install-file "doc/nvidia-settings.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "doc/nvidia-settings.png"
                            (string-append #$output
                             "/share/icons/hicolor/128x128/apps"))))
          (add-after 'install 'wrap-program
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/bin/nvidia-settings")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append out "/lib/"))))))))))
    (native-inputs (list m4 pkg-config))
    (inputs (list bash-minimal
                  dbus
                  glu
                  gtk+
                  gtk+-2
                  jansson
                  libvdpau
                  libx11
                  libxext
                  libxrandr
                  libxv
                  libxxf86vm
                  vulkan-headers))
    (synopsis "Nvidia driver control panel")
    (description
     "This package provides Nvidia driver control panel for monitor
configuration, creating application profiles, gpu monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license-gnu:gpl2)))

(define-public nvidia-settings-beta
  (package
    (inherit nvidia-settings)
    (name "nvidia-settings-beta")
    (version "565.57.01")
    (source
     (nvidia-settings-source name version
      "006my5a69689wkzjcg3k1y35ifmizfyfj4n7f02naxhbgrxq9fqz"))))


;;;
;;; ‘nvda’ packages
;;;

(define-public libglvnd-for-nvda
  (hidden-package (package
                    (inherit libglvnd)
                    (arguments
                     (substitute-keyword-arguments (package-arguments libglvnd)
                       ((#:configure-flags flags
                         #~'())
                        #~(cons* "-Dc_link_args=-Wl,-rpath=$ORIGIN"
                                 #$flags))
                       ((#:phases phases
                         #~%standard-phases)
                        #~(modify-phases #$phases
                            (delete 'shrink-runpath))))))))

(define-public mesa-for-nvda
  (hidden-package (package
                    (inherit mesa)
                    (propagated-inputs (modify-inputs (package-propagated-inputs
                                                       mesa)
                                         (prepend libglvnd-for-nvda)))
                    (arguments
                     (substitute-keyword-arguments (package-arguments mesa)
                       ((#:configure-flags flags
                         #~'())
                        #~(cons* "-Dglvnd=true"
                                 #$flags))
                       ((#:phases phases
                         #~%standard-phases)
                        #~(modify-phases #$phases
                            (add-after 'install 'fix-egl-vendor-icd
                              (lambda _
                                (substitute* (string-append #$output
                                              "/share/glvnd/egl_vendor.d/50_mesa.json")
                                  (("libEGL_mesa\\.so\\.." all)
                                   (string-append #$output "/lib/" all)))))
                            (add-after 'set-layer-path-in-manifests 'add-architecture-to-filename
                              (lambda _
                                (for-each (lambda (path)
                                            (let* ((out #$output)
                                                   (system #$(or (%current-target-system)
                                                                 (%current-system)))
                                                   (dash (string-index system
                                                                       #\-))
                                                   (arch (string-take system
                                                                      dash))

                                                   (dot (string-index-right
                                                         path #\.))
                                                   (base (string-take path dot))
                                                   (ext (string-drop path
                                                                     (+ 1 dot))))
                                              ;; <...>/50_mesa.json -> <...>/50_mesa.x86_64.json
                                              (rename-file (string-append out
                                                            path)
                                                           (string-append out
                                                            base
                                                            "."
                                                            arch
                                                            "."
                                                            ext))))
                                          '("/share/glvnd/egl_vendor.d/50_mesa.json"
                                            "/share/vulkan/explicit_layer.d/VkLayer_MESA_overlay.json"
                                            "/share/vulkan/implicit_layer.d/VkLayer_MESA_device_select.json")))))))))))

;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define-public nvda
  (package
    (name "nvda")
    (version (string-pad-right (package-version nvidia-driver)
                               (string-length (package-version mesa-for-nvda))
                               #\0))
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build union))
      #:builder
      #~(begin
          (use-modules (guix build union))
          (union-build #$output
                       '#$(list (this-package-input "libglvnd")
                                (this-package-input "mesa")
                                (this-package-input "nvidia-driver")
                                (this-package-input "nvidia-vaapi-driver"))))))
    (native-search-paths
     (list
      ;; https://github.com/NVIDIA/egl-wayland/issues/39
      (search-path-specification
       (variable "__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS")
       (files '("share/egl/egl_external_platform.d")))
      ;; https://gitlab.freedesktop.org/glvnd/libglvnd/-/blob/master/src/EGL/icd_enumeration.md
      (search-path-specification
       (variable "__EGL_VENDOR_LIBRARY_DIRS")
       (files '("share/glvnd/egl_vendor.d")))
      ;; See also: ‘src/gbm/main/backend.c’ in mesa source.
      (search-path-specification
       (variable "GBM_BACKENDS_PATH")
       (files '("lib/gbm")))
      ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
      ;; this to all VA-API back ends instead of once to libva.
      (search-path-specification
       (variable "LIBVA_DRIVERS_PATH")
       (files '("lib/dri")))
      (search-path-specification
       (variable "VDPAU_DRIVER_PATH")
       (files '("lib/vdpau"))
       (separator #f))
      ;; https://github.com/KhronosGroup/Vulkan-Loader/blob/main/docs/LoaderLayerInterface.md
      (search-path-specification
       (variable "XDG_DATA_DIRS")
       (files '("share")))))
    (synopsis "Nonguix's user-facing NVIDIA driver package")
    (description
     "This package provides a drop-in replacement for @code{mesa} and is
intended to be installed by @code{nvidia-service-type}.

To actually use the NVIDIA card, replacement must be applied for individual
packages, this can be done either by rewriting inputs with
@code{--with-input=mesa=nvda} or grafting with @code{--with-graft=mesa=nvda}.
For a programmatical way, the procedure @code{replace-mesa} can be used.

Additionally, if the NVIDIA card is not used for displaying, environment
variables @code{__GLX_VENDOR_LIBRARY_NAME=nvidia} and
@code{__NV_PRIME_RENDER_OFFLOAD=1} may be set.")
    (native-inputs '())
    (propagated-inputs (append (package-propagated-inputs mesa-for-nvda)
                               (package-propagated-inputs nvidia-driver)))
    (inputs (list mesa-for-nvda nvidia-driver nvidia-vaapi-driver))
    (outputs '("out"))
    (license (package-license nvidia-driver))
    (home-page (package-home-page nvidia-driver))))

(define-public nvdb
  (package
    (inherit nvda)
    (name "nvdb")
    (version (string-pad-right (package-version nvidia-driver-beta)
                               (string-length (package-version mesa-for-nvda))
                               #\0))
    (arguments
     (list
      #:modules '((guix build union))
      #:builder
      #~(begin
          (use-modules (guix build union))
          (union-build #$output
                       '#$(list (this-package-input "libglvnd")
                                (this-package-input "mesa")
                                (this-package-input "nvidia-driver-beta"))))))
    (propagated-inputs (append (package-propagated-inputs mesa-for-nvda)
                               (package-propagated-inputs nvidia-driver-beta)))
    (inputs (list mesa-for-nvda nvidia-driver-beta))))

(define replace-mesa
  (package-input-grafting `((,mesa unquote nvda))))


;;;
;;; Other packages
;;;

(define-public egl-gbm
  (package
    (name "egl-gbm")
    (version "1.1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/egl-gbm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zcr1jksnh0431marzvgg301aybli29r1xw5vs4wnxgcp9bigvn6"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "GBM EGL external platform library")
    (description
     "This package provides an EGL External Platform library implementation for
GBM EGL support.")
    (home-page "https://github.com/NVIDIA/egl-gbm")
    (license license-gnu:expat)))

(define-public egl-x11
  (package
    (name "egl-x11")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/egl-x11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hh1wkdijjhsmym5ab5nw8wyi0w9x7aznnmyg8sczhwdfb5rdnrj"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "X11 and XCB EGL external platform library")
    (description
     "This package provides an EGL platform library for the NVIDIA driver to
support XWayland via xlib (using @code{EGL_KHR_platform_x11}) or xcb (using
@code{EGL_EXT_platform_xcb}).")
    (home-page "https://github.com/NVIDIA/egl-x11")
    (license license-gnu:expat)))

(define-public nvidia-htop
  (package
    (name "nvidia-htop")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nvidia-htop" version))
       (sha256
        (base32 "1a70gwapcmkpln279rmhb2y2f7j7y8z9ax3y1yx8bjaahh75svhh"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-libnvidia
            (lambda _
              (substitute* "nvidia-htop.py"
                (("nvidia-smi")
                 (string-append #$(this-package-input "nvidia-driver")
                                "/bin/nvidia-smi"))))))))
    (inputs (list nvidia-driver))
    (propagated-inputs (list python-termcolor))
    (home-page "https://github.com/peci1/nvidia-htop")
    (synopsis "Tool to enrich the output of nvidia-smi")
    (description "This package provides tool for enriching the output of
nvidia-smi.")
    (license license-gnu:bsd-3)))

(define-public nvidia-system-monitor
  (package
    (name "nvidia-system-monitor")
    (version "1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/congard/nvidia-system-monitor-qt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wgcrbpp30pi30pb0l2rvw3q16xzflj2bl30gj2gigda00r3py0s"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-nvidia-smi
            (lambda _
              (let ((nvidia-smi (string-append #$(this-package-input
                                                  "nvidia-driver")
                                               "/bin/nvidia-smi")))
                (substitute* "src/core/InfoProvider.cpp"
                  (("nvidia-smi")
                   nvidia-smi))
                (substitute* "src/main.cpp"
                  (("which nvidia-smi")
                   (string-append "which " nvidia-smi))
                  (("exec..nvidia-smi")
                   (string-append "exec(\"" nvidia-smi))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "qnvsm" bin)))))))
    (inputs (list qtbase-5 qtdeclarative-5 nvidia-driver))
    (home-page "https://github.com/congard/nvidia-system-monitor-qt")
    (synopsis "Task manager for Nvidia graphics cards")
    (description
     "This package provides a task manager for Nvidia graphics cards.")
    (license license-gnu:expat)))

(define-public python-nvidia-ml-py
  (package
    (name "python-nvidia-ml-py")
    (version "13.580.65")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nvidia_ml_py" version))
       (sha256
        (base32 "1jd3yqa6542a1ad3arcy8g73cnqmqb3g1x8w04kqfrfkqw1qpwbv"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-libnvidia
            (lambda _
              (substitute* "pynvml.py"
                (("libnvidia-ml.so.1")
                 (string-append #$(this-package-input "nvidia-driver")
                                "/lib/libnvidia-ml.so.1"))))))))
    (inputs (list nvidia-driver))
    (home-page "https://forums.developer.nvidia.com")
    (synopsis "Python Bindings for the NVIDIA Management Library")
    (description
     "This package provides official Python Bindings for the NVIDIA
Management Library")
    (license license-gnu:bsd-3)))

(define-public python-py3nvml
  (package
    (name "python-py3nvml")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py3nvml" version))
       (sha256
        (base32 "0wxxky9amy38q7qjsdmmznk1kqdzwd680ps64i76cvlab421vvh9"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-libnvidia
            (lambda _
              (substitute* "py3nvml/py3nvml.py"
                (("libnvidia-ml.so.1")
                 (string-append #$(this-package-input "nvidia-driver")
                                "/lib/libnvidia-ml.so.1"))))))))
    (propagated-inputs (list nvidia-driver python-xmltodict))
    (home-page "https://github.com/fbcotter/py3nvml")
    (synopsis "Unoffcial Python 3 Bindings for the NVIDIA Management Library")
    (description "This package provides unofficial Python 3 Bindings for the
NVIDIA Management Library")
    (license license-gnu:bsd-3)))

(define-public cuda-toolkit
  (package
    (name "cuda-toolkit")
    (version "12.9.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://developer.download.nvidia.com/compute/cuda/12.9.0/local_installers/cuda_12.9.0_575.51.03_linux.run")
       (sha256
        (base32 "0hzf0yg0m6sfrig7bcnpmlciaz1ppw1rwwkgr2hnq2g21xv2pkmv"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (outputs '("out"))
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (ice-9 ftw))
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (define ld.so
                (search-input-file inputs
                                   #$(glibc-dynamic-linker)))
              (let ((source (assoc-ref inputs "source")))
                (invoke "sh" source "--keep" "--noexec")
                (chdir "pkg"))))
          (delete 'configure)
          (delete 'check)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (define libc
                (assoc-ref inputs "libc"))
              (define gcc-lib
                (assoc-ref inputs "gcc:lib"))
              (define ld.so
                (search-input-file inputs
                                   #$(glibc-dynamic-linker)))
              (define rpath
                (string-join (list "$ORIGIN"
                                   (string-append #$output "/lib")
                                   (string-append #$output "/nvvm/lib64")
                                   (string-append libc "/lib")
                                   (string-append gcc-lib "/lib")) ":"))
              (define (patch-elf file)
                (make-file-writable file)
                (format #t "Setting RPATH on '~a'...~%" file)
                ;; RPATH should be modified before the interpreter. If
                ;; done the other way around, it nukes the resulting
                ;; binary.
                (invoke "patchelf" "--set-rpath" rpath "--force-rpath" file)
                (unless (string-contains file ".so")
                  (format #t "Setting interpreter on '~a'...~%" file)
                  (invoke "patchelf" "--set-interpreter" ld.so file)))
              (for-each (lambda (file)
                          (when (elf-file? file)
                            (patch-elf file)))
                        (find-files "."
                                    (lambda (file stat)
                                      (eq? 'regular
                                           (stat:type stat)))))))
          (replace 'install
            (lambda _
              (define (copy-from-directory directory)
                (for-each (lambda (entry)
                            (define sub-directory
                              (string-append directory "/" entry))

                            (define target
                              (string-append #$output "/"
                                             (basename entry)))

                            (when (file-exists? sub-directory)
                              (copy-recursively sub-directory target)))
                          '("bin" "targets/x86_64-linux/lib"
                            "targets/x86_64-linux/include" "nvvm/bin"
                            "nvvm/include" "nvvm/lib64")))

              (setenv "COLUMNS" "200")
              (with-directory-excursion "builds"
                (for-each copy-from-directory
                          (scandir "."
                                   (match-lambda
                                     ((or "." "..")
                                      #f)
                                     (_ #t))))
                (copy-recursively "cuda_nvcc/nvvm"
                                  (string-append #$output "/nvvm")))
              (symlink (string-append #$output "/lib/stubs")
                       (string-append #$output "/lib64/stubs"))))
          (add-after 'install 'symlink-libcuda
            (lambda _
              (with-directory-excursion (string-append #$output "/lib/stubs")
                (symlink "libcuda.so" "libcuda.so.1"))))

          (add-after 'install 'install-cupti
            (lambda _
              (copy-recursively "builds/cuda_cupti/extras/CUPTI"
                                #$output)))
          (add-after 'install 'delete-stray-symlinks
            (lambda _
              (delete-file (string-append #$output "/include/include")))))))
    (native-inputs (list which patchelf-0.16 perl python-2))
    (inputs `(("gcc:lib" ,gcc "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (license (license:nonfree
              "https://developer.nvidia.com/nvidia-cuda-license"))))

(define-public cuda-toolkit-11.8
  (package
    (inherit cuda-toolkit)
    (version "11.8.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://developer.download.nvidia.com/compute/cuda/11.8.0/local_installers/cuda_11.8.0_520.61.05_linux.run")
       (sha256
        (base32 "05jskb06lw0v1m52pg2zhm5v78837cb9pgcsxnxsgr7b7apw88wj"))))))

(define-public cudnn
  (package
    (name "cudnn")
    (version "9.12.0.46")
    (source
     (origin
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "17mi138jny2msqr28x81a6p369j9pn120jqqd2403nx3m2rx3yi1"))
       (method url-fetch)))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (define libc
                (assoc-ref inputs "libc"))
              (define gcc-lib
                (assoc-ref inputs "gcc:lib"))
              (define ld.so
                (search-input-file inputs
                                   #$(glibc-dynamic-linker)))
              (define rpath
                (string-join (list "$ORIGIN"
                                   (string-append #$output "/lib")
                                   (string-append #$output "/nvvm/lib64")
                                   (string-append libc "/lib")
                                   (string-append gcc-lib "/lib")) ":"))

              (define (patch-elf file)
                (make-file-writable file)
                (unless (string-contains file ".so")
                  (format #t "Setting interpreter on '~a'...~%" file)
                  (invoke "patchelf" "--set-interpreter" ld.so file))
                (format #t "Setting RPATH on '~a'...~%" file)
                (invoke "patchelf" "--set-rpath" rpath "--force-rpath" file))

              (for-each (lambda (file)
                          (when (elf-file? file)
                            (patch-elf file)))
                        (find-files "."
                                    (lambda (file stat)
                                      (eq? 'regular
                                           (stat:type stat)))))))
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (include (string-append #$output "/include")))
                (mkdir-p #$output)
                (copy-recursively "lib" lib)
                (copy-recursively "include" include)))))))
    (native-inputs (list patchelf-0.16))
    (inputs `(("gcc:lib" ,gcc "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "NVIDIA CUDA Deep Neural Network library (cuDNN)")
    (description "This package provides the CUDA Deep Neural Network library.")
    (license (license:nonfree
              "https://docs.nvidia.com/deeplearning/cudnn/sla/index.html"))))

(define-public cudnn-8.9
  (package
    (inherit cudnn)
    (version "8.9.1.23")
    (source
     (origin
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
             version "_cuda11-archive.tar.xz"))
       (sha256
        (base32 "0p286gnjslz06z9vff136pq8srkax75nbklmvg4r11g2cxr8ind6"))
       (method url-fetch)))))

(define-public cudnn-8.6
  (package
    (inherit cudnn)
    (version "8.6.0.163")
    (source
     (origin
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
             version "_cuda11-archive.tar.xz"))
       (sha256
        (base32 "0p286gnjslz06z9vff136pq8srkax75nbklmvg4r11g2cxr8ind6"))
       (method url-fetch)))))

(define-public cudnn-frontend
  (package
    (name "cudnn-frontend")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cudnn-frontend")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mzm2w9616pbypk9qp4nsr8baxq4nxzqzfq3x5mlqbimp01jfk9x"))
       (modules '((guix build utils)))
       (snippet #~(begin
                    (delete-file-recursively
                     "include/cudnn_frontend/thirdparty")
                    (substitute* (find-files "include" "\\.(cpp|h|hpp)")
                      (("\"cudnn_frontend/thirdparty/nlohmann/json\\.hpp\"")
                       "<nlohmann/json.hpp>"))))
       (patches (parameterize ((%patch-path (map (lambda (directory)
                                                   (string-append directory
                                                    "/myguix/patches"))
                                                 %load-path)))
                  (search-patches
                   "nvidia-cudnn-frontend_find_built_dlpack.patch"
                   "nvidia-cudnn-frontend_use_store_so.patch")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; No tests defined
      #:modules '((guix build pyproject-build-system)
                  (guix build union)
                  (guix build utils))
      #:imported-modules `(,@%pyproject-build-system-modules (guix build union))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set_cuda_paths
            (lambda _
              (substitute* "python/cudnn/__init__.py"
                (("@store-cudnn\\.so-path@")
                 (format #f "\"~a/lib/libcudnn.so\""
                         #$(this-package-input "cudnn"))))
              (setenv "CUDA_PATH"
                      #$(this-package-input "cuda-toolkit"))
              (setenv "CUDNN_PATH"
                      #$(this-package-input "cudnn"))
              (setenv "CUDNN_FRONTEND_FETCH_PYBINDS_IN_CMAKE" "0")
              (setenv "CMAKE_BUILD_PARALLEL_LEVEL"
                      (number->string (parallel-job-count)))))
          (add-after 'install 'post-install
            (lambda _
              (union-build (string-append #$output "/include")
                           (find-files (string-append #$output "/lib")
                                       (lambda (file stat)
                                         (string-suffix? "include" file))
                                       #:directories? #t)))))))
    (native-inputs (list cmake dlpack pybind11 python-setuptools python-wheel))
    (inputs (list cuda-toolkit nlohmann-json cudnn))
    (home-page "https://github.com/NVIDIA/cudnn-frontend")
    (synopsis "cuDNN API header-only library")
    (description
     "This package provides a C++ header-only library that wraps
the NVIDIA CUDA Deep Neural Network library (cuDNN) C backend API.  This entry
point to the same API is less verbose (without loss of control), and adds
functionality on top of the backend API, such as errata filters and
autotuning.")
    (license license-gnu:expat)))

(define-public cutlass
  (package
    (name "cutlass")
    (version "4.1.0")
    (home-page "https://github.com/NVIDIA/cutlass")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxss2jxbgjija8c7cryjq75irrnj17f5yjfppmaf2y21x7bm3v5"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DCUTLASS_ENABLE_EXAMPLES=NO"
              "-DCUTLASS_ENABLE_TOOLS=YES"
              "-DCUTLASS_ENABLE_LIBRARY=YES"
              "-DCUTLASS_ENABLE_PROFILER=YES"
              "-DCUTLASS_ENABLE_PERFORMANCE=YES"
              "-DCUTLASS_ENABLE_CUDNN=YES"
              "-DCUTLASS_ENABLE_CUBLAS=YES"
              "-DCUTLASS_ENABLE_F16C=YES"
              "-DCUTLASS_ENABLE_TESTS=NO"
              "-DCUTLASS_INSTALL_TESTS=NO"
              "-DCUTLASS_NVCC_ARCHS=80;86;89"
              "-DCUTLASS_LIBRARY_KERNELS=all")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cuda-build
            (lambda _
              (substitute* "CMakeLists.txt"
                (("--user")
                 (string-append "--prefix="
                                #$output)))
              (setenv "PYTHONPATH"
                      (string-append (getcwd) "/python"))))
          (add-before 'build 'set_cuda_paths
            (lambda _
              (setenv "CUDACXX"
                      #$(file-append (this-package-input "cuda-toolkit")
                                     "/bin/nvcc"))))
          (add-after 'install 'cleanup
            (lambda _
              (delete-file-recursively (string-append #$output "/test")))))))
    (native-inputs (list python python-setuptools git-minimal))
    (inputs (list cuda-toolkit cudnn))
    (propagated-inputs (list nvidia-driver
                             python-networkx
                             python-numpy
                             python-pydot
                             python-scipy
                             python-treelib))
    (synopsis
     "CUDA C++ template abstractions for high-performance linear algebra")
    (description
     "CUTLASS is a collection of CUDA C++ template abstractions for implementing
high-performance matrix-matrix multiplication (GEMM) and related computations
at all levels and scales within CUDA.  It incorporates strategies for
hierarchical decomposition and data movement similar to those used to
implement cuBLAS and cuDNN.

CUTLASS decomposes these ``moving parts'' into reusable, modular software
components abstracted by C++ template classes.  Primitives for different
levels of a conceptual parallelization hierarchy can be specialized and tuned
via custom tiling sizes, data types, and other algorithmic policy.  The
resulting flexibility simplifies their use as building blocks within custom
kernels and applications.")
    (license license-gnu:bsd-3)))

(define-public cutlass-headers
  (package
    (name "cutlass-headers")
    (version "4.1.0")
    (home-page "https://github.com/NVIDIA/cutlass")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxss2jxbgjija8c7cryjq75irrnj17f5yjfppmaf2y21x7bm3v5"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("LICENSE.txt" "LICENSE"))))
    (synopsis
     "CUDA C++ template abstractions for high-performance linear algebra")
    (description
     "CUTLASS is a collection of CUDA C++ template abstractions for implementing
high-performance matrix-matrix multiplication (GEMM) and related computations
at all levels and scales within CUDA.  It incorporates strategies for
hierarchical decomposition and data movement similar to those used to
implement cuBLAS and cuDNN.")
    (license license-gnu:bsd-3)))

(define-public cutlass-headers-3.4
  (package
    (inherit cutlass-headers)
    (name "cutlass-headers")
    (version "3.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i8h7hfa7ixlhk58p7cyam6l7zzbsir6jm6zv3vfjc6cbp8bqlzk"))))))

(define-public cutlass-tools
  (package
    (name "cutlass-tools")
    (version "4.1.0")
    (home-page "https://github.com/NVIDIA/cutlass")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxss2jxbgjija8c7cryjq75irrnj17f5yjfppmaf2y21x7bm3v5"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("tools" "tools")
                        ("LICENSE.txt" "LICENSE"))))
    (synopsis
     "CUDA C++ template abstractions for high-performance linear algebra")
    (description
     "CUTLASS is a collection of CUDA C++ template abstractions for implementing
high-performance matrix-matrix multiplication (GEMM) and related computations
at all levels and scales within CUDA.  It incorporates strategies for
hierarchical decomposition and data movement similar to those used to
implement cuBLAS and cuDNN.")
    (license license-gnu:bsd-3)))

(define-public cutlass-tools-3.4
  (package
    (inherit cutlass-tools)
    (name "cutlass-tools")
    (version "3.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i8h7hfa7ixlhk58p7cyam6l7zzbsir6jm6zv3vfjc6cbp8bqlzk"))))))

(define-public nccl
  (package
    (name "nccl")
    (version "2.28.1-1")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/NVIDIA/nccl")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0cmkvmr728nwb9f8vq4l39dbgjfwppq9jzdcha3cv7npssw0g31s"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CUDA_HOME="
                             #$(this-package-input "cuda-toolkit")))
      ;; No tests in source.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; No configure script.
          (delete 'configure)
          (add-before 'install 'set-prefix
            (lambda _
              (setenv "PREFIX"
                      #$output))))))
    (native-inputs (list python which))
    (inputs (list cuda-toolkit))
    (propagated-inputs (list nvidia-driver))
    (home-page "https://developer.nvidia.com/nccl")
    (synopsis
     "Optimized primitives for collective multi-GPU communication between
NVIDIA GPUs")
    (description
     "NCCL (pronounced \"Nickel\") is a stand-alone library of standard
communication routines for NVIDIA GPUs, implementing all-reduce,
all-gather, reduce, broadcast, reduce-scatter, as well as any
send/receive based communication pattern. It has been optimized to
achieve high bandwidth on platforms using PCIe, NVLink, NVswitch, as
well as networking using InfiniBand Verbs or TCP/IP sockets. NCCL
supports an arbitrary number of GPUs installed in a single node or
across multiple nodes, and can be used in either single- or
multi-process (e.g., MPI) applications.")
    (license license-gnu:bsd-3)))

(define-public cutensor
  (package
    (name "cutensor")
    (version "2.3.0.6")
    (home-page "https://developer.nvidia.com/cutensor")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cutensor/redist/libcutensor/linux-x86_64/libcutensor-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "12zxv2nzr5kcn49p8cgjncmzpbkmnjyjrldll7fjabm3v82fl0w9"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("LICENSE" "LICENSE"))))
    (synopsis "Nvidia cuTENSOR library")
    (description "This package provides the proprietary cuTENSOR
library for NVIDIA GPUs.")
    (license (license:nonfree
              "https://docs.nvidia.com/cuda/cutensor/latest/license.html"))))

(define-public no-float128
  ;; FIXME: We cannot simply add it to 'propagated-inputs' of cuda-toolkit
  ;; because then it would come after glibc in CPLUS_INCLUDE_PATH.
  (package
    (name "no-float128")
    (version "0")
    (source
     #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))

          (let* ((header "/include/bits/floatn.h")
                 (target (string-append #$output
                                        (dirname header)))
                 (libc #$(this-package-input "libc")))
            (mkdir-p target)
            (install-file (string-append libc header) target)
            (substitute* (string-append target "/"
                                        (basename header))
              (("#([[:blank:]]*)define __HAVE_FLOAT128[[:blank:]]+1" _ space)
               (string-append "#" space "define __HAVE_FLOAT128 0")))))))
    (inputs `(("libc" ,glibc)))
    (home-page "https://hpc.guix.info")
    (synopsis "@file{<bits/floatn.h>} header that disables float128 support")
    (description
     "This package provides a @file{<bits/floatn.h>} header to override that
of glibc and disable float128 support.  This is required allow the use of
@command{nvcc} with CUDA 8.0 and glibc 2.26+.  Otherwise, @command{nvcc} fails like this:

@example
/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(61): error: invalid argument to attribute \"__mode__\"

/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(73): error: identifier \"__float128\" is undefined
@end example

See also
@url{https://devtalk.nvidia.com/default/topic/1023776/cuda-programming-and-performance/-request-add-nvcc-compatibility-with-glibc-2-26/1}.")
    (license license-gnu:gpl3+)))

(define-public cusparselt
  (package
    (name "cusparselt")
    (version "0.8.0.4")
    (home-page "https://docs.nvidia.com/cuda/cusparselt/")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cusparselt/redist/libcusparse_lt/linux-x86_64/libcusparse_lt-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "024wcs477wvafc1vas3lbppmz3chadni5vzcgf3xxfk62xcm8fa8"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("LICENSE" "LICENSE"))))
    (synopsis
     "A High-Performance CUDA Library for Sparse Matrix-Matrix Multiplication")
    (description
     "NVIDIA cuSPARSELt is a high-performance CUDA library dedicated to general matrix-matrix operations in which at least one operand is a sparse matrix. The cuSPARSELt APIs allow flexibility in the algorithm/operation selection, epilogue, and matrix characteristics, including memory layout, alignment, and data types.")
    (license (license:nonfree
              "https://docs.nvidia.com/cuda/cusparselt/license.html"))))

(define-public cudss
  (package
    (name "cudss")
    (version "0.6.0.5")
    (home-page "https://docs.nvidia.com/cuda/cudss/")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudss/redist/libcudss/linux-x86_64/libcudss-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "0x9fy0kwjs07k2n9nkx9nx9rr1jbwi1zjp6i1cxs3fz4wgaf370m"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #t
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("LICENSE" "LICENSE"))))
    (synopsis "A high-performance CUDA Library for Direct Sparse Solvers")
    (description
     "NVIDIA cuDSS (Preview) is a library of GPU-accelerated linear solvers with sparse matrices. It provides algorithms for solving linear systems of the following type: AX = B with a sparse matrix A, right-hand side B, and unknown solution X (could be a matrix or a vector). The cuDSS functionality allows flexibility in matrix properties and solver configuration, as well as execution parameters like CUDA streams.")
    (license (license:nonfree
              "https://docs.nvidia.com/cuda/cudss/license.html"))))

(define-public nvidia-modprobe
  (package
    (name "nvidia-modprobe")
    (version "580.76.05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/nvidia-modprobe")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lnxafs2aaa4fmdcyssxvsdjsjfbdapbhj38kqf3dqmbxgx5sr9l"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'set-correct-cflags
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (setenv "CFLAGS" "-fPIC")
              (display "setting CFLAGS\n")
              (substitute* "modprobe-utils/nvidia-modprobe-utils.c"
                (("^static int nvidia_cap_get_device_file_attrs")
                 "int nvidia_cap_get_device_file_attrs"))))
          (add-after 'build 'build-static-link-libraries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (invoke "ar" "rcs"
                      "_out/Linux_x86_64/libnvidia-modprobe-utils.a"
                      "_out/Linux_x86_64/nvidia-modprobe-utils.o"
                      "_out/Linux_x86_64/pci-sysfs.o")
              (copy-recursively "_out/Linux_x86_64/"
                                (string-append (assoc-ref %outputs "out")
                                               "/lib"))))
          (delete 'check)
          (add-after 'patch-source-shebangs 'replace-prefix
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (setenv "CC" "gcc")
              (setenv "PREFIX"
                      (assoc-ref %outputs "out"))
              (copy-recursively "modprobe-utils/"
                                (string-append (assoc-ref %outputs "out")
                                               "/include")) #t) ;must return true for success
            ))
      #:tests? #f))
    (native-inputs (list gcc-toolchain m4))
    (synopsis
     "Load the NVIDIA kernel module and create NVIDIA character device files")
    (description
     "Load the NVIDIA kernel module and create NVIDIA character device files")
    (home-page "https://github.com/NVIDIA/nvidia-modprobe")
    (license license-gnu:gpl2)))

(define-public nvtx
  (package
    (name "nvtx")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/NVTX")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xlxjr7s12n9q83mbi1pk7jd73npd9fr8pn8b2a4xc5labbc6m0p"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("c/include" "include")
          ("LICENSE.txt" "share/doc/nvtx/LICENSE.txt"))))
    (home-page "https://github.com/NVIDIA/NVTX")
    (synopsis "NVIDIA Tools Extension Library")
    (description
     "NVTX (NVIDIA Tools Extension) is a C-based API for annotating events,
code ranges, and resources in applications.  These annotations are used by
developer tools from NVIDIA and third parties to visualize the execution of
applications.  NVTX provides a platform-independent API that can be used to
instrument CPU and GPU code.  It is particularly useful for profiling and
debugging CUDA applications with tools like NVIDIA Nsight Systems and NVIDIA
Nsight Compute.")
    (license license-gnu:asl2.0)))

