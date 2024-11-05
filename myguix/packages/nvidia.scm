;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2022, 2023, 2024 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2024 Ayan Das <bvits@riseup.net>

(define-module (myguix packages nvidia)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((myguix licenses)
                #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (myguix build-system cuda)
  #:use-module (myguix packages linux)
  #:use-module (ice-9 match))

(define-public %nvidia-environment-variable-regexps
  '("^__GL_" ;NVIDIA OpenGL settings.
    "^__GLX_VENDOR_LIBRARY_NAME$" ;For GLVND.
    ;; NVIDIA PRIME Render Offload.
    "^__NV_PRIME_RENDER_OFFLOAD(_PROVIDER)?$"
    "^__VK_LAYER_NV_optimus$"
    ;; NVIDIA NGX.
    "^__NGX_CONF_FILE$"
    "^__NV_SIGNED_LOAD_CHECK$"
    "^PROTON_ENABLE_NGX_UPDATER$"
    ;; NVIDIA VDPAU settings.
    "^VDPAU_NVIDIA_"
    ;; GSYNC control for Vulkan direct-to-display applications.
    "^VKDirectGSYNC(Compatible)?Allowed$"))

(define-public nvidia-version
  "550.127.05")


;;;
;;; NVIDIA driver checkouts
;;;
(define %nvidia-driver-hashes
  '(("550.127.05" . "1sba12953gz35qf5lj5dlfa80gcpdmmfngbdagbnp29abm7z716k")))

(define %nvidia-settings-hashes
  '(("550.127.05" . "0a5zdkizqa1pxvj88bwcph1ck51f4yhzqy3nmfc4malyrd78wi3i")))

(define (nvidia-source-unbundle-libraries version)
  #~(begin
      (use-modules (guix build utils))
      (for-each delete-file
                (find-files "."
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
                                           "libOpenCL\\.so\\.") "|")))))

(define* (make-nvidia-source version hash
                             #:optional (get-cleanup-snippet
                                         nvidia-source-unbundle-libraries))
  "Given VERSION and HASH of an NVIDIA driver installer, return an <origin> for
its unpacked checkout.  GET-CLEANUP-SNIPPET is a procedure that accepts the
VERSION as argument and returns a G-expression."
  (define installer
    (origin
      (method url-fetch)
      (uri (string-append
            "https://us.download.nvidia.com/XFree86/Linux-x86_64/" version
            "/NVIDIA-Linux-x86_64-" version ".run"))
      (sha256 hash)))
  (origin
    (method (@@ (guix packages) computed-origin-method))
    (file-name (string-append "nvidia-driver-" version "-checkout"))
    (sha256 #f)
    (snippet (get-cleanup-snippet version))
    (uri (delay (with-imported-modules '((guix build utils))
                                       #~(begin
                                           (use-modules (guix build utils)
                                                        (ice-9 ftw))
                                           (set-path-environment-variable
                                            "PATH"
                                            '("bin")
                                            '#+(list bash-minimal
                                                     coreutils
                                                     gawk
                                                     grep
                                                     tar
                                                     which
                                                     xz
                                                     zstd))
                                           (setenv "XZ_OPT"
                                                   (string-join (%xz-parallel-args)))
                                           (invoke "sh"
                                                   #$installer "-x")
                                           (copy-recursively (car (scandir (canonicalize-path
                                                                            (getcwd))
                                                                           (lambda 
                                                                                   (file)
                                                                             (not
                                                                              (member
                                                                               file
                                                                               '
                                                                               ("."
                                                                                ".."))))))
                                                             #$output)))))))

(define-public nvidia-source
  (make-nvidia-source nvidia-version
                      (base32 (assoc-ref %nvidia-driver-hashes nvidia-version))))


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
    (version nvidia-version)
    (source
     nvidia-source)
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (srfi srfi-26))
      #:install-plan #~`((#$(match (or (%current-target-system)
                                       (%current-system))
                              ("i686-linux" "32")
                              ("x86_64-linux" ".")
                              (_ "."))
                          "lib/"
                          #:include-regexp ("^./[^/]+\\.so"))
                         ("." "share/nvidia/"
                          #:include-regexp ("nvidia-application-profiles"))
                         ("." "share/egl/egl_external_platform.d/"
                          #:include-regexp ("(gbm|wayland)\\.json"))
                         ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
                         ("90-nvidia.rules" "lib/udev/rules.d/")
                         ("nvidia-drm-outputclass.conf"
                          "share/X11/xorg.conf.d/")
                         ("nvidia-dbus.conf" "share/dbus-1/system.d/")
                         ("nvidia.icd" "etc/OpenCL/vendors/")
                         ("nvidia_icd.json" "share/vulkan/icd.d/")
                         ("nvidia_layers.json"
                          "share/vulkan/implicit_layer.d/"))
      #:phases #~(modify-phases %standard-phases
                   (delete 'strip)
                   (add-after 'unpack 'create-misc-files
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; EGL external platform configuraiton
                       (substitute* '("10_nvidia_wayland.json"
                                      "15_nvidia_gbm.json")
                         (("libnvidia-egl-(wayland|gbm)\\.so\\.." all)
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
                                   "/share/glvnd/egl_vendor.d/10_nvidia.json"
                                   "/share/vulkan/icd.d/nvidia_icd.json"
                                   "/share/vulkan/implicit_layer.d/nvidia_layers.json"))))
                   (add-after 'install 'patch-elf
                     (lambda _
                       (let* ((ld.so (string-append #$(this-package-input
                                                       "glibc")
                                                    #$(glibc-dynamic-linker)))
                              (rpath (string-join (list (string-append #$output
                                                                       "/lib")
                                                        (string-append #$openssl-1.1
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "egl-wayland")
                                                                       "/lib")
                                                        (string-append (ungexp
                                                                        (this-package-input
                                                                         "gcc")
                                                                        "lib")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "glibc")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "libdrm")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "libglvnd")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "libx11")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "libxext")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "openssl")
                                                                       "/lib")
                                                        (string-append #$(this-package-input
                                                                          "wayland")
                                                                       "/lib"))
                                                  ":")))
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
                                     (let ((bindir (string-append #$output
                                                                  "/bin"))
                                           (manual (string-append binary
                                                                  ".1.gz"))
                                           (mandir (string-append #$output
                                                    "/share/man/man1")))
                                       (install-file binary bindir)
                                       (when (file-exists? manual)
                                         (install-file manual mandir))))
                                   '("nvidia-cuda-mps-control"
                                     "nvidia-cuda-mps-server" "nvidia-smi")))))
                   (add-before 'patch-elf 'relocate-libraries
                     (lambda _
                       (let* ((version #$(package-version this-package))
                              (libdir (string-append #$output "/lib"))
                              (gbmdir (string-append libdir "/gbm"))
                              (vdpaudir (string-append libdir "/vdpau"))
                              (xorgmoddir (string-append libdir
                                                         "/xorg/modules"))
                              (xorgdrvdir (string-append xorgmoddir "/drivers"))
                              (xorgextdir (string-append xorgmoddir
                                                         "/extensions"))
                              (move-to-dir (lambda (file dir)
                                             (install-file file dir)
                                             (delete-file file))))
                         (for-each (lambda (file)
                                     (mkdir-p gbmdir)
                                     (with-directory-excursion gbmdir
                                       (symlink file "nvidia-drm_gbm.so")))
                                   (find-files libdir
                                               "libnvidia-allocator\\.so\\."))

                         (for-each (cut move-to-dir <> vdpaudir)
                                   (find-files libdir
                                               "libvdpau_nvidia\\.so\\."))

                         (for-each (cut move-to-dir <> xorgdrvdir)
                                   (find-files libdir "nvidia_drv\\.so$"))

                         (for-each (lambda (file)
                                     (move-to-dir file xorgextdir)
                                     (with-directory-excursion xorgextdir
                                       (symlink (basename file)
                                                "libglxserver_nvidia.so")))
                                   (find-files libdir
                                               "libglxserver_nvidia\\.so\\.")))))
                   (add-after 'patch-elf 'create-short-name-symlinks
                     (lambda _
                       (define (get-soname file)
                         (when (elf-file? file)
                           (let* ((cmd (string-append
                                        "patchelf --print-soname " file))
                                  (port (open-input-pipe cmd))
                                  (soname (read-line port)))
                             (close-pipe port) soname)))
                       (for-each (lambda (lib)
                                   (let ((lib-soname (get-soname lib)))
                                     (when (string? lib-soname)
                                       (let* ((soname (string-append (dirname
                                                                      lib) "/"
                                                       lib-soname))
                                              (base (string-append (regexp-substitute
                                                                    #f
                                                                    (string-match
                                                                     "(.*)\\.so.*"
                                                                     soname) 1)
                                                                   ".so"))
                                              (source (basename lib)))
                                         (for-each (lambda (target)
                                                     (unless (file-exists?
                                                              target)
                                                       (format #t
                                                        "Symlinking ~a -> ~a..."
                                                        target source)
                                                       (symlink source target)
                                                       (display " done\n")))
                                                   (list soname base))))))
                                 (find-files #$output "\\.so\\.")))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs (list patchelf))
    (inputs (list egl-gbm
                  egl-wayland
                  `(,gcc-11 "lib")
                  glibc
                  libdrm
                  libglvnd-for-nvda
                  libx11
                  libxext
                  openssl
                  openssl-1.1
                  wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (libraries)")
    (description
     "This package provides libraries of the proprietary NVIDIA driver.  It's
mainly used as a dependency of other packages.  For user-facing purpose, use
@code{nvda} instead.")
    (license (nonfree:nonfree (format #f
                               "file:///share/doc/nvidia-driver-~a/LICENSE"
                               version)))))

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
        #:install-plan #~'(("firmware" #$(string-append "lib/firmware/nvidia/"
                                          (package-version this-package))))
        #:phases #~(modify-phases %standard-phases
                     (delete 'strip))))
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


;;;
;;; NVIDIA kernel modules
;;;

(define-public nvidia-module
  (package
    (name "nvidia-module")
    (version nvidia-version)
    (source
     nvidia-source)
    (build-system linux-module-build-system)
    (arguments
     (list
      #:linux linux
      #:source-directory "kernel"
      #:tests? #f
      #:make-flags #~(list (string-append "CC="
                                          #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'strip)
                   (add-before 'configure 'fixpath
                     (lambda* (#:key (source-directory ".") #:allow-other-keys)
                       (substitute* (string-append source-directory "/Kbuild")
                         (("/bin/sh")
                          (which "sh")))))
                   (replace 'build
                     (lambda* (#:key (make-flags '())
                               (parallel-build? #t)
                               (source-directory ".") inputs
                               #:allow-other-keys)
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
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (kernel modules)")
    (description
     "This package provides kernel modules of the proprietary NVIDIA driver.
Module setup can be done with @code{nvidia-service-type}, to actually use these
modules, also add @code{modprobe.blacklist=nouveau} to @code{kernel-arguments}
field of the @code{operating-system} configuration.

If the NVIDIA card is not used for displaying, or on a Wayland environment,
add @code{nvidia_drm.modeset=1} to @code{kernel-arguments} as well.")
    (license (nonfree:nonfree (format #f
                               "file:///share/doc/nvidia-driver-~a/LICENSE"
                               version)))))

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
      (license license:gpl2))))


;;;
;;; ‘nvidia-settings’ packages
;;;

(define-public nvidia-settings
  (package
    (name "nvidia-settings")
    (version nvidia-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/nvidia-settings")
             (commit version)))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))
       (sha256
        (base32 (assoc-ref %nvidia-settings-hashes version)))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no test suite
      #:make-flags #~(list "NV_USE_BUNDLED_LIBJANSSON=0"
                           (string-append "PREFIX="
                                          #$output)
                           (string-append "CC="
                                          #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
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
                                     (string-append #$output
                                                    "/share/applications"))
                       (install-file "doc/nvidia-settings.png"
                                     (string-append #$output
                                      "/share/icons/hicolor/128x128/apps"))))
                   (add-after 'install 'wrap-program
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (wrap-program (string-append out
                                                      "/bin/nvidia-settings")
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
                  libxxf86vm))
    (synopsis "Nvidia driver control panel")
    (description
     "This package provides Nvidia driver control panel for monitor
configuration, creating application profiles, gpu monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license:gpl2)))


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
    (inherit nvidia-driver)
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
      #:builder #~(begin
                    (use-modules (guix build union))
                    (union-build #$output
                                 '#$(list (this-package-input "libglvnd")
                                          (this-package-input "mesa")
                                          (this-package-input "nvidia-driver"))))))
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
      (search-path-specification
       (variable "VDPAU_DRIVER_PATH")
       (files '("lib/vdpau"))
       (separator #f))
      ;; https://github.com/KhronosGroup/Vulkan-Loader/blob/main/docs/LoaderLayerInterface.md
      (search-path-specification
       (variable "XDG_DATA_DIRS")
       (files '("share")))))
    (synopsis "NVIDIA driver package")
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
    (inputs (list mesa-for-nvda nvidia-driver))
    (outputs '("out"))))

(define mesa/fake
  (package
    (inherit mesa)
    (replacement nvda)))

(define-public replace-mesa
  (package-input-rewriting `((,mesa unquote mesa/fake))))

(define-public (replace-mesa-from-propagated-inputs package)
  ;; Apply replace-mesa to every package input.
  (map (match-lambda
         ((_ (? package? pkg))
          (replace-mesa pkg))
         ((_ (? package? pkg) output)
          (replace-mesa (list pkg output))))
       (package-propagated-inputs package)))


;;;
;;; Other packages
;;;

(define-public nvidia-exec
  (package
    (name "nvidia-exec")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pedro00dk/nvidia-exec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "079alqgz3drv5mvx059fzhj3f20rnljl7r4yihfd5qq7djgmvv0v"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~`(("nvx" "bin/"))
      #:modules #~((guix build copy-build-system)
                   (guix build utils)
                   (srfi srfi-1))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'wrap-nvx
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (wrap-program (string-append #$output "/bin/nvx")
                         `("PATH" ":" prefix
                           ,(fold (lambda (input paths)
                                    (let* ((in (assoc-ref inputs input))
                                           (bin (string-append in "/bin")))
                                      (append (filter file-exists?
                                                      (list bin)) paths)))
                                  '()
                                  '("jq" "lshw" "lsof")))))))))
    (inputs (list bash-minimal jq lshw lsof))
    (home-page "https://github.com/pedro00dk/nvidia-exec")
    (synopsis "GPU switching without login out for Nvidia Optimus laptops")
    (description
     "This package provides GPU switching without login out for Nvidia Optimus
laptops.")
    (license license:gpl3+)))

(define-public nvidia-nvml
  (package
    (name "nvidia-nvml")
    (version "352.79")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cuda/7.5/Prod/gdk/"
             (format #f "gdk_linux_amd64_~a_release.run"
                     (string-replace-substring version "." "_"))))
       (sha256
        (base32 "1r2cwm0j9svaasky3qw46cpg2q6rrazwzrc880nxh6bismyd3a9z"))
       (file-name (string-append "nvidia-nvml-" version "-checkout"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (replace 'unpack
                     (lambda* (#:key source #:allow-other-keys)
                       (invoke "sh" source "--tar" "xvf"))))
      #:install-plan ''(("payload/nvml/lib" "lib")
                        ("payload/nvml/include" "include/nvidia/gdk")
                        ("payload/nvml/example" "src/gdk/nvml/examples")
                        ("payload/nvml/doc/man" "share/man")
                        ("payload/nvml/README.txt" "README.txt")
                        ("payload/nvml/COPYRIGHT.txt" "COPYRIGHT.txt"))))
    (home-page "https://www.nvidia.com")
    (synopsis "The NVIDIA Management Library (NVML)")
    (description
     "C-based programmatic interface for monitoring and managing various
states within NVIDIA Tesla GPUs.  It is intended to be a platform for
building 3rd party applications, and is also the underlying library for the
NVIDIA-supported nvidia-smi tool.  NVML is thread-safe so it is safe to make
simultaneous NVML calls from multiple threads.")
    ;; Doesn't have any specific LICENSE file, but see COPYRIGHT.txt for details.
    (license (nonfree:nonfree "file://COPYRIGHT.txt"))))

(define-public nvidia-system-monitor
  (package
    (name "nvidia-system-monitor")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/congard/nvidia-system-monitor-qt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aghdqljvjmc02g9jpc7sb3yhha738ywny51riska56hkxd3jg2l"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
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
    (license license:expat)))

(define-public cuda-toolkit
  (package
    (name "cuda-toolkit")
    (version "12.4.0")
    (source
     (origin
       (uri
        "https://developer.download.nvidia.com/compute/cuda/12.4.0/local_installers/cuda_12.4.0_550.54.14_linux.run")
       (sha256
        (base32 "05vxwn91hhrc57p8vr3xi5dbjiwdnwdnp2xnrmshajd9xks45a76"))
       (method url-fetch)))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (outputs '("out"))
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (ice-9 ftw))
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
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
                                            (string-append #$output
                                                           "/nvvm/lib64")
                                            (string-append libc "/lib")
                                            (string-append gcc-lib "/lib"))
                                      ":"))
                       (define (patch-elf file)
                         (make-file-writable file)
                         (format #t "Setting RPATH on '~a'...~%" file)
                         ;; RPATH should be modified before the interpreter. If
                         ;; done the other way around, it nukes the resulting
                         ;; binary.
                         (invoke "patchelf" "--set-rpath" rpath
                                 "--force-rpath" file)
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
                         (copy-recursively "cuda_nvcc/nvvm/libdevice"
                                           (string-append #$output
                                                          "/nvvm/libdevice")))))
                   (add-after 'install 'install-cupti
                     (lambda _
                       (copy-recursively "builds/cuda_cupti/extras/CUPTI"
                                         #$output)))
                   (add-after 'install 'delete-stray-symlinks
                     (lambda _
                       (delete-file (string-append #$output "/include/include")))))))
    (native-inputs (list which patchelf perl python-2))
    (inputs `(("gcc:lib" ,gcc-11 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (license (nonfree:nonfree
              "https://developer.nvidia.com/nvidia-cuda-license"))))

(define-public cudnn
  (package
    (name "cudnn")
    (version "9.2.1.18")
    (source
     (origin
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
             version "_cuda12-archive.tar.xz"))
       (sha256
        (base32 "16c34a0ymxhh3867pk53bwf81dicgci5cq5n723nc8isvnkxrqnn"))
       (method url-fetch)))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases #~(modify-phases %standard-phases
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
                                            (string-append #$output
                                                           "/nvvm/lib64")
                                            (string-append libc "/lib")
                                            (string-append gcc-lib "/lib"))
                                      ":"))

                       (define (patch-elf file)
                         (make-file-writable file)
                         (unless (string-contains file ".so")
                           (format #t "Setting interpreter on '~a'...~%" file)
                           (invoke "patchelf" "--set-interpreter" ld.so file))
                         (format #t "Setting RPATH on '~a'...~%" file)
                         (invoke "patchelf" "--set-rpath" rpath
                                 "--force-rpath" file))

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
    (native-inputs (list patchelf))
    (inputs `(("gcc:lib" ,gcc-11 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis "NVIDIA CUDA Deep Neural Network library (cuDNN)")
    (description "This package provides the CUDA Deep Neural Network library.")
    (license (nonfree:nonfree
              "https://docs.nvidia.com/deeplearning/cudnn/sla/index.html"))))

(define-public cutlass
  (package
    (name "cutlass")
    (version "3.5.1")
    (home-page "https://github.com/NVIDIA/cutlass")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/cutlass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h1cvlvmm0mcvsij8382qdzzswy75zyaybgaxj84md73wqvrhcdi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list (string-append "-DGOOGLETEST_DIR="
                                               #$(package-source googletest))
                                "-DCUTLASS_ENABLE_EXAMPLES=NO"
                                "-DCUTLASS_NVCC_ARCHS=80"
                                "-DCUTLASS_LIBRARY_KERNELS=all"
                                "-DCUTLASS_ENABLE_TESTS=NO"
                                "-DCUTLASS_INSTALL_TESTS=NO")
      #:validate-runpath? #f))
    (native-inputs (list python))
    (inputs (list cuda-toolkit))
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
    (license license:bsd-3)))

(define-public nccl
  (package
    (name "nccl")
    (version "2.22.3-1")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/NVIDIA/nccl")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1kwh4950q953c2sr7ir2inyw34mwh5av7cq93j852yd2sqxyyk3v"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CUDA_HOME="
                                          #$(this-package-input "cuda-toolkit")))
      ;; No tests in source.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   ;; No configure script.
                   (delete 'configure)
                   (add-before 'install 'set-prefix
                     (lambda _
                       (setenv "PREFIX"
                               #$output))))))
    (native-inputs (list python which))
    (inputs (list cuda-toolkit))
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
    (license license:bsd-3)))

(define-public cutensor
  (package
    (name "cutensor")
    (version "2.0.1.2")
    (home-page "https://developer.nvidia.com/cutensor")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cutensor/redist/libcutensor/linux-x86_64/libcutensor-linux-x86_64-"
             version "-archive.tar.xz"))
       (sha256
        (base32 "18l6qmfjcn75jsyzlsj66mji8lgab2ih19d0drqavfi2lqna3vgd"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("LICENSE" "LICENSE"))))
    (synopsis "Nvidia cuTENSOR library")
    (description "This package provides the proprietary cuTENSOR
library for NVIDIA GPUs.")
    (license (nonfree:nonfree
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
      #:builder #~(begin
                    (use-modules (guix build utils))

                    (let* ((header "/include/bits/floatn.h")
                           (target (string-append #$output
                                                  (dirname header)))
                           (libc #$(this-package-input "libc")))
                      (mkdir-p target)
                      (install-file (string-append libc header) target)
                      (substitute* (string-append target "/"
                                                  (basename header))
                        (("#([[:blank:]]*)define __HAVE_FLOAT128[[:blank:]]+1"
                          _ space)
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
    (license license:gpl3+)))

(define-public cuda-cccl
  (package
    (name "cuda-cccl")
    (version "12.1.109")
    (source
     (origin
       (method url-fetch)
       (uri (cuda-module-url name version))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "1ahvk632nh05m3mmjk8mhkxgkmry1ipq89dycw98kd617png6kmq")
                  ("aarch64-linux"
                   "1yc5irxn35ii0qal1qi8v6gq25ws4a7axjnmc5b20g0ypzxdlc2n")
                  ("powerpc64le-linux"
                   "0s6zidp5ajsqh519x3c38ihip4m1hkdzhrsdq04pybk8sfjh7z2l"))))))
    (build-system cuda-build-system)
    (arguments
     (list
      #:install-plan ''(("include" "include")
                        ("lib" "lib"))))
    (synopsis "C++ Core Compute Libraries for the CUDA language")
    (description
     "This package provides the CUDA C++ developers with building blocks that
make it easier to write safe and efficient code.  It unifies three essential former
CUDA C++ libraries into a single repository:
@itemize
@item Thrust (former repo)
@item CUB (former repo)
@item libcudacxx (former repo)
@end itemize")
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (license (cuda-license name))))

(define-public cuda-nvrtc
  (package
    (name "cuda-nvrtc")
    (version "12.1.105")
    (source
     (origin
       (method url-fetch)
       (uri (cuda-module-url name version))
       (sha256
        (base32 (match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux"
                   "0yriv3gcb4kpvpav3ilv8zyhravmz0blb0gv1c7pfq37r9m705dv")
                  ("aarch64-linux"
                   "0amp7qg64i6rfkqnjinizh9vhpajvqdpyan4jda9vqr7ckrdfq31")
                  ("powerpc64le-linux"
                   "10dwwhk2pfz6dcqpgjp2dryg5qb08ghnbxvbk4mfhvsajj9ik4wv"))))))
    (build-system cuda-build-system)
    (arguments
     (list
      #:install-plan ''(("include" "include")
                        ("lib" "lib")
                        ("pkg-config" "share/pkg-config"))))
    (inputs (list `(,gcc "lib") glibc))
    (outputs (list "out" "static"))
    (synopsis "Runtime compilation library for CUDA C++")
    (description
     "This package accepts CUDA C++ source code in character string form and
creates handles that can be used to obtain the CUDA PTX, for further
instrumentation with the CUDA Toolkit.  It allows to shrink compilation
overhead and simplify application deployment.")
    (home-page "https://docs.nvidia.com/cuda/nvrtc/index.html")
    (license (cuda-license name))))
