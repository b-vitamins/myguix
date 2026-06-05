(define-module (myguix packages maths)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages compiler-tools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses)
                #:prefix license-gnu:)
  #:use-module ((myguix licenses)
                #:prefix license:)
  #:use-module (myguix build-system bazel)
  #:use-module (myguix packages)
  #:use-module (myguix packages bazel)
  #:use-module (myguix packages nvidia)
  #:use-module ((gnu packages bootstrap)
                #:select (glibc-dynamic-linker))
  #:use-module (srfi srfi-26))


;;;
;;; SMT solvers.
;;;

(define (dreal-bazel-distdir-source url hash)
  (origin
    (method url-fetch)
    (uri url)
    (sha256 (base32 hash))))

(define dreal-bazel-distdir-sources
  (list
   (dreal-bazel-distdir-source
    "https://github.com/bazelbuild/bazel-skylib/archive/1.0.3.tar.gz"
    "1jvb4h8rvf7gk6xycn9nhwn4jdg2h7phjppvkxdnzbf4q24gmh3s")
   (dreal-bazel-distdir-source
    "https://github.com/cpplint/cpplint/archive/1.5.3.tar.gz"
    "1wr8j17licsgzx3frlfkqfy21kl3f2nc0nv52wniq8pwcpg0hxa4")
   (dreal-bazel-distdir-source
    "https://github.com/PyCQA/pycodestyle/archive/2.6.0.tar.gz"
    "15wwqcphcbj0dxvnp7w8wnj95clrkfpyih8p679sz4nc92y7yd08")
   (dreal-bazel-distdir-source
    "https://github.com/dreal-deps/ezoptionparser/archive/94bc81269eb500fb188727777e1ced9b15d97572.tar.gz"
    "1yzp3fx58shivgcq31rj0z98i5fgw8pgifd33mqja73s3p16mww1")
   (dreal-bazel-distdir-source
    "https://github.com/google/googletest/archive/662fe38e44900c007eccb65a5d2ea19df7bd520e.tar.gz"
    "18skbb5gqlfwvv502las6rs514m6x70zbh6cc6x4y8j5vvdl54vi")
   (dreal-bazel-distdir-source
    "https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz"
    "1843rhjcrdz7x8lmlxm6yr93l8hkxs842m5drvrw0gisa8w69m5n")
   (dreal-bazel-distdir-source
    "https://github.com/bazelbuild/rules_pkg/releases/download/0.3.0/rules_pkg-0.3.0.tar.gz"
    "0jl04fwqykyzqb6rr4f7l25vwbzkzh33nxqnz010rdnpmjknjnbb")
   (dreal-bazel-distdir-source
    "https://github.com/gabime/spdlog/archive/v1.8.5.tar.gz"
    "1f5l8fjka7q6wpni0s7krrknzlay7c7vp8nwk09p5b33qzbhnkcl")
   (dreal-bazel-distdir-source
    "https://github.com/fmtlib/fmt/archive/7.1.3.tar.gz"
    "1k2yq341aflrgy6kdpniv94nkdxv0ks0xmak5phl6c1b0ir71bjw")
   (dreal-bazel-distdir-source
    "https://github.com/dreal-deps/picosat/archive/4ee7aa1d1c645df8fa9daa07f2be17c6d03b35fc.tar.gz"
    "18x7nvq81ldm9izsc8px4b48vhs1b4lys657az4ksklxcp9n3r0v")
   (dreal-bazel-distdir-source
    "https://github.com/pybind/pybind11/archive/v2.12.0.tar.gz"
    "19w544l1jkbf5g9858hrjpbsnwgvj1s0d9qnsmsx7g0splm293xz")
   (dreal-bazel-distdir-source
    "https://github.com/abseil/abseil-cpp/archive/20200923.3.tar.gz"
    "1lnyagawb9g8sagqdflyl3kda4pk5v5f44a2pzj86wyjh0aavqpb")
   (dreal-bazel-distdir-source
    "https://github.com/khizmax/libcds/archive/v2.3.3.tar.gz"
    "1273wk370nqpq5yg9xvf90icnb6yc1rb5gghnb1a6qvbrl73i47h")))

(define-public ibex
  (package
    (name "ibex")
    (version "2.7.4-10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dreal-deps/ibex-lib")
             (commit "0112b423c94515302c551c478c30e98ff442752d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mzw0yjmq2sfq7pwp417j74xqn0ccb84wqrjs46b8z7j9v55j346"))
       (patches (myguix-patches "ibex-gcc15.patch"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "CXXFLAGS" "-D_GLIBCXX_HAVE_FENV_H=1")
              (invoke "python2.7" "./waf" "configure"
                      (string-append "--prefix=" #$output)
                      "--enable-shared"
                      "--interval-lib=filib"
                      "--lp-lib=clp"
                      "--with-optim"
                      "--with-solver"
                      "--with-affine")))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "python2.7" "./waf" "build"
                      (string-append
                       "-j"
                       (number->string
                        (if parallel-build?
                            (parallel-job-count)
                            1))))))
          (replace 'install
            (lambda _
              (invoke "python2.7" "./waf" "install")))
          (add-after 'install 'move-pkg-config-file
            (lambda _
              (let ((pkgconfig (string-append #$output "/lib/pkgconfig")))
                (mkdir-p pkgconfig)
                (rename-file (string-append #$output "/share/pkgconfig/ibex.pc")
                             (string-append pkgconfig "/ibex.pc")))))
          (add-after 'install 'set-rpaths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((runpath (string-join
                              (list (string-append #$output "/lib")
                                     (string-append #$(this-package-input "clp")
                                                   "/lib")
                                     (string-append #$(this-package-input "bzip2")
                                                   "/lib")
                                     (string-append
                                      (assoc-ref inputs "gcc:lib") "/lib")
                                     (string-append
                                      (assoc-ref inputs "glibc") "/lib"))
                              ":")))
                (for-each
                 (lambda (file)
                   (invoke "patchelf" "--set-rpath" runpath file))
                 (append (find-files (string-append #$output "/bin"))
                         (find-files (string-append #$output "/lib")
                                     "^libibex\\.so$"))))))
          (add-after 'set-rpaths 'check-installed
            (lambda _
              (invoke (string-append #$output "/bin/ibexsolve") "--version"))))))
    (native-inputs (list bash-minimal bison flex patchelf-0.16 pkg-config python-2))
    (propagated-inputs (list bzip2 clp))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("glibc" ,glibc)))
    (home-page "https://ibex-lib.readthedocs.io/")
    (synopsis "Interval-based solver library")
    (description
     "IBEX is a C++ library for interval arithmetic, constraint propagation,
and interval-based nonlinear solving.  This package uses the dReal-maintained
IBEX 2.7.4 branch with the Filib interval backend and CLP support.")
    (license license-gnu:lgpl3+)))

(define-public dreal
  (package
    (name "dreal")
    (version "4.21.06.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dreal/dreal4")
             (commit "4067225cd85ae8668d8172ed5dc594f573721134")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yyhvcwy96ygzx8zgfvfafcg0akxw77hxp57kg370g0lkl2ha3wz"))
       (patches (myguix-patches "dreal-guix-compat.patch"))))
    (supported-systems '("x86_64-linux"))
    (build-system bazel-build-system)
    (arguments
     (list
      #:tests? #f
      #:bazel bazel
      #:fetch-targets '(list "//dreal:dreal" "//:libdreal.so" "//dreal:_dreal_py.so")
      #:build-targets '(list "//dreal:dreal" "//:libdreal.so" "//dreal:_dreal_py.so")
      #:distdir-inputs dreal-bazel-distdir-sources
      #:bazel-jobs 4
      #:bazel-arguments
      #~(list "--cxxopt=-D_GLIBCXX_HAVE_FENV_H=1"
              "--action_env=PYTHON_BIN_PATH"
              "--host_action_env=PYTHON_BIN_PATH"
              (string-append "--python_path="
                             #$(this-package-input "python")
                             "/bin/python3"))
      #:vendored-inputs-hash
      "098cjvmc8rwpbsp2l8w22dnp3f7161kpx54nrvfnyh8h7icc435j"
      #:bazel-configuration
      #~(let* ((python (which "python3"))
               (shell (which "sh"))
               (pkg-config-paths
                (list (string-append #$(this-package-input "ibex")
                                     "/lib/pkgconfig")
                      (string-append #$(this-package-input "clp")
                                     "/lib/pkgconfig")
                      (string-append #$(this-package-input "nlopt")
                                     "/lib/pkgconfig")
                      (string-append #$(this-package-input "gmp")
                                     "/lib/pkgconfig"))))
          (setenv "PYTHON_BIN_PATH" python)
          (setenv "DREAL_GUIX_PKG_CONFIG_PATHS"
                  (string-join pkg-config-paths ":"))
          (setenv "DREAL_GUIX_GMP_PREFIX"
                  #$(this-package-input "gmp"))
          (setenv "BAZEL_SH" shell)
          (setenv "SHELL" shell)
          (setenv "CONFIG_SHELL" shell))
      #:phases
      #~(modify-phases (@ (myguix build bazel-build-system) %standard-phases)
          (add-after 'unpack 'patch-guix-paths
            (lambda _
              (let* ((pkg-config-paths
                      (list (string-append #$(this-package-input "ibex")
                                           "/lib/pkgconfig")
                            (string-append #$(this-package-input "clp")
                                           "/lib/pkgconfig")
                            (string-append #$(this-package-input "nlopt")
                                           "/lib/pkgconfig")))
                     (pkg-config-lines
                      (string-append
                       "pkg_config_paths = [\n"
                       (string-join
                        (map (lambda (path)
                               (string-append "            \"" path "\","))
                             pkg-config-paths)
                        "\n"))))
                (substitute* "dreal/workspace.bzl"
                  (("pkg_config_paths = \\[") pkg-config-lines))
                (substitute*
                    "third_party/com_github_google_kythe/tools/build_rules/BUILD.bazel"
                  (("/usr/bin/bison") (which "bison"))
                  (("/usr/bin/flex") (which "flex")))
                (substitute* "third_party/org_gmplib/repository.bzl"
                  (("/usr/include/x86_64-linux-gnu/gmp.h")
                   (string-append #$(this-package-input "gmp") "/include/gmp.h"))
                  (("/usr/include/gmp.h")
                   (string-append #$(this-package-input "gmp") "/include/gmp.h"))
                  (("/usr/include/gmpxx.h")
                   (string-append #$(this-package-input "gmp") "/include/gmpxx.h")))
                (substitute* "third_party/org_gmplib/package-ubuntu.BUILD.bazel"
                  (("-L/usr/lib/x86_64-linux-gnu")
                   (string-append "-L" #$(this-package-input "gmp") "/lib"))))))
          (add-after 'build 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (pkgconfig (string-append lib "/pkgconfig"))
                     (include (string-append out "/include"))
                     (python-version
                      #$(version-major+minor (package-version python)))
                     (site-packages
                      (string-append lib "/python" python-version
                                     "/site-packages/dreal"))
                     (install-relative-header
                      (lambda (root header)
                        (let ((relative
                               (substring header (string-length root))))
                          (install-file
                           header
                           (string-append include "/" (dirname relative)))))))
                (mkdir-p bin)
                (mkdir-p lib)
                (mkdir-p pkgconfig)
                (mkdir-p include)
                (mkdir-p site-packages)
                (install-file "bazel-bin/dreal/dreal" bin)
                (install-file "bazel-bin/libdreal.so" lib)
                (for-each
                 (lambda (header)
                   (install-relative-header "" header))
                 (find-files "dreal" "\\.(h|hpp)$"))
                (for-each
                 (lambda (root)
                   (for-each
                    (lambda (header)
                      (install-relative-header root header))
                    (find-files root "\\.(h|hpp)$")))
                 '("third_party/com_github_pinam45_dynamic_bitset/"
                   "third_party/com_github_robotlocomotion_drake/"
                   "third_party/com_github_tartanllama_optional/"))
                (install-file "bazel-bin/dreal/version.h"
                              (string-append include "/dreal"))
                (install-file "dreal/__init__.py" site-packages)
                (install-file "bazel-bin/dreal/_dreal_py.so" site-packages)
                (install-file "bazel-bin/libdreal.so" site-packages)
                (call-with-output-file (string-append pkgconfig "/dreal.pc")
                  (lambda (port)
                    (format port "prefix=~a~%" out)
                    (display "includedir=${prefix}/include\n" port)
                    (display "libdir=${prefix}/lib\n\n" port)
                    (display "Name: dReal\n" port)
                    (display "Description: SMT Solver for Nonlinear Theories\n"
                             port)
                    (format port "Version: ~a~%" #$version)
                    (display "Requires: ibex, nlopt\n" port)
                    (display "Libs: -L${libdir} -ldreal\n" port)
                    (display "Cflags: -I${includedir}\n" port))))))
          (add-after 'install 'set-rpaths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (python-version
                      #$(version-major+minor (package-version python)))
                     (site-packages
                      (string-append out "/lib/python" python-version
                                     "/site-packages/dreal"))
                     (runpath
                      (string-join
                       (list (string-append out "/lib")
                             (string-append #$(this-package-input "ibex") "/lib")
                             (string-append #$(this-package-input "clp") "/lib")
                             (string-append #$(this-package-input "bzip2") "/lib")
                             (string-append #$(this-package-input "nlopt") "/lib")
                             (string-append #$(this-package-input "gmp") "/lib")
                             (string-append
                              (assoc-ref inputs "gcc:lib") "/lib")
                             (string-append
                             (assoc-ref inputs "glibc") "/lib"))
                       ":")))
                (for-each (lambda (file)
                            (chmod file #o755))
                          (list (string-append out "/bin/dreal")
                                (string-append out "/lib/libdreal.so")
                                (string-append site-packages "/_dreal_py.so")
                                (string-append site-packages "/libdreal.so")))
                (invoke "patchelf" "--set-rpath" runpath
                        (string-append out "/bin/dreal"))
                (invoke "patchelf" "--set-rpath" runpath
                        (string-append out "/lib/libdreal.so"))
                (invoke "patchelf" "--set-rpath"
                        (string-append "$ORIGIN:" runpath)
                        (string-append site-packages "/_dreal_py.so"))
                (invoke "patchelf" "--set-rpath"
                        (string-append "$ORIGIN:" runpath)
                        (string-append site-packages "/libdreal.so")))))
          (add-after 'set-rpaths 'check-runtime
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (python-version
                      #$(version-major+minor (package-version python)))
                     (pythonpath
                      (string-append out "/lib/python" python-version
                                     "/site-packages"))
                     (smt2 (string-append (getcwd) "/guix-smoke.smt2")))
                (call-with-output-file smt2
                  (lambda (port)
                    (display "(set-logic QF_NRA)\n" port)
                    (display "(declare-fun x () Real)\n" port)
                    (display "(assert (and (>= x 0) (<= x 1)))\n" port)
                    (display "(check-sat)\n" port)))
                (invoke (string-append out "/bin/dreal") "--version")
                (invoke (string-append out "/bin/dreal") smt2)
                (setenv "PYTHONPATH" pythonpath)
                (with-directory-excursion out
                  (invoke "python3" "-c"
                          (string-append
                           "import dreal; "
                           "x = dreal.Variable('x'); "
                           "print(dreal.CheckSatisfiability("
                           "dreal.And(x >= 0, x <= 1), 0.001)); "
                           "print(dreal.__version__)")))))))))
    (native-inputs (list bison flex patchelf-0.16 pkg-config))
    (propagated-inputs (list ibex nlopt))
    (inputs `(("bash-minimal" ,bash-minimal)
              ("bzip2" ,bzip2)
              ("clp" ,clp)
              ("gcc:lib" ,gcc "lib")
              ("gmp" ,gmp)
              ("ibex" ,ibex)
              ("glibc" ,glibc)
              ("python" ,python)))
    (home-page "https://dreal.github.io/")
    (synopsis "SMT solver for nonlinear theories of real numbers")
    (description
     "dReal is an SMT solver for nonlinear theories over real numbers.  It
implements delta-complete decision procedures and includes a command-line
solver, shared library, and Python bindings.")
    (license license-gnu:asl2.0)))


;;;
;;; Scmutils.
;;;

(define-public scmutils
  (package
    (name "scmutils")
    (version "20230902")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://groups.csail.mit.edu/mac/users/gjs/6946/"
             "mechanics-system-installation/native-code/"
             "scmutils-20230902.tar.gz"))
       (sha256
        (base32 "0npc3i9dy255vb43ypkybix4w3k5ajg6npyjn8286lrgf8714d43"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (inputs (list bash-minimal mit-scheme))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (let* ((out #$output)
                     (library-dir (string-append out "/lib/mit-scheme"))
                     (scmutils-dir (string-append library-dir "/scmutils"))
                     (bin-dir (string-append out "/bin"))
                     (doc-dir (string-append out "/share/doc/scmutils")))
                (for-each
                 (lambda (file)
                   (let ((target-directory
                          (string-append scmutils-dir "/" (dirname file))))
                     (mkdir-p target-directory)
                     (install-file file target-directory)))
                 (find-files "." "\\.bci$"))
                (install-file "mechanics.com" library-dir)
                (mkdir-p bin-dir)
                (with-output-to-file (string-append bin-dir "/mechanics")
                  (lambda _
                    (format #t "#!~a/bin/sh~%"
                            #$(this-package-input "bash-minimal"))
                    (display "export MITSCHEME_HEAP_SIZE=100000\n")
                    (display "export MITSCHEME_BAND=mechanics.com\n")
                    (format
                     #t
                     (string-append
                      "export MITSCHEME_LIBRARY_PATH=\"~a"
                      "${MITSCHEME_LIBRARY_PATH:+:${MITSCHEME_LIBRARY_PATH}}"
                      "\"~%")
                     library-dir)
                    (format #t "exec ~a/bin/mit-scheme \"$@\"~%"
                            #$(this-package-input "mit-scheme"))))
                (chmod (string-append bin-dir "/mechanics") #o555)
                (symlink (string-append bin-dir "/mechanics")
                         (string-append bin-dir "/mechanics.sh"))
                (mkdir-p doc-dir)
                (for-each (lambda (file)
                            (install-file file doc-dir))
                          '("README" "INSTALL" "COPYING"))))))))
    (home-page "https://groups.csail.mit.edu/mac/users/gjs/6946/")
    (synopsis "MIT Scheme utility library for mechanics and applied math")
    (description
     "Scmutils is a collection of MIT Scheme libraries for symbolic and
numerical work in mechanics and applied mathematics.  This package installs
the precompiled Scmutils image and provides the @command{mechanics} launcher
for running it with MIT/GNU Scheme.")
    (license license-gnu:gpl2+)))


;;;
;;; Guile Scmutils.
;;;

(define-public guile-scmutils
  (package
    (name "guile-scmutils")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://www.cs.rochester.edu/~gildea/guile-scmutils/guile-scmutils-v1.1.tgz")
       (sha256
        (base32 "0nvc7nwvhfhgvr2yqr8piimlvxk18scgmbk8i9kmaj2jjzvxzfsl"))))
    (build-system gnu-build-system)
    (inputs (list bash-minimal guile-3.0 gnuplot))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (let* ((out #$output)
                     (scmutils-dir (string-append out "/share/guile-scmutils"))
                     (bin-dir (string-append out "/bin")))
                (copy-recursively "src" scmutils-dir)
                (mkdir-p bin-dir)
                (with-output-to-file (string-append bin-dir "/guile-scmutils")
                  (lambda _
                    (format #t "#!~a/bin/sh~%"
                            #$(this-package-input "bash-minimal"))
                    (format #t "export PATH=\"~a/bin${PATH:+:${PATH}}\"~%"
                            #$(this-package-input "gnuplot"))
                    (format #t "exec ~a/bin/guile -l ~a/load.scm \"$@\"~%"
                            #$(this-package-input "guile-3.0")
                            scmutils-dir)))
                (chmod (string-append bin-dir "/guile-scmutils") #o555)))))))
    (home-page "https://www.cs.rochester.edu/~gildea/guile-scmutils/")
    (synopsis "Scmutils port for Guile")
    (description
     "Guile Scmutils is a Guile port of the Scmutils symbolic mathematics
system used with the Structure and Interpretation of Classical Mechanics
materials.  This package installs the source tree and provides a
@command{guile-scmutils} launcher that preloads the system.")
    (license license-gnu:gpl2+)))


;;;
;;; MAGMA.
;;;

(define-public magma-cuda
  (package
    (name "magma-cuda")
    (version "2.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/icl-utk-edu/magma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00ypz9wg5cshqilzisizwncnvzdyidfz6fxgskf2m23avy4p8pk5"))))
    (supported-systems '("x86_64-linux"))
    (build-system cmake-build-system)
    (native-inputs (list patchelf-0.16 which python perl pkg-config))
    (inputs (list gcc cuda-toolkit openblas))
    (arguments
     (list
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-shebang
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Fix the shebang in codegen.py
              (let ((python (which "python3")))
                (substitute* "tools/codegen.py"
                  (("^#!.*python.*$")
                   (string-append "#!" python "\n"))))))
          (add-before 'configure 'copy-source
            (lambda* (#:key outputs #:allow-other-keys)
              (copy-recursively "."
                                (string-append (assoc-ref outputs "out")
                                               "/source"))))
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((openblas-dir (assoc-ref inputs "openblas"))
                     (cuda-dir (assoc-ref inputs "cuda-toolkit"))
                     (gcc (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
                     (g++ (string-append (assoc-ref inputs "gcc") "/bin/g++"))
                     (nvcc (string-append cuda-dir "/bin/nvcc")))
                (call-with-output-file "make.inc"
                  (lambda (port)
                    (format port "BACKEND = cuda\n")
                    (format port "OPENBLASDIR = ~a\n" openblas-dir)
                    (format port "CUDADIR = ~a\n" cuda-dir)
                    (format port "CC = ~a\n" gcc)
                    (format port "CXX = ~a\n" g++)
                    (format port "FORT = false\n")
                    (format port "NVCC = ~a\n" nvcc)
                    (format port "DEVCC = ~a\n\n" nvcc)
                    (format port "ARCH = ar\n")
                    (format port "ARCHFLAGS = cr\n")
                    (format port "RANLIB = ranlib\n\n")
                    (format port "GPU_TARGET = Volta Turing Ampere\n\n")
                    (format port "FPIC = -fPIC\n")
                    (format port
                     "CFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -fopenmp -std=c99
")
                    (format port
                     "CXXFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -fopenmp -std=c++11
")
                    (format port
                     "FFLAGS = -O3 $(FPIC) -DNDEBUG -Wall -Wno-unused-dummy-argument
")
                    (format port "LDFLAGS = $(FPIC) -fopenmp\n")
                    (format port "DEVCCFLAGS = -O3 -DNDEBUG \n\n")
                    (format port
                            "LIB = -lopenblas -lcublas -lcusparse -lcudart
")
                    (format port "LIBDIR = -L~a/lib -L~a/lib64\n" openblas-dir
                            cuda-dir)
                    (format port "INC = -I~a/include\n" cuda-dir))))
              (invoke "make" "generate")))
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((cuda-dir (assoc-ref inputs "cuda-toolkit")))
                (invoke "cmake"
                        "-DMAGMA_ENABLE_CUDA=ON"
                        (string-append "-DCMAKE_INSTALL_PREFIX=build/target")
                        "-DGPU_TARGET=sm_80"
                        "-DBLA_VENDOR=OpenBLAS"
                        "-DBUILD_SHARED_LIBS=ON"
                        "-DUSE_FORTRAN=OFF"
                        "."
                        "-Bbuild")
                (invoke "cmake"
                        "--build"
                        "build"
                        "-j"
                        (number->string (parallel-job-count))
                        "--target"
                        "install"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (guix build utils))
              (let ((out (assoc-ref outputs "out")))
                (copy-recursively "build/target/include"
                                  (string-append out "/include"))
                (install-file "build/target/lib/libmagma.so"
                              (string-append out "/lib"))
                (install-file "build/target/lib/libmagma_sparse.so"
                              (string-append out "/lib")))))
          (add-after 'install 'create-pkgconfig
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (cuda-dir (assoc-ref inputs "cuda-toolkit"))
                     (pc-dir (string-append out "/lib/pkgconfig"))
                     (pc-file (string-append pc-dir "/magma.pc"))
                     (cflags (string-join (list "Cflags:"
                                                "-I${includedir}"
                                                "-std=c++11"
                                                "-fopenmp"
                                                "-Wall -Wno-unused-function"
                                                (string-append "-I" cuda-dir
                                                               "/include")
                                                (string-append "-I" out
                                                               "/include")
                                                (string-append "-I" out
                                                 "/source/control")
                                                (string-append "-I" out
                                                 "/source/magmablas")
                                                (string-append "-I" out
                                                 "/source/sparse/include")
                                                (string-append "-I" out
                                                 "/source/sparse/control")
                                                (string-append "-I" out
                                                 "/source/testing\n")) " "))
                     (libs (string-join (list "Libs:"
                                              "-L${libdir}"
                                              "-lmagma_sparse"
                                              "-lmagma"
                                              "-llapack"
                                              "-lblas"
                                              (string-append "-L" cuda-dir
                                                             "/lib")
                                              "-lcudart"
                                              "-lcublas"
                                              "-lcusparse") " ")))
                (mkdir-p pc-dir)
                (call-with-output-file pc-file
                  (lambda (port)
                    (format port "prefix=~a\n" out)
                    (format port "exec_prefix=${prefix}\n")
                    (format port "libdir=${exec_prefix}/lib\n")
                    (format port "includedir=${prefix}/include\n\n")
                    (format port "Name: magma\n")
                    (format port
                     "Description: Matrix Algebra on GPU and Multicore Architectures
")
                    (format port
                            (string-append "Version: "
                                           #$(package-version this-package)
                                           "\n"))
                    (format port cflags)
                    (format port libs))))))
          (add-after 'create-pkgconfig 'set-rpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((libdir (string-append (assoc-ref outputs "out") "/lib"))
                     (libmagma-so (string-append libdir "/libmagma.so"))
                     (libmagma-sparse-so (string-append libdir
                                                        "/libmagma_sparse.so")))
                (define libmagma-runpaths
                  (string-join (list (string-append (assoc-ref inputs "libc")
                                                    "/lib")
                                     (string-append (assoc-ref inputs "gcc")
                                                    "/lib")
                                     (string-append (assoc-ref inputs
                                                               "cuda-toolkit")
                                                    "/lib")
                                     (string-append (assoc-ref inputs
                                                               "cuda-toolkit")
                                                    "/lib64")) ":"))
                (define libmagma-sparse-runpaths
                  (string-join (list libmagma-runpaths libdir) ":"))
                (make-file-writable libmagma-so)
                (format #t "Setting RPATH on '~a'...~%" libmagma-so)
                (invoke "patchelf" "--set-rpath" libmagma-runpaths
                        "--force-rpath" libmagma-so)
                (make-file-writable libmagma-sparse-so)
                (format #t "Setting RPATH on '~a'...~%" libmagma-sparse-so)
                (invoke "patchelf" "--set-rpath" libmagma-sparse-runpaths
                        "--force-rpath" libmagma-sparse-so))))
          (add-after 'set-rpath 'add-copyright
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (copyright (string-append out "/source/COPYRIGHT")))
                (install-file copyright
                              (string-append out "/share")))))
          (delete 'check))))
    (synopsis "MAGMA linear algebra library with CUDA support")
    (description
     "MAGMA is a high-performance linear algebra library that uses GPUs for
optimized calculations. It supports the CUDA backend and is compatible with multiple
GPU architectures.")
    (home-page "https://bitbucket.org/icl/magma/")
    (license license-gnu:bsd-3)))


;;;
;;; oneAPI MKL.
;;;

;; Work In Progress
;; References:
;; https://gitlab.archlinux.org/archlinux/packaging/packages/intel-oneapi-mkl
;; https://gitlab.archlinux.org/archlinux/packaging/packages/intel-oneapi-basekit
;; https://gitlab.archlinux.org/archlinux/packaging/packages/magma

(define (intel-url subpackage version superversion debversion suffix)
  (string-append "https://apt.repos.intel.com/oneapi/pool/main/intel-oneapi-"
                 subpackage
                 "-"
                 version
                 "-"
                 superversion
                 "-"
                 debversion
                 "_"
                 suffix
                 ".deb"))

(define (make-intel-oneapi subpackage
                           version
                           superversion
                           debversion
                           suffix
                           hash)
  (package
    (name (string-append "intel-oneapi-" subpackage))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (intel-url subpackage version superversion debversion suffix))
       (sha256
        hash)))
    (native-inputs (list tar gzip cpio))
    (build-system gnu-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (add-before 'install 'extract-deb
            (lambda _
              (for-each (lambda (deb)
                          (format #t "extracting ~a...~%" deb)
                          (let* ((command (string-append "ar x "
                                           deb
                                           " && tar xf data.tar.xz && rm data.tar.xz"
                                           " && rm control.tar.xz && rm -rf debian-binary"
                                           " && rm "
                                           deb))
                                 (status (system command)))
                            (unless (zero? status)
                              (error (format #f "command '~a' failed with ~a"
                                             command status)))))
                        (find-files "." "\\.deb$"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define out
                (assoc-ref outputs "out"))
              (define source-root
                (string-append "opt/intel/oneapi/mkl/"
                               #$(package-version this-package)))
              (when (directory-exists? source-root)
                (copy-recursively source-root out)
                (delete-file-recursively source-root))
              (copy-recursively "." out))))))
    (home-page "https://software.intel.com/en-us/mkl")
    (synopsis "Intel oneAPI Math Kernel Library")
    (description
     "Intel® Math Kernel Library (MKL) is a proprietary library of
highly optimized, extensively threaded routines for applications that
require maximum performance.")
    (license (license:nonfree (string-append
                               "https://www.intel.com/content/www/us/en/developer/articles/license/"
                               "end-user-license-agreement.html")))))

(define-public intel-oneapi-mkl-classic
  (make-intel-oneapi "mkl-classic"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "07cmn9x1frfql0blghmqhbx28pj4aw7v4022hhcrhj584kdcq5pl")))

(define-public intel-oneapi-mkl-classic-include
  (make-intel-oneapi "mkl-classic-include"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "090sjzkvicxinj26rpp75zcpf3vn3rsbmz4xp0rv7njfi5441lja")))

(define-public intel-oneapi-mkl-classic-include-common
  (make-intel-oneapi "mkl-classic-include-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "08g40zab9wdwaqmjsdafqgqp78l8jg7zbfshavlp50fxl7cw05kh")))

(define-public intel-oneapi-mkl-core
  (make-intel-oneapi "mkl-core"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "13xhp025x956zfwq5c25lga04wddjydyvl0ymngq1d6f4iz21i1f")))

(define intel-oneapi-mkl-core-common
  (make-intel-oneapi "mkl-core-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "0w7hflyppn9pv825bjf902p0g1v4s60bc50lmxj15hcsiwd0cnla")))

(define intel-oneapi-mkl-core-devel
  (make-intel-oneapi "mkl-core-devel"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1z4sxw28nkjwfqiqmhfmxn6lz44diy94r0rcgdr3iykw1gyc1qrv")))

(define intel-oneapi-mkl-core-devel-common
  (make-intel-oneapi "mkl-core-devel-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "194xabclwhl19zvn9nlxdx518xx9gwm5pn1hlm7s1x4wi3g93598")))

(define intel-oneapi-mkl-cluster
  (make-intel-oneapi "mkl-cluster"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "0lzzhf91w4qjgkv1gvxlw7hdn9pakij9h4688ndvz6jws3wvrfjw")))

(define intel-oneapi-mkl-cluster-devel
  (make-intel-oneapi "mkl-cluster-devel"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "0fc19c1q73gf0sxcl0rb6i44jnllkfq5ivn3v9jp351aamjqdl3f")))

(define intel-oneapi-mkl-cluster-devel-common
  (make-intel-oneapi "mkl-cluster-devel-common"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "all"
                     (base32
                      "0n00k4iiayshj5rjmh8azzrjbnj2k9jz151sxp5m4j63scrv8386")))

(define intel-oneapi-mkl-sycl
  (make-intel-oneapi "mkl-sycl"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "18yrgksqm1p7bg148wn5mm0vxh5qvivnqd25mbvywbfpw78b6kiz")))

(define intel-oneapi-mkl-sycl-blas
  (make-intel-oneapi "mkl-sycl-blas"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "11dalggxrg7wglcf88n1sfkcmz2r5d2fqd88j5x9mghxdcpv9nkc")))

(define intel-oneapi-mkl-sycl-lapack
  (make-intel-oneapi "mkl-sycl-lapack"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1hgbr3n90x3f3a2xgwhicyllcg3z2623kswhscgfys4ln5wzj6hz")))

(define intel-oneapi-mkl-sycl-dft
  (make-intel-oneapi "mkl-sycl-dft"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1zsq2kywpaqw933xxmqicgp03bxwkhph21brljpc96rrlax106sz")))

(define intel-oneapi-mkl-sycl-sparse
  (make-intel-oneapi "mkl-sycl-sparse"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1mvj95z4k72ipsj1wj9bp439q3lgqbwwg64clcggrca74b8bjyvx")))

(define intel-oneapi-mkl-sycl-data-fitting
  (make-intel-oneapi "mkl-sycl-data-fitting"
                     "2024.1"
                     "2024.1.0"
                     "691"
                     "amd64"
                     (base32
                      "1ci8wdbccd0bwdnd0z7gmgyxh0hibv1ig3alwmhh9pkq2rv33ny5")))
