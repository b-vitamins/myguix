(define-module (myguix packages rust)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages web)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages libffi)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (myguix packages llvm-pqrs)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define* (rust-uri version #:key (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (origin
       (inherit (package-source base-rust))
       (uri (rust-uri version))
       (sha256 (base32 checksum))))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define (make-ignore-test-list strs)
  "Function to make creating a list to ignore tests a bit easier."
  (map (lambda (str)
    `((,str) (string-append "#[ignore]\n" ,str)))
    strs))

(define-public rust-1.76
  (let ((base-rust (rust-bootstrapped-package rust-1.75 "1.76.0"
                                              "08f06shp6l72qrv5fwg1is7yzr6kwj8av0l9h5k243bz781zyp4y")))
    (package
      (inherit base-rust)
      (properties (append
                    (alist-delete 'hidden? (package-properties base-rust))
                    (clang-compiler-cpu-architectures "17")))
      (outputs (cons* "rust-src" "tools" (package-outputs base-rust)))
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("vendor/openssl-src/openssl"
                         ;; "vendor/tikv-jemalloc-sys/jemalloc"
                         ;; These are referenced by the cargo output
                         ;; so we unbundle them.
                         "vendor/curl-sys/curl"
                         "vendor/curl-sys-0.4.63+curl-8.1.2/curl"
                         "vendor/libffi-sys/libffi"
                         "vendor/libnghttp2-sys/nghttp2"
                         "vendor/libz-sys/src/zlib"))
             ;; Use the packaged nghttp2
             (delete-file "vendor/libnghttp2-sys/build.rs")
             (with-output-to-file "vendor/libnghttp2-sys/build.rs"
               (lambda _
                 (format #t "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* "vendor/tempfile/Cargo.toml"
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\"")))))))
      (arguments
       (substitute-keyword-arguments
         (strip-keyword-arguments '(#:tests?)
           (package-arguments base-rust))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'relax-gdb-auto-load-safe-path
               ;; Allow GDB to load binaries from any location, otherwise the
               ;; gdbinfo tests fail.  This is only useful when testing with a
               ;; GDB version newer than 8.2.
               (lambda _
                 (setenv "HOME" (getcwd))
                 (with-output-to-file (string-append (getenv "HOME") "/.gdbinit")
                   (lambda _
                     (format #t "set auto-load safe-path /~%")))
                 ;; Do not launch gdb with '-nx' which causes it to not execute
                 ;; any init file.
                 (substitute* "src/tools/compiletest/src/runtest.rs"
                   (("\"-nx\".as_ref\\(\\), ")
                    ""))))
             (add-after 'unpack 'disable-tests-requiring-git
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/git.rs"
                   ,@(make-ignore-test-list
                      '("fn fetch_downloads_with_git2_first_")))))
             (add-after 'unpack 'disable-tests-requiring-mercurial
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/cargo_init"
                   (substitute* '("mercurial_autodetect/mod.rs"
                                  "simple_hg_ignore_exists/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case"))))))
             (add-after 'unpack 'disable-tests-using-cargo-publish
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite"
                   (substitute* "alt_registry.rs"
                     ,@(make-ignore-test-list
                        '("fn warn_for_unused_fields")))
                   (substitute* '("cargo_add/locked_unchanged/mod.rs"
                                  "cargo_add/lockfile_updated/mod.rs"
                                  "cargo_remove/update_lock_file/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case")))
                   (substitute* "git_shallow.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_git_dependency_with_shallow_protocol_and_git2_is_used_for_followup_fetches"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_aborts_and_updates_again"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_fetch_maintains_shallowness"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_with_git2_fetch"
                          "fn gitoxide_clones_registry_without_shallow_protocol_and_follow_up_fetch_uses_shallowness"
                          "fn gitoxide_shallow_clone_followed_by_non_shallow_update"
                          "fn gitoxide_clones_shallow_two_revs_same_deps"
                          "fn gitoxide_git_dependencies_switch_from_branch_to_rev"
                          "fn shallow_deps_work_with_revisions_and_branches_mixed_on_same_dependency")))
                   (substitute* "install.rs"
                     ,@(make-ignore-test-list
                        '("fn failed_install_retains_temp_directory")))
                   (substitute* "offline.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_cargo_compile_offline_with_cached_git_dep_shallow_dep")))
                   (substitute* "patch.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_shallow_old_git_patch"))))))
             ,@(if (target-riscv64?)
                   ;; Keep this phase separate so it can be adjusted without needing
                   ;; to adjust the skipped tests on other architectures.
                   `((add-after 'unpack 'disable-tests-broken-on-riscv64
                       (lambda _
                         (with-directory-excursion "src/tools/cargo/tests/testsuite"
                           (substitute* "build.rs"
                             ,@(make-ignore-test-list
                                 '("fn uplift_dwp_of_bin_on_linux")))
                           (substitute* "cache_lock.rs"
                             ,@(make-ignore-test-list
                                 '("fn multiple_download")))))))
                   `())
             (add-after 'unpack 'disable-tests-broken-on-aarch64
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/"
                   (substitute* "build_script_extra_link_arg.rs"
                     ,@(make-ignore-test-list
                        '("fn build_script_extra_link_arg_bin_single")))
                   (substitute* "build_script.rs"
                     ,@(make-ignore-test-list
                        '("fn env_test")))
                   (substitute* "collisions.rs"
                     ,@(make-ignore-test-list
                        '("fn collision_doc_profile_split")))
                   (substitute* "concurrent.rs"
                     ,@(make-ignore-test-list
                        '("fn no_deadlock_with_git_dependencies")))
                   (substitute* "features2.rs"
                     ,@(make-ignore-test-list
                        '("fn dep_with_optional_host_deps_activated"))))))
             (add-after 'unpack 'patch-command-exec-tests
               ;; This test suite includes some tests that the stdlib's
               ;; `Command` execution properly handles in situations where
               ;; the environment or PATH variable are empty, but this fails
               ;; since we don't have `echo` available at its usual FHS
               ;; location.
               (lambda _
                 (substitute* "tests/ui/command/command-exec.rs"
                   (("Command::new\\(\"echo\"\\)")
                    (format #f "Command::new(~s)" (which "echo"))))))
             (add-after 'unpack 'patch-command-uid-gid-test
               (lambda _
                 (substitute* "tests/ui/command/command-uid-gid.rs"
                   (("/bin/sh") (which "sh"))
                   (("/bin/ls") (which "ls")))))
             (add-after 'unpack 'patch-jemalloc-sys
               (lambda _
                 (substitute* "vendor/jemalloc-sys/jemalloc/Makefile.in"
                   (("/bin/sh") (which "sh")))))
             (add-after 'unpack 'skip-shebang-tests
               ;; This test make sure that the parser behaves properly when a
               ;; source file starts with a shebang. Unfortunately, the
               ;; patch-shebangs phase changes the meaning of these edge-cases.
               ;; We skip the test since it's drastically unlikely Guix's
               ;; packaging will introduce a bug here.
               (lambda _
                 (delete-file "tests/ui/parser/shebang/sneaky-attrib.rs")))
             (add-after 'unpack 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (with-directory-excursion "library/std/src"
                     (substitute* "process/tests.rs"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))
                     ;; The three tests which are known to fail upstream on QEMU
                     ;; emulation on aarch64 and riscv64 also fail on x86_64 in
                     ;; Guix's build system.  Skip them on all builds.
                     (substitute* "sys/unix/process/process_common/tests.rs"
                       ;; We can't use make-ignore-test-list because we will get
                       ;; build errors due to the double [ignore] block.
                       (("target_arch = \"arm\"" arm)
                        (string-append "target_os = \"linux\",\n"
                                       "        " arm)))))))
             (add-after 'unpack 'disable-interrupt-tests
               (lambda _
                 ;; This test hangs in the build container; disable it.
                 (substitute* "src/tools/cargo/tests/testsuite/freshness.rs"
                   ,@(make-ignore-test-list
                      '("fn linking_interrupted")))
                 ;; Likewise for the ctrl_c_kills_everyone test.
                 (substitute* "src/tools/cargo/tests/testsuite/death.rs"
                   ,@(make-ignore-test-list
                      '("fn ctrl_c_kills_everyone")))))
             (add-after 'unpack 'adjust-rpath-values
               ;; This adds %output:out to rpath, allowing us to install utilities in
               ;; different outputs while reusing the shared libraries.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "src/bootstrap/src/core/builder.rs"
                     ((" = rpath.*" all)
                      (string-append all
                                     "                "
                                     "rustflags.arg(\"-Clink-args=-Wl,-rpath="
                                     out "/lib\");\n"))))))
             (add-after 'configure 'customize-config
                        (lambda* (#:key inputs #:allow-other-keys)
                          ;; Assuming `gdb` is part of your inputs and you want to keep the gdb example
                          (let ((gdb (assoc-ref inputs "gdb"))
                                (clang (assoc-ref inputs "clang"))
                                (llvm (assoc-ref inputs "llvm")))
                            (substitute* "config.toml"
                                         (("^python =.*$" all)
                                          (string-append all 
                                                         "gdb = \"" gdb "/bin/gdb\"\n"))
                                         (("^\\[llvm\\].*$" all)
                                          (string-append all "thin-lto = true\n"))
                                         (("^\\[llvm\\].*$" all)
                                          (string-append all "plugins = true\n"))
                                         (("^\\[llvm\\].*$" all)
                                          (string-append all "polly = true\n"))
                                         (("^\\[build\\].*$" all)
                                          (string-append all "profiler = true\n"))
                                         (("^\\[build\\].*$" all)
                                          (string-append all "extended = true\n"))
                                         (("docs = false") "docs = true")
                                         (("jemalloc=false") "jemalloc = true")
                                         (("^cc =.*$" all)
                                          (string-append "cc = \"" clang "/bin/clang\"\n"))
                                         (("^cxx =.*$" all)
                                          (string-append "cxx = \"" clang "/bin/clang++\"\n"))
                                         (("^ar =.*$" all)
                                          (string-append "ar = \"" llvm "/bin/llvm-ar\"\n"))
                                         (("^ar =.*$" all)
                                          (string-append all "ranlib = \"" llvm "/bin/llvm-ranlib\"\n"))))))
             (replace 'build
               ;; Phase overridden to also build more tools.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std"    ;rustc
                           "src/tools/cargo"
                           "src/tools/clippy"
                           "src/tools/rust-analyzer"
                           "src/tools/rustfmt"))))
             (replace 'check
               ;; Phase overridden to also test more tools.
               (lambda* (#:key tests? parallel-build? #:allow-other-keys)
                 (when tests?
                   (let ((job-spec (string-append
                                    "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1"))))
                     (invoke "./x.py" job-spec "test" "-vv"
                           "library/std"    ;rustc
                           "src/tools/cargo"
                           "src/tools/clippy"
                           "src/tools/rust-analyzer"
                           "src/tools/rustfmt")))))
             (replace 'install
               ;; Phase overridden to also install more tools.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'tools' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "tools"))))
                 (invoke "./x.py" "install" "clippy")
                 (invoke "./x.py" "install" "rust-analyzer")
                 (invoke "./x.py" "install" "rustfmt")))
             (add-after 'install 'install-rust-src
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "rust-src"))
                       (dest "/lib/rustlib/src/rust"))
                   (mkdir-p (string-append out dest))
                   (copy-recursively "library" (string-append out dest "/library"))
                   (copy-recursively "src" (string-append out dest "/src"))
                   (delete-file-recursively (string-append out dest "/src/llvm-project")))))
             (add-after 'install 'remove-uninstall-script
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; This script has no use on Guix
                 ;; and it retains a reference to the host's bash.
                 (delete-file (string-append (assoc-ref outputs "out")
                                             "/lib/rustlib/uninstall.sh"))))
             (add-after 'install-rust-src 'wrap-rust-analyzer
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((bin (string-append (assoc-ref outputs "tools") "/bin")))
                   (rename-file (string-append bin "/rust-analyzer")
                                (string-append bin "/.rust-analyzer-real"))
                   (call-with-output-file (string-append bin "/rust-analyzer")
                     (lambda (port)
                       (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                               (which "bash")
                               (string-append (assoc-ref outputs "rust-src")
                                              "/lib/rustlib/src/rust/library")
                               (string-append bin "/.rust-analyzer-real"))))
                   (chmod (string-append bin "/rust-analyzer") #o755))))))))
      (inputs (modify-inputs (package-inputs base-rust)
                             (replace "llvm" llvm-with-bolt-17)
                             (prepend curl libffi `(,nghttp2 "lib") zlib jemalloc ninja)))
      (native-inputs (cons* `("gdb" ,gdb/pinned)
                            `("procps" ,procps)
                            `("clang" ,clang-17)
                            `("cmake" ,cmake)
                            (package-native-inputs base-rust))))))

(define-public rust
  (let ((base-rust (rust-bootstrapped-package rust-1.76 "1.77.0"
                                              "11rda8d8qj24a5mkjzj1x6x9pkvaq0zlhkgdp5b39zj5m0gwsv0d")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
               (replace 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (with-directory-excursion "library/std/src"
                     (substitute* "process/tests.rs"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))
                     (substitute* "sys/pal/unix/process/process_common/tests.rs"
                       ;; We can't use make-ignore-test-list because we will get
                       ;; build errors due to the double [ignore] block.
                       (("target_arch = \"arm\"" arm)
                        (string-append "target_os = \"linux\",\n"
                                       "        " arm))))))))))))))