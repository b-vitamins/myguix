(use-modules (srfi srfi-64)
             (myguix build bazel-build-system)
             (guix build utils)
             (guix utils)
             (ice-9 textual-ports))

(test-begin "bazel build system")

(test-assert "unpack sets HOME"
  (call-with-temporary-directory
   (lambda (tmp)
    (let* ((tar (string-append tmp "/deps.tar"))
           (external-dir (string-append tmp "/external")))
      (mkdir-p external-dir)
      (call-with-output-file (string-append external-dir "/foo") (lambda (_) (display "")))
      (invoke "tar" "cf" tar "-C" tmp "external")
      (setenv "NIX_BUILD_TOP" tmp)
      (unpack-vendored-inputs #:vendored-inputs tar)
      (string=? (getenv "HOME") tmp)))))

(test-assert "build uses server mode"
  (call-with-temporary-directory
   (lambda (tmp)
     (let* ((bin (string-append tmp "/bin"))
            (args-file (string-append tmp "/args"))
            (bazel (string-append bin "/bazel")))
       (mkdir-p bin)
       (call-with-output-file bazel
         (lambda (port)
           (format port "#!~a\n" (which "sh"))
           (format port "echo \"$@\" > $ARGS_FILE\n")))
       (chmod bazel #o755)
       (setenv "PATH" (string-append bin ":" (getenv "PATH")))
       (setenv "ARGS_FILE" args-file)
       (setenv "NIX_BUILD_TOP" tmp)
       ((module-ref (resolve-module '(myguix build bazel-build-system)) 'build)
        #:parallel-build? #f
        #:bazel-arguments '()
        #:build-targets '("//:foo"))
       (let ((content (call-with-input-file args-file get-string-all)))
         (and (not (string-contains content "--batch"))
             (string-contains content "--max_idle_secs=1")))))))

(test-assert "custom bazelrc is honored"
  (call-with-temporary-directory
   (lambda (tmp)
     (let* ((bin (string-append tmp "/bin"))
            (args-file (string-append tmp "/args"))
            (rc-file (string-append tmp "/my.bazelrc"))
            (bazel (string-append bin "/bazel")))
       (mkdir-p bin)
       (call-with-output-file bazel
         (lambda (port)
           (format port "#!~a\n" (which "sh"))
           (format port "echo \"$@\" > $ARGS_FILE\n")))
       (call-with-output-file rc-file (const #t))
       (chmod bazel #o755)
       (setenv "PATH" (string-append bin ":" (getenv "PATH")))
       (setenv "ARGS_FILE" args-file)
       (setenv "NIX_BUILD_TOP" tmp)
       ((module-ref (resolve-module '(myguix build bazel-build-system)) 'build)
        #:parallel-build? #f
        #:bazel-arguments '()
        #:bazelrc rc-file
        #:build-targets '("//:foo"))
       (string-contains (call-with-input-file args-file get-string-all)
                       rc-file)))))

(test-end)
