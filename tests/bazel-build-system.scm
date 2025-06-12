(use-modules (srfi srfi-64)
             (myguix build bazel-build-system)
             (guix build utils)
             (guix utils))

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

(test-end)
