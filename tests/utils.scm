;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((myguix utils) #:select (with-transformation/myguix))
             (gnu home)
             (gnu packages base)
             (gnu services)
             (gnu system)
             (gnu system install)
             (guix download)
             (guix packages)
             (srfi srfi-64))

(define (mark pkg)
  (package
    (inherit pkg)
    (name (string-append (package-name pkg) "-transformed"))))

(define (transform object)
  (with-transformation/myguix mark object))

(test-begin "myguix-utils")

(test-equal "package" "hello-transformed"
  (package-name (transform hello)))
(test-equal "ordinary values are preserved" '("text" 23 #f)
  (transform '("text" 23 #f)))
(test-equal "improper pairs" "hello-transformed"
  (package-name (cdr (transform (cons 'package hello)))))
(test-equal "lists" '("hello-transformed" "hello-transformed")
  (map package-name (transform (list hello hello))))
(test-equal "vectors" "hello-transformed"
  (package-name (vector-ref (transform (vector hello)) 0)))
(test-equal "custom predicates" '#(2 "text")
  (with-transformation/myguix (lambda (number) (+ number 1))
                              '#(1 "text") number?))
(test-equal "procedures preserve multiple values"
  '("hello-transformed" "unchanged")
  (call-with-values
      (transform (lambda () (values hello "unchanged")))
    (lambda (package text) (list (package-name package) text))))
(test-eq "service types are not rewritten" profile-service-type
  (transform profile-service-type))
(let ((source (origin
                (method url-fetch)
                (uri "https://example.org/source.tar.gz")
                (sha256 (base32
                         "0000000000000000000000000000000000000000000000000000")))))
  (test-eq "origins are not rewritten" source (transform source)))

(let* ((home (home-environment (packages (list hello))))
       (result (transform home))
       (field (module-variable (resolve-interface '(gnu home))
                               'home-environment-transformation)))
  (test-equal "home locations are preserved"
    (home-environment-location home) (home-environment-location result))
  (if field
      (let ((apply-transformation
             ((eval '(lambda (home) (home-environment-transformation home))
                    (resolve-module '(gnu home)))
              result)))
        (test-eq "deferred homes preserve package fields" hello
          (car (home-environment-packages result)))
        (test-equal "deferred home transformation" "hello-transformed"
          (package-name (apply-transformation hello)))
        (test-equal "deferred home predicates" "unchanged"
          (apply-transformation "unchanged"))
        (test-equal "deferred transformations compose"
          "hello-transformed-transformed"
          (package-name
           (((eval '(lambda (home) (home-environment-transformation home))
                   (resolve-module '(gnu home)))
             (transform result))
            hello))))
      (test-equal "older homes transform packages directly" "hello-transformed"
        (package-name (car (home-environment-packages result))))))

(let* ((os (operating-system
            (inherit installation-os)
            (host-name "transformation-test")))
       (result (transform os))
       (field (module-variable (resolve-interface '(gnu system))
                               'operating-system-transformation)))
  (test-equal "system locations are preserved"
    (operating-system-location os) (operating-system-location result))
  (test-equal "system host names are preserved"
    (operating-system-host-name os) (operating-system-host-name result))
  (when field
    (let ((apply-transformation
           ((eval '(lambda (os) (operating-system-transformation os))
                  (resolve-module '(gnu system)))
            result)))
      (test-equal "deferred system transformation" "hello-transformed"
        (package-name (apply-transformation hello)))
      (test-equal "deferred system predicates" "unchanged"
        (apply-transformation "unchanged")))))

(define failures (test-runner-fail-count (test-runner-current)))
(test-end "myguix-utils")
(exit (if (zero? failures) 0 1))
