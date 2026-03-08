(define-module (tests borg-backup)
  #:use-module (gnu services shepherd)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (myguix home services backup)
  #:use-module (myguix services backup)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:export (run-tests))

(define %test-file
  (or (current-filename)
      (error "current-filename is unavailable")))

(define %guile-executable
  (canonicalize-path "/proc/self/exe"))

(define %backup-module
  (resolve-module '(myguix services backup)))

(define %effective-archive-name-format
  (module-ref %backup-module
              'borg-backup-job-effective-archive-name-format))

(define %effective-archive-match-pattern
  (module-ref %backup-module
              'borg-backup-job-effective-archive-match-pattern))

(define %check-service-constructor
  (module-ref %backup-module
              'borg-check-job->shepherd-service))

(define %validated-borg-backup-jobs
  (module-ref %backup-module
              'validated-borg-backup-jobs))

(define (capture command . arguments)
  (let* ((port (apply open-pipe* OPEN_READ command arguments))
         (output (get-string-all port))
         (status (close-pipe port)))
    (values status output)))

(define (capture-lines command . arguments)
  (call-with-values (lambda ()
                      (apply capture command arguments))
                    (lambda (status output)
                      (values status
                              (filter (lambda (line)
                                        (not (string-null? line)))
                                      (string-split (string-trim-right output)
                                                    #\newline))))))

(define (mktemp-directory)
  (mkdtemp! (string-copy (string-append (or (getenv "TMPDIR")
                                            "/tmp")
                                        "/myguix-test-XXXXXX"))))

(define (write-text-file file text)
  (call-with-output-file file
    (lambda (port)
      (display text port))))

(define (read-text-file file)
  (call-with-input-file file
    get-string-all))

(define (evaluate-expression expression)
  (call-with-input-string expression
    (lambda (port)
      (eval (read port) (current-module)))))

(define (build-object object)
  (let ((store (open-connection)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (let ((drv (run-with-store store
                                   (lower-object object))))
          (build-derivations store (list drv))
          (derivation->output-path drv)))
      (lambda ()
        (close-connection store)))))

(define (build-expression expression)
  (build-object (evaluate-expression expression)))

(define (build-job-program expression)
  (build-expression (string-append "(begin\n"
                                   "  (use-modules (myguix services backup))
"
                                   "  (define job\n"
                                   expression
                                   ")\n"
                                   "  (borg-backup-job-program job))\n")))

(define (build-wrapper-program expression)
  (build-expression (string-append "(begin\n"
                                   "  (use-modules (myguix services backup))
"
                                   "  (borg-guix (list\n" expression "))\n)\n")))

(define (archive-names program repository)
  (call-with-values (lambda ()
                      (capture-lines program "borg" "list" repository))
                    (lambda (status lines)
                      (unless (zero? status)
                        (error "borg list failed" repository))
                      (map (lambda (line)
                             (car (string-tokenize line)))
                           (filter (lambda (line)
                                     (not (string-prefix? "Running " line)))
                                   lines)))))

(define (archive-contents program repository archive)
  (call-with-values (lambda ()
                      (capture program "borg" "list"
                               (string-append repository "::"
                                              archive)))
                    (lambda (status output)
                      (unless (zero? status)
                        (error "borg list archive failed" archive)) output)))

(define (extract-archive program repository archive directory)
  (with-directory-excursion directory
    (system* program "borg" "extract"
             (string-append repository "::" archive))))

(define (contains-substring? text needle)
  (and (string-contains text needle) #t))

(define (running-as-script?)
  (match (command-line)
    ((file)
     (and (string? file)
          (false-if-exception
           (string=? (canonicalize-path file) %test-file))))
    (_ #f)))

(define (run-tests)
(test-begin "borg-backup")

(let ((job (borg-backup-job (name "documents")
                            (repository "/tmp/repository")
                            (passphrase-file "/tmp/passphrase")
                            (schedule "0 3 * * *")
                            (paths '("/home/b/Documents")))))
  (test-assert "lower-borg-backup-job returns a gexp"
               (gexp? (lower-borg-backup-job job)))
  (test-equal "default archive naming is job-scoped"
              "{hostname}-{user}-documents-{now:%Y-%m-%dT%H:%M:%S}"
              (%effective-archive-name-format job))
  (test-equal "default prune/check matching is job-scoped"
              "{hostname}-{user}-documents-*"
              (%effective-archive-match-pattern job)))

(let ((job (borg-backup-job (name "documents")
                            (repository "/tmp/repository")
                            (passphrase-file "/tmp/passphrase")
                            (schedule "0 3 * * *")
                            (check-schedule "30 4 * * 0")
                            (paths '("/home/b/Documents")))))
  (test-assert "backup shepherd service is constructed"
               (shepherd-service? (borg-backup-job->shepherd-service job)))
  (test-assert
   "check shepherd service is constructed when check-schedule is set"
   (shepherd-service? (%check-service-constructor job)))
  (test-assert "profile package is added when jobs exist"
               (= 1
                  (length (borg-backup-service-profile (borg-backup-configuration
                                                        (jobs (list job))))))))

(test-assert "duplicate job names are rejected"
             (guard (condition (#t #t))
                    (%validated-borg-backup-jobs (borg-backup-configuration (jobs
                                                                             (list
                                                                              (borg-backup-job
                                                                               (name
                                                                                "dup")
                                                                               
                                                                               (repository
                                                                                "/tmp/repo-1")
                                                                               
                                                                               (passphrase-file
                                                                                "/tmp/pass-1")
                                                                               
                                                                               (schedule
                                                                                "0 0 * * *")
                                                                               
                                                                               (paths '
                                                                                ("/tmp/a")))
                                                                              
                                                                              (borg-backup-job
                                                                               (name
                                                                                "dup")
                                                                               
                                                                               (repository
                                                                                "/tmp/repo-2")
                                                                               
                                                                               (passphrase-file
                                                                                "/tmp/pass-2")
                                                                               
                                                                               (schedule
                                                                                "0 0 * * *")
                                                                               
                                                                               (paths '
                                                                                ("/tmp/b")))))))
                    #f))

(test-assert "home backup module exports a service type"
             home-borg-backup-service-type)

(let* ((directory (mktemp-directory))
       (source (string-append directory "/source"))
       (cache (string-append source "/.cache"))
       (extract (string-append directory "/extract"))
       (passphrase-file (string-append directory "/passphrase"))
       (repository (string-append directory "/repo"))
       (passphrase "test-passphrase")
       (job-program (build-job-program (format #f
                                        "(borg-backup-job
  (name ~s)
  (repository ~s)
  (passphrase-file ~s)
  (initialize-repository? #t)
  (schedule ~s)
  (paths '~s)
  (exclude-patterns '~s)
  (prune-keep-daily 1)
  (compact-threshold 0)
  (verbose? #t))"
                                        "passfile"
                                        repository
                                        passphrase-file
                                        "0 0 * * *"
                                        (list source)
                                        (list "sh:**/.cache/**"))))
       (partial-check-program (build-job-program (format #f
                                                  "(borg-backup-job
  (name ~s)
  (repository ~s)
  (passphrase-file ~s)
  (schedule ~s)
  (paths '~s)
  (check-repository? #t)
  (check-archives? #f)
  (check-max-duration 1)
  (verbose? #t))"
                                                  "passfile"
                                                  repository
                                                  passphrase-file
                                                  "0 0 * * *"
                                                  (list source))))
       (archive-check-program (build-job-program (format #f
                                                  "(borg-backup-job
  (name ~s)
  (repository ~s)
  (passphrase-file ~s)
  (schedule ~s)
  (paths '~s)
  (check-repository? #f)
  (check-archives? #t)
  (check-verify-data? #t)
  (verbose? #t))"
                                                  "passfile"
                                                  repository
                                                  passphrase-file
                                                  "0 0 * * *"
                                                  (list source)))))
  (dynamic-wind (lambda ()
                  (mkdir-p cache)
                  (mkdir-p extract)
                  (write-text-file passphrase-file
                                   (string-append passphrase "\n"))
                  (write-text-file (string-append source "/keep.txt")
                                   "first version\n")
                  (write-text-file (string-append cache "/skip.txt")
                                   "skip me\n"))
                (lambda ()
                  (call-with-values (lambda ()
                                      (capture job-program))
                                    (lambda (status output)
                                      (test-equal
                                       "passphrase-file job program initial backup succeeds"
                                       0 status)
                                      (test-assert
                                       "initial backup logs repository init"
                                       (contains-substring? output
                                        "init --encryption repokey"))))

                  (let* ((archives (archive-names job-program repository))
                         (archive (car archives))
                         (contents (archive-contents job-program repository
                                                     archive)))
                    (test-equal
                     "initial backup leaves one archive after prune" 1
                     (length archives))
                    (test-assert "included file is present in archive"
                                 (contains-substring? contents
                                                      "source/keep.txt"))
                    (test-assert "excluded cache file is absent from archive"
                                 (not (contains-substring? contents
                                       "source/.cache/skip.txt")))
                    (test-equal "extract succeeds for created archive" 0
                                (extract-archive job-program repository archive
                                                 extract))
                    (test-equal "restored file contents match source"
                                "first version\n"
                                (read-text-file (string-append extract source
                                                               "/keep.txt"))))

                  (sleep 1)
                  (write-text-file (string-append source "/keep.txt")
                                   "second version\n")

                  (call-with-values (lambda ()
                                      (capture job-program))
                                    (lambda (status output)
                                      (test-equal "second backup succeeds" 0
                                                  status)
                                      (test-assert "second backup ran prune"
                                                   (contains-substring? output
                                                    " prune "))
                                      (test-assert "second backup ran compact"
                                       (contains-substring? output " compact "))))

                  (let ((archives (archive-names job-program repository)))
                    (test-equal
                     "retention policy keeps one archive after second backup"
                     1
                     (length archives)))

                  (call-with-values (lambda ()
                                      (capture partial-check-program "check"))
                                    (lambda (status output)
                                      (test-equal
                                       "repository-only partial check succeeds"
                                       0 status)
                                      (test-assert
                                       "repository-only partial check uses --repository-only"
                                       (contains-substring? output
                                        "--repository-only"))))

                  (call-with-values (lambda ()
                                      (capture archive-check-program "check"))
                                    (lambda (status output)
                                      (test-equal
                                       "archive-only verify-data check succeeds"
                                       0 status)
                                      (test-assert
                                       "archive-only verify-data check uses --verify-data"
                                       (contains-substring? output
                                                            "--verify-data")))))
                (lambda ()
                  (false-if-exception (delete-file-recursively directory)))))

(let* ((directory (mktemp-directory))
       (source (string-append directory "/source"))
       (passphrase-file (string-append directory "/passphrase"))
       (repository (string-append directory "/repo"))
       (wrapper-program (build-wrapper-program (format #f
                                                "(borg-backup-job
  (name ~s)
  (repository ~s)
  (passcommand ~s)
  (encryption ~s)
  (schedule ~s)
  (paths '~s)
  (check-repository? #f)
  (check-archives? #t)
  (check-verify-data? #t)
  (verbose? #t))"
                                                "pcmd"
                                                repository
                                                (format #f "~a -c ~s"
                                                        %guile-executable
                                                        (format #f
                                                                "(use-modules (ice-9 textual-ports)) (display (call-with-input-file ~s get-string-all))"
                                                                passphrase-file))
                                                "repokey"
                                                "0 1 * * *"
                                                (list source)))))
  (dynamic-wind (lambda ()
                  (mkdir-p source)
                  (write-text-file passphrase-file "wrapper-passphrase\n")
                  (write-text-file (string-append source "/hello.txt")
                                   "hello from wrapper\n"))
                (lambda ()
                  (call-with-values (lambda ()
                                      (capture wrapper-program "init" "pcmd"))
                                    (lambda (status output)
                                      (test-equal
                                       "wrapper init succeeds with passcommand"
                                       0 status)
                                      (test-assert
                                       "wrapper init reaches borg init"
                                       (contains-substring? output
                                        "init --encryption repokey"))))

                  (call-with-values (lambda ()
                                      (capture wrapper-program "backup" "pcmd"))
                                    (lambda (status output)
                                      (test-equal
                                       "wrapper backup succeeds with passcommand"
                                       0 status)
                                      (test-assert
                                       "wrapper backup reaches borg create"
                                       (contains-substring? output " create "))))

                  (call-with-values (lambda ()
                                      (capture wrapper-program "check" "pcmd"))
                                    (lambda (status output)
                                      (test-equal
                                       "wrapper check succeeds with archive verification"
                                       0 status)
                                      (test-assert
                                       "wrapper check reaches borg check"
                                       (contains-substring? output
                                                            "--verify-data"))))

                  (call-with-values (lambda ()
                                      (capture wrapper-program "borg" "pcmd"
                                               "list"))
                                    (lambda (status output)
                                      (test-equal
                                       "wrapper arbitrary borg command succeeds"
                                       0 status)
                                      (test-assert
                                       "wrapper borg list shows the job archive"
                                       (contains-substring? output "pcmd")))))
                (lambda ()
                  (false-if-exception (delete-file-recursively directory)))))

(test-end "borg-backup"))

(when (running-as-script?)
  (run-tests))
