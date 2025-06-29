(define-module (myguix services backup)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (borg-backup-job borg-backup-job?
                            borg-backup-job-fields
                            borg-backup-job-borg
                            borg-backup-job-user
                            borg-backup-job-group
                            borg-backup-job-log-file
                            borg-backup-job-max-duration
                            borg-backup-job-wait-for-termination?
                            borg-backup-job-name
                            borg-backup-job-repository
                            borg-backup-job-passphrase-file
                            borg-backup-job-encryption
                            borg-backup-job-archive-name-format
                            borg-backup-job-schedule
                            borg-backup-job-paths
                            borg-backup-job-exclude-patterns
                            borg-backup-job-requirement
                            borg-backup-job-compression
                            borg-backup-job-verbose?
                            borg-backup-job-stats?
                            borg-backup-job-progress?
                            borg-backup-job-prune-keep-daily
                            borg-backup-job-prune-keep-weekly
                            borg-backup-job-prune-keep-monthly
                            borg-backup-job-prune-keep-yearly
                            borg-backup-job-extra-create-flags
                            borg-backup-job-extra-prune-flags

                            lower-borg-backup-job

                            borg-backup-configuration
                            borg-backup-configuration?
                            borg-backup-configuration-jobs

                            borg-backup-job-program
                            borg-backup-job->shepherd-service
                            borg-guix
                            borg-guix-wrapper-package
                            borg-backup-service-profile
                            borg-program
                            borg-job-log-file
                            borg-backup-job-command
                            borg-backup-job-modules
                            borg-backup-service-type))

(define (gexp-or-string? value)
  (or (gexp? value)
      (string? value)))

(define (lowerable? value)
  (or (file-like? value)
      (gexp-or-string? value)))

(define list-of-lowerables?
  (list-of lowerable?))

(define list-of-symbols?
  (list-of symbol?))

(define list-of-strings?
  (list-of string?))

(define-maybe/no-serialization string)
(define-maybe/no-serialization number)
(define-maybe/no-serialization symbol)
(define-maybe/no-serialization list-of-symbols)

(define-configuration/no-serialization borg-backup-job
                                       (borg (package
                                               borg)
                                        "The borg package to be used for the current job.")
                                       (user (string "root")
                                        "The user used for running the current job.")
                                       (group (string "root")
                                        "The group used for running the current job.")
                                       (log-file (maybe-string)
                                        "The file system path to the log file for this job.  By default the file will
be @file{/var/log/borg-backup/@var{job-name}.log}, where @var{job-name} is the
name defined in the @code{name} field.  For Guix Home services it defaults to
@file{$XDG_STATE_HOME/shepherd/borg-backup/@var{job-name}.log}.")
                                       (max-duration (maybe-number)
                                        "The maximum duration in seconds that a job may last.  Past
@code{max-duration} seconds, the job is forcefully terminated.")
                                       (wait-for-termination? (boolean #f)
                                        "Wait until the job has finished before considering executing it again;
otherwise, perform it strictly on every occurrence of event, at the risk of
having multiple instances running concurrently.")
                                       (name (string)
                                        "A string denoting a name for this job.")
                                       (repository (string)
                                        "The borg repository target of this job.")
                                       (passphrase-file (maybe-string)
                                        "Name of the passphrase file, readable by the configured @code{user}, that
will be used to set the @code{BORG_PASSPHRASE} environment variable for the
current job.  Leave unset for repositories without encryption.")
                                       (encryption (string "repokey")
                                        "The encryption mode for the repository.  Common values are @code{\"none\"},
@code{\"repokey\"}, @code{\"keyfile\"}, @code{\"repokey-blake2\"}, and
@code{\"keyfile-blake2\"}.")
                                       (archive-name-format (string
                                                             "{hostname}-{user}-{now:%Y-%m-%d_%H:%M:%S}")
                                        "The format string for archive names.  Borg supports placeholders like
@code{{hostname}}, @code{{user}}, and @code{{now}} with strftime formatting.")
                                       (schedule (gexp-or-string)
                                        "A string or a gexp representing the frequency of the backup.  Gexp must
evaluate to @code{calendar-event} records or to strings.  Strings must contain
Vixie cron date lines.")
                                       (requirement (maybe-list-of-symbols)
                                        "The list of Shepherd services that this backup job depends upon.  When unset it
defaults to @code{'()}, for Guix Home.  Otherwise to
@code{'(user-processes file-systems)}.")
                                       (paths (list-of-lowerables '())
                                        "The list of files or directories to be backed up.  It must be a list of
values that can be lowered to strings.")
                                       (exclude-patterns (list-of-strings '())
                                        "List of patterns for paths to exclude from the backup.")
                                       (compression (string "lz4")
                                        "Compression algorithm and level, e.g. @code{\"lz4\"}, @code{\"zstd,3\"},
@code{\"lzma,6\"}, or @code{\"none\"}.")
                                       (verbose? (boolean #f)
                                        "Whether to enable verbose output for the current backup job.")
                                       (stats? (boolean #t)
                                        "Whether to print statistics at the end of the backup.")
                                       (progress? (boolean #t)
                                        "Whether to show progress during backup.")
                                       (prune-keep-daily (maybe-number)
                                        "Number of daily backups to keep when pruning.")
                                       (prune-keep-weekly (maybe-number)
                                        "Number of weekly backups to keep when pruning.")
                                       (prune-keep-monthly (maybe-number)
                                        "Number of monthly backups to keep when pruning.")
                                       (prune-keep-yearly (maybe-number)
                                        "Number of yearly backups to keep when pruning.")
                                       (extra-create-flags (list-of-lowerables '())
                                        "A list of values that are lowered to strings.  These will be passed as
command-line arguments to the current job @command{borg create} invocation.")
                                       (extra-prune-flags (list-of-lowerables '())
                                        "A list of values that are lowered to strings.  These will be passed as
command-line arguments to the @command{borg prune} invocation."))

(define-record-type* <borg-backup-configuration> borg-backup-configuration
                     make-borg-backup-configuration
  borg-backup-configuration?
  this-borg-backup-configuration

  (jobs borg-backup-configuration-jobs
        (default '())) ;list of borg-backup-job
  (home-service? borg-backup-configuration-home-service?
                 (default for-home?)
                 (innate)))

(define (lower-borg-backup-job config)
  (let ((borg (file-append (borg-backup-job-borg config) "/bin/borg"))
        (repository (borg-backup-job-repository config))
        (passphrase-file (borg-backup-job-passphrase-file config))
        (archive-name-format (borg-backup-job-archive-name-format config))
        (paths (borg-backup-job-paths config))
        (exclude-patterns (borg-backup-job-exclude-patterns config))
        (compression (borg-backup-job-compression config))
        (extra-create-flags (borg-backup-job-extra-create-flags config))
        (extra-prune-flags (borg-backup-job-extra-prune-flags config))
        (verbose? (borg-backup-job-verbose? config))
        (stats? (borg-backup-job-stats? config))
        (progress? (borg-backup-job-progress? config))
        (prune-keep-daily (borg-backup-job-prune-keep-daily config))
        (prune-keep-weekly (borg-backup-job-prune-keep-weekly config))
        (prune-keep-monthly (borg-backup-job-prune-keep-monthly config))
        (prune-keep-yearly (borg-backup-job-prune-keep-yearly config)))
    #~(list #$borg
            #$repository
            #$passphrase-file
            #$archive-name-format
            (list #$@paths)
            '#$exclude-patterns
            #$compression
            #$verbose?
            #$stats?
            #$progress?
            #$prune-keep-daily
            #$prune-keep-weekly
            #$prune-keep-monthly
            #$prune-keep-yearly
            (list #$@extra-create-flags)
            (list #$@extra-prune-flags))))

(define borg-program
  #~(lambda (action job-args)
      (use-modules (ice-9 format)
                   (ice-9 match)
                   (srfi srfi-1))

      (match job-args
        ((borg repository
               passphrase-file
               archive-name-format
               paths
               exclude-patterns
               compression
               verbose?
               stats?
               progress?
               prune-keep-daily
               prune-keep-weekly
               prune-keep-monthly
               prune-keep-yearly
               extra-create-flags
               extra-prune-flags)
         
         (when (and passphrase-file
                    (file-exists? passphrase-file))
           (setenv "BORG_PASSPHRASE"
                   (call-with-input-file passphrase-file
                     (lambda (port)
                       (string-trim-right (get-string-all port))))))

         (define create-command
           `(,borg "create"
             ,@(if verbose?
                   '("--verbose")
                   '())
             ,@(if stats?
                   '("--stats")
                   '())
             ,@(if progress?
                   '("--progress")
                   '())
             "--compression"
             ,compression
             ,@(append-map (lambda (pattern)
                             (list "--exclude" pattern)) exclude-patterns)
             ,@extra-create-flags
             ,(string-append repository "::" archive-name-format)
             ,@paths))

         (define prune-command
           `(,borg "prune"
             ,@(if verbose?
                   '("--verbose")
                   '())
             ,@(if stats?
                   '("--stats")
                   '())
             ,@(if progress?
                   '("--progress")
                   '())
             ,@(if (maybe-value-set? prune-keep-daily)
                   `("--keep-daily" ,(number->string prune-keep-daily))
                   '())
             ,@(if (maybe-value-set? prune-keep-weekly)
                   `("--keep-weekly" ,(number->string prune-keep-weekly))
                   '())
             ,@(if (maybe-value-set? prune-keep-monthly)
                   `("--keep-monthly" ,(number->string prune-keep-monthly))
                   '())
             ,@(if (maybe-value-set? prune-keep-yearly)
                   `("--keep-yearly" ,(number->string prune-keep-yearly))
                   '())
             ,@extra-prune-flags
             ,repository))

         (cond
           ((string=? action "create")
            (when verbose?
              (format #t "Running~{ ~a~}~%" create-command))
            (apply execlp create-command))

           ((string=? action "prune")
            (when verbose?
              (format #t "Running~{ ~a~}~%" prune-command))
            (apply execlp prune-command))

           ((string=? action "create-and-prune")
            ;; Run create first
            (when verbose?
              (format #t "Running~{ ~a~}~%" create-command))
            (let ((status (apply system* create-command)))
              (if (zero? status)
                  (begin
                    ;; If any prune options are set, run prune
                    (when (or (maybe-value-set? prune-keep-daily)
                              (maybe-value-set? prune-keep-weekly)
                              (maybe-value-set? prune-keep-monthly)
                              (maybe-value-set? prune-keep-yearly))
                      (when verbose?
                        (format #t "Running~{ ~a~}~%" prune-command))
                      (apply execlp prune-command))
                    ;; If no prune options, just exit successfully
                    (exit 0))
                  (exit 1))))

           (else (error "Unknown action" action)))))))

(define (borg-backup-job-program config)
  (program-file "borg-backup"
                #~(let ((borg-exec #$borg-program)
                        (job #$(lower-borg-backup-job config)))
                    (apply borg-exec
                           (list "create-and-prune" job)))))

(define (borg-guix jobs)
  (program-file "borg-guix"
                #~(begin
                    (use-modules (ice-9 match)
                                 (srfi srfi-1))

                    (define names
                      '#$(map borg-backup-job-name jobs))
                    (define programs
                      '#$(map borg-backup-job-program jobs))
                    (define repositories
                      '#$(map borg-backup-job-repository jobs))
                    (define jobs
                      '#$(map lower-borg-backup-job jobs))

                    (define (get-program name)
                      (define idx
                        (list-index (lambda (n)
                                      (string=? n name)) names))
                      (unless idx
                        (error (string-append "Unknown job name " name "\n\n"
                                              "Possible job names are: "
                                              (string-join names " "))))
                      (list-ref programs idx))

                    (define (get-repository name)
                      (define idx
                        (list-index (lambda (n)
                                      (string=? n name)) names))
                      (list-ref repositories idx))

                    (define (get-job name)
                      (define idx
                        (list-index (lambda (n)
                                      (string=? n name)) names))
                      (list-ref jobs idx))

                    (define (backup args)
                      (define name
                        (third args))
                      (define program
                        (get-program name))
                      (execlp program program))

                    (define (run-borg-command args)
                      (define name
                        (third args))
                      (define command
                        (fourth args))
                      (define rest-args
                        (drop args 4))
                      (define job
                        (get-job name))
                      (define repository
                        (get-repository name))

                      ;; Set up environment from job
                      (match job
                        ((borg _ passphrase-file . _) (when (and
                                                             passphrase-file
                                                             (file-exists?
                                                              passphrase-file))
                                                        (setenv
                                                         "BORG_PASSPHRASE"
                                                         (call-with-input-file passphrase-file
                                                           (lambda (port)
                                                             (string-trim-right
                                                              (get-string-all
                                                               port))))))))

                      ;; Execute borg command with repository
                      (apply execlp borg
                             `(,borg ,command
                               ,repository
                               ,@rest-args)))

                    (define (validate-args args)
                      (when (not (>= (length args) 3))
                        (error (string-append "Usage: "
                                              (basename (car args))
                                              " backup NAME\n"
                                              "       "
                                              (basename (car args))
                                              " borg NAME COMMAND [ARGS...]"))))

                    (define (main args)
                      (validate-args args)
                      (define action
                        (second args))
                      (match action
                        ("backup" (backup args))
                        ("borg" (when (not (>= (length args) 4))
                                  (error
                                   "borg action requires NAME and COMMAND"))
                         (run-borg-command args))
                        (_ (error (string-append "Unknown action: " action)))))

                    (main (command-line)))))

(define* (borg-job-log-file job
                            #:key (home-service? #f))
  (let ((name (borg-backup-job-name job))
        (log-file (borg-backup-job-log-file job)))
    (if (maybe-value-set? log-file) log-file
        (if home-service?
            #~(begin
                (use-modules (shepherd support))
                (string-append %user-log-dir "/borg-backup/"
                               #$name ".log"))
            (string-append "/var/log/borg-backup/" name ".log")))))

(define* (borg-backup-job-command name paths
                                  #:key (home-service? #f))
  (if home-service?
      #~(list "borg-guix" "backup"
              #$name)
      #~(list (string-append #$bash-minimal "/bin/bash") "-l" "-c"
              (string-append "borg-guix backup "
                             #$name))))

(define* (borg-job-requirement config
                               #:key (home-service? #f))
  (define maybe-requirement
    (borg-backup-job-requirement config))
  (if (maybe-value-set? maybe-requirement) maybe-requirement
      (if home-service?
          '()
          '(user-processes file-systems))))

(define* (borg-backup-job-modules #:key (home-service? #f))
  `((shepherd service timer)
    ,@(if home-service?
          '((shepherd support))
          '())))

(define* (borg-backup-job->shepherd-service config
                                            #:key (home-service? #f))
  (let ((schedule (borg-backup-job-schedule config))
        (name (borg-backup-job-name config))
        (paths (borg-backup-job-paths config))
        (user (borg-backup-job-user config))
        (group (borg-backup-job-group config))
        (max-duration (borg-backup-job-max-duration config))
        (wait-for-termination? (borg-backup-job-wait-for-termination? config))
        (log-file (borg-job-log-file config
                                     #:home-service? home-service?))
        (requirement (borg-job-requirement config
                                           #:home-service? home-service?)))
    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement requirement)
                      (documentation "Run borg backups on a regular basis.")
                      (modules (borg-backup-job-modules #:home-service?
                                                        home-service?))
                      (start #~(make-timer-constructor (if (string? #$schedule)
                                                           (cron-string->calendar-event #$schedule)
                                                           #$schedule)
                                                       (command #$(borg-backup-job-command
                                                                   name paths
                                                                   #:home-service?
                                                                   home-service?)
                                                                #$@(if
                                                                    home-service?
                                                                    '()
                                                                    (list
                                                                     #:user
                                                                     user))
                                                                #$@(if
                                                                    home-service?
                                                                    '()
                                                                    (list
                                                                     #:group
                                                                     group))
                                                                #$@(if
                                                                    home-service?
                                                                    '()
                                                                    (list
                                                                          #:environment-variables #~
                                                                          (list
                                                                           (string-append
                                                                            "HOME="
                                                                            (passwd:dir
                                                                             (getpwnam #$user)))))))
                                                       #:log-file #$log-file
                                                       #:wait-for-termination? #$wait-for-termination?
                                                       #:max-duration #$(and (maybe-value-set?
                                                                              max-duration)
                                                                         max-duration)))
                      (stop #~(make-timer-destructor))
                      (actions (list (shepherd-action (inherit
                                                       shepherd-trigger-action)
                                                      (documentation
                                                       "Manually trigger a backup,
without waiting for the scheduled time.")))))))

(define (borg-guix-wrapper-package jobs)
  (package
    (name "borg-backup-service-wrapper")
    (version "0.0.0")
    (source
     (borg-guix jobs))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("./" "/bin"))))
    (home-page "https://www.borgbackup.org")
    (synopsis "Easily interact from the CLI with Guix configured Borg backups")
    (description
     "This package provides a simple wrapper around @code{borg}, handled
by the @code{borg-backup-service-type}.  It allows for easily interacting
with Guix configured backup jobs, for example for manually triggering a backup
without waiting for the scheduled job to run.  You can also run arbitrary borg
commands on configured repositories using @command{borg-guix borg JOB-NAME COMMAND}.")
    (license license:gpl3+)))

(define borg-backup-service-profile
  (lambda (config)
    (define jobs
      (borg-backup-configuration-jobs config))
    (if (> (length jobs) 0)
        (list (borg-guix-wrapper-package jobs))
        '())))

(define borg-backup-service-type
  (service-type (name 'borg-backup)
                (extensions (list (service-extension profile-service-type
                                   borg-backup-service-profile)
                                  (service-extension
                                   shepherd-root-service-type
                                   (match-record-lambda <borg-backup-configuration>
                                     (jobs home-service?)
                                     (map (lambda (job)
                                            (borg-backup-job->shepherd-service
                                             job
                                             #:home-service? home-service?))
                                          jobs)))))
                (compose concatenate)
                (extend (lambda (config jobs)
                          (borg-backup-configuration (inherit config)
                                                     (jobs (append (borg-backup-configuration-jobs
                                                                    config)
                                                                   jobs)))))
                (default-value (borg-backup-configuration))
                (description
                 "This service configures Shepherd timers for running backups
with Borg backup system.")))
