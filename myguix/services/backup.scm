;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025, 2026 Ayan Das <bvits@riseup.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

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
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
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
                            borg-backup-job-passcommand
                            borg-backup-job-encryption
                            borg-backup-job-append-only?
                            borg-backup-job-make-parent-directories?
                            borg-backup-job-initialize-repository?
                            borg-backup-job-archive-name-format
                            borg-backup-job-archive-match-pattern
                            borg-backup-job-schedule
                            borg-backup-job-check-schedule
                            borg-backup-job-requirement
                            borg-backup-job-paths
                            borg-backup-job-patterns
                            borg-backup-job-patterns-files
                            borg-backup-job-exclude-patterns
                            borg-backup-job-exclude-files
                            borg-backup-job-exclude-caches?
                            borg-backup-job-exclude-if-present
                            borg-backup-job-keep-exclude-tags?
                            borg-backup-job-one-file-system?
                            borg-backup-job-compression
                            borg-backup-job-verbose?
                            borg-backup-job-stats?
                            borg-backup-job-progress?
                            borg-backup-job-show-rc?
                            borg-backup-job-lock-wait
                            borg-backup-job-remote-path
                            borg-backup-job-rsh-command
                            borg-backup-job-environment-variables
                            borg-backup-job-extra-options
                            borg-backup-job-extra-init-flags
                            borg-backup-job-extra-create-flags
                            borg-backup-job-prune?
                            borg-backup-job-prune-keep-within
                            borg-backup-job-prune-keep-last
                            borg-backup-job-prune-keep-secondly
                            borg-backup-job-prune-keep-minutely
                            borg-backup-job-prune-keep-hourly
                            borg-backup-job-prune-keep-daily
                            borg-backup-job-prune-keep-weekly
                            borg-backup-job-prune-keep-monthly
                            borg-backup-job-prune-keep-quarterly-13weekly
                            borg-backup-job-prune-keep-quarterly-3monthly
                            borg-backup-job-prune-keep-yearly
                            borg-backup-job-prune-save-space?
                            borg-backup-job-extra-prune-flags
                            borg-backup-job-compact?
                            borg-backup-job-compact-threshold
                            borg-backup-job-compact-cleanup-commits?
                            borg-backup-job-extra-compact-flags
                            borg-backup-job-check-repository?
                            borg-backup-job-check-archives?
                            borg-backup-job-check-verify-data?
                            borg-backup-job-check-max-duration
                            borg-backup-job-extra-check-flags

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

;;; Commentary:
;;;
;;; This module provides a production-oriented Borg backup service for
;;; Guix System and Guix Home.  It keeps the service model used by Guix's
;;; restic service, but adds Borg-specific safety and maintenance support:
;;; archive names and prune/check matching default to job-scoped values,
;;; prune and compact are first-class operations, repositories can be
;;; initialized from the same job description, and integrity checks can be
;;; scheduled independently from backup runs.
;;;
;;; Code:

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
(define-maybe/no-serialization lowerable)
(define-maybe/no-serialization gexp-or-string)
(define-maybe/no-serialization list-of-symbols)

(define %borg-backup-job-name-characters
  (string->list
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._+-"))

(define (valid-borg-backup-job-name? name)
  (and (not (string-null? name))
       (every (lambda (character)
                (member character %borg-backup-job-name-characters))
              (string->list name))))

(define-configuration/no-serialization borg-backup-job
                                       (borg (package
                                               borg)
                                        "The Borg package to be used for the current job.")
                                       (user (string "root")
                                        "The user used for running the current job.")
                                       (group (string "root")
                                        "The group used for running the current job.")
                                       (log-file (maybe-string)
                                        "The file system path to the log file for this job.  By default the
file will be @file{/var/log/borg-backup/@var{job-name}.log}, where @var{job-name}
is the value of the @code{name} field.  For Guix Home services it defaults to
@file{$XDG_STATE_HOME/shepherd/borg-backup/@var{job-name}.log}.")
                                       (max-duration (maybe-number)
                                        "The maximum duration in seconds that a timer-triggered run may
last.  Past @code{max-duration} seconds, Shepherd forcefully terminates it.")
                                       (wait-for-termination? (boolean #f)
                                        "Wait until the current run has finished before
considering executing it again; otherwise, execute it strictly on every event,
at the risk of overlapping runs.")
                                       (name (string)
                                        "A short shell-safe name identifying this job.  Names must only contain
letters, digits, @code{.}, @code{_}, @code{+}, and @code{-}.")
                                       (repository (string)
                                        "The Borg repository target of this job.")
                                       (passphrase-file (maybe-lowerable)
                                        "A file name or file-like object containing the repository
passphrase.  The passphrase is passed to Borg through
@code{BORG_PASSPHRASE_FD}.  This option is mutually exclusive with
@code{passcommand}.")
                                       (passcommand (maybe-string)
                                        "A command string passed through @code{BORG_PASSCOMMAND}.  Use
this for password managers or other secret providers.  This option is mutually
exclusive with @code{passphrase-file}.")
                                       (encryption (string "repokey")
                                        "The encryption mode used when initializing a repository.  This
is only used by @command{borg init}.")
                                       (append-only? (boolean #f)
                                        "Whether to pass @code{--append-only} when initializing a new
repository.")
                                       (make-parent-directories? (boolean #f)
                                        "Whether to pass @code{--make-parent-dirs} when
initializing a new repository.")
                                       (initialize-repository? (boolean #f)
                                        "Whether scheduled backup runs should automatically
initialize a missing local repository before the first backup.  Remote
repositories are never auto-initialized; use @command{borg-guix init JOB} for
those.")
                                       (archive-name-format (maybe-string)
                                        "The format string for archive names.  When unset, it
defaults to a value derived from the job name:
@code{{hostname}-{user}-@var{job-name}-{now:%Y-%m-%dT%H:%M:%S}}.  When you
override this field, set @code{archive-match-pattern} too unless your custom
format keeps the same unique prefix.")
                                       (archive-match-pattern (maybe-string)
                                        "The @option{--glob-archives} pattern used for prune
and archive checks.  When unset, it defaults to
@code{{hostname}-{user}-@var{job-name}-*}, which safely scopes maintenance to a
single job.")
                                       (schedule (gexp-or-string)
                                        "A string or gexp representing the frequency of backup runs.  Gexps
must evaluate to @code{calendar-event} records or to strings.  Strings must
contain Vixie cron date lines.")
                                       (check-schedule (maybe-gexp-or-string)
                                        "An optional string or gexp representing the frequency of
integrity checks.  When set, an additional Shepherd timer runs
@command{borg check} using this job's check-related fields.")
                                       (requirement (maybe-list-of-symbols)
                                        "The list of Shepherd services that this job depends upon.  When
unset it defaults to @code{'()}, for Guix Home.  Otherwise it defaults to
@code{'(user-processes file-systems)}.")
                                       (paths (list-of-lowerables '())
                                        "The list of files or directories to back up.  It must be a list of
values that can be lowered to strings.")
                                       (patterns (list-of-strings '())
                                        "A list of Borg @option{--pattern} rules.")
                                       (patterns-files (list-of-lowerables '())
                                        "A list of values lowered to strings and passed via
@option{--patterns-from}.")
                                       (exclude-patterns (list-of-strings '())
                                        "A list of Borg @option{--exclude} patterns.")
                                       (exclude-files (list-of-lowerables '())
                                        "A list of values lowered to strings and passed via
@option{--exclude-from}.")
                                       (exclude-caches? (boolean #f)
                                        "Whether to pass @option{--exclude-caches}.")
                                       (exclude-if-present (list-of-strings '())
                                        "A list of tag file names passed through repeated
@option{--exclude-if-present} flags.")
                                       (keep-exclude-tags? (boolean #f)
                                        "Whether to pass @option{--keep-exclude-tags}.")
                                       (one-file-system? (boolean #f)
                                        "Whether to pass @option{--one-file-system}.")
                                       (compression (string "lz4")
                                        "Compression algorithm and level, for example @code{\"lz4\"},
@code{\"zstd,3\"}, @code{\"lzma,6\"}, or @code{\"none\"}.")
                                       (verbose? (boolean #f)
                                        "Whether to use Borg's informational logging output.")
                                       (stats? (boolean #t)
                                        "Whether create and prune operations should print statistics.")
                                       (progress? (boolean #f)
                                        "Whether to show progress information during Borg operations.")
                                       (show-rc? (boolean #t)
                                        "Whether Borg should log its return code with
@option{--show-rc}.")
                                       (lock-wait (number 600)
                                        "The maximum number of seconds Borg should wait for a repository
or cache lock.")
                                       (remote-path (maybe-string)
                                        "The value passed to Borg's @option{--remote-path}.")
                                       (rsh-command (maybe-string)
                                        "The value passed to Borg's @option{--rsh}.")
                                       (environment-variables (list-of-lowerables '())
                                        "A list of @code{NAME=VALUE} strings, or values
lowered to such strings, added to the environment before invoking Borg.")
                                       (extra-options (list-of-lowerables '())
                                        "A list of values lowered to strings and passed to Borg before
the subcommand.  Use this for common options not modeled explicitly by the
service.")
                                       (extra-init-flags (list-of-lowerables '())
                                        "Additional command-line arguments passed to
@command{borg init}.")
                                       (extra-create-flags (list-of-lowerables '())
                                        "Additional command-line arguments passed to
@command{borg create}.")
                                       (prune? (boolean #t)
                                        "Whether scheduled backup runs should prune old archives when a prune
policy is configured.")
                                       (prune-keep-within (maybe-string)
                                        "The value passed to @option{--keep-within}.")
                                       (prune-keep-last (maybe-number)
                                        "The value passed to @option{--keep-last}.")
                                       (prune-keep-secondly (maybe-number)
                                        "The value passed to @option{--keep-secondly}.")
                                       (prune-keep-minutely (maybe-number)
                                        "The value passed to @option{--keep-minutely}.")
                                       (prune-keep-hourly (maybe-number)
                                        "The value passed to @option{--keep-hourly}.")
                                       (prune-keep-daily (maybe-number)
                                        "The value passed to @option{--keep-daily}.")
                                       (prune-keep-weekly (maybe-number)
                                        "The value passed to @option{--keep-weekly}.")
                                       (prune-keep-monthly (maybe-number)
                                        "The value passed to @option{--keep-monthly}.")
                                       (prune-keep-quarterly-13weekly (maybe-number)
                                        "The value passed to
@option{--keep-13weekly}.")
                                       (prune-keep-quarterly-3monthly (maybe-number)
                                        "The value passed to
@option{--keep-3monthly}.")
                                       (prune-keep-yearly (maybe-number)
                                        "The value passed to @option{--keep-yearly}.")
                                       (prune-save-space? (boolean #f)
                                        "Whether to pass @option{--save-space} to
@command{borg prune}.")
                                       (extra-prune-flags (list-of-lowerables '())
                                        "Additional command-line arguments passed to
@command{borg prune}.")
                                       (compact? (boolean #t)
                                        "Whether scheduled backup runs should compact the repository after
pruning when compaction is enabled.")
                                       (compact-threshold (maybe-number)
                                        "The value passed to @option{--threshold} for
@command{borg compact}.")
                                       (compact-cleanup-commits? (boolean #f)
                                        "Whether to pass @option{--cleanup-commits} to
@command{borg compact}.")
                                       (extra-compact-flags (list-of-lowerables '())
                                        "Additional command-line arguments passed to
@command{borg compact}.")
                                       (check-repository? (boolean #t)
                                        "Whether scheduled or manual @command{borg check} runs
should include repository checks.")
                                       (check-archives? (boolean #f)
                                        "Whether scheduled or manual @command{borg check} runs
should include archive checks.")
                                       (check-verify-data? (boolean #f)
                                        "Whether archive checks should use
@option{--verify-data}.")
                                       (check-max-duration (maybe-number)
                                        "The value passed to @option{--max-duration} for partial
repository checks.  Per Borg's rules, this can only be used for
repository-only checks.")
                                       (extra-check-flags (list-of-lowerables '())
                                        "Additional command-line arguments passed to
@command{borg check}."))

;; (for-home (borg-backup-configuration ...)) is not able to replace for-home?
;; with #t, pk prints #f.  Once for-home can work with
;; (gnu services configuration), this record can be migrated back to
;; define-configuration.
(define-record-type* <borg-backup-configuration> borg-backup-configuration
                     make-borg-backup-configuration
  borg-backup-configuration?
  this-borg-backup-configuration

  (jobs borg-backup-configuration-jobs
        (default '())) ;list of borg-backup-job
  (home-service? borg-backup-configuration-home-service?
                 (default for-home?)
                 (innate)))

(define (borg-backup-job-default-archive-prefix config)
  (string-append "{hostname}-{user}-"
                 (borg-backup-job-name config) "-"))

(define (borg-backup-job-effective-archive-name-format config)
  (let ((format (borg-backup-job-archive-name-format config)))
    (if (maybe-value-set? format) format
        (string-append (borg-backup-job-default-archive-prefix config)
                       "{now:%Y-%m-%dT%H:%M:%S}"))))

(define (borg-backup-job-effective-archive-match-pattern config)
  (let ((pattern (borg-backup-job-archive-match-pattern config)))
    (if (maybe-value-set? pattern) pattern
        (string-append (borg-backup-job-default-archive-prefix config) "*"))))

(define (borg-backup-job-prune-policy-configured? config)
  (or (maybe-value-set? (borg-backup-job-prune-keep-within config))
      (maybe-value-set? (borg-backup-job-prune-keep-last config))
      (maybe-value-set? (borg-backup-job-prune-keep-secondly config))
      (maybe-value-set? (borg-backup-job-prune-keep-minutely config))
      (maybe-value-set? (borg-backup-job-prune-keep-hourly config))
      (maybe-value-set? (borg-backup-job-prune-keep-daily config))
      (maybe-value-set? (borg-backup-job-prune-keep-weekly config))
      (maybe-value-set? (borg-backup-job-prune-keep-monthly config))
      (maybe-value-set? (borg-backup-job-prune-keep-quarterly-13weekly config))
      (maybe-value-set? (borg-backup-job-prune-keep-quarterly-3monthly config))
      (maybe-value-set? (borg-backup-job-prune-keep-yearly config))
      (pair? (borg-backup-job-extra-prune-flags config))))

(define (borg-backup-job-prune-enabled? config)
  (and (borg-backup-job-prune? config)
       (borg-backup-job-prune-policy-configured? config)))

(define (borg-backup-job-compact-enabled? config)
  (and (borg-backup-job-compact? config)
       (or (borg-backup-job-prune-enabled? config)
           (maybe-value-set? (borg-backup-job-compact-threshold config))
           (borg-backup-job-compact-cleanup-commits? config)
           (pair? (borg-backup-job-extra-compact-flags config)))))

(define (validate-borg-backup-job config)
  (let ((name (borg-backup-job-name config))
        (passphrase-file (borg-backup-job-passphrase-file config))
        (passcommand (borg-backup-job-passcommand config))
        (check-repository? (borg-backup-job-check-repository? config))
        (check-archives? (borg-backup-job-check-archives? config))
        (check-verify-data? (borg-backup-job-check-verify-data? config))
        (check-max-duration (borg-backup-job-check-max-duration config)))
    (unless (valid-borg-backup-job-name? name)
      (error "Invalid Borg backup job name" name))
    (when (and (maybe-value-set? passphrase-file)
               (maybe-value-set? passcommand))
      (error "Borg backup job cannot use both passphrase-file and passcommand"
       name))
    (unless (or check-repository? check-archives?)
      (error "Borg backup job must enable repository or archive checks" name))
    (when (and check-verify-data?
               (not check-archives?))
      (error "Borg backup job enables verify-data without archive checks" name))
    (when (and (maybe-value-set? check-max-duration)
               (or (not check-repository?) check-archives?))
      (error
       "Borg backup job uses check-max-duration without a repository-only check"
       name))) config)

(define (validate-borg-backup-jobs jobs)
  (let loop
    ((seen '())
     (remaining jobs))
    (match remaining
      (() jobs)
      ((job . rest) (let ((job (validate-borg-backup-job job))
                          (name (borg-backup-job-name job)))
                      (when (member name seen)
                        (error "Duplicate Borg backup job name" name))
                      (loop (cons name seen) rest))))))

(define (validated-borg-backup-jobs config)
  (let ((jobs (borg-backup-configuration-jobs config)))
    (validate-borg-backup-jobs jobs) jobs))

(define (lower-borg-backup-job config)
  (let ((borg (file-append (borg-backup-job-borg config) "/bin/borg"))
        (passphrase-file (borg-backup-job-passphrase-file config))
        (check-schedule (borg-backup-job-check-schedule config))
        (patterns-files (borg-backup-job-patterns-files config))
        (exclude-files (borg-backup-job-exclude-files config))
        (environment-variables (borg-backup-job-environment-variables config))
        (extra-options (borg-backup-job-extra-options config))
        (extra-init-flags (borg-backup-job-extra-init-flags config))
        (paths (borg-backup-job-paths config))
        (extra-create-flags (borg-backup-job-extra-create-flags config))
        (extra-prune-flags (borg-backup-job-extra-prune-flags config))
        (extra-compact-flags (borg-backup-job-extra-compact-flags config))
        (extra-check-flags (borg-backup-job-extra-check-flags config)))
    #~(list (cons 'borg
                  #$borg)
            (cons 'name
                  #$(borg-backup-job-name config))
            (cons 'repository
                  #$(borg-backup-job-repository config))
            (cons 'passphrase-file
                  #$(and (maybe-value-set? passphrase-file) passphrase-file))
            (cons 'passcommand
                  #$(let ((value (borg-backup-job-passcommand config)))
                      (and (maybe-value-set? value) value)))
            (cons 'encryption
                  #$(borg-backup-job-encryption config))
            (cons 'append-only?
                  #$(borg-backup-job-append-only? config))
            (cons 'make-parent-directories?
                  #$(borg-backup-job-make-parent-directories? config))
            (cons 'initialize-repository?
                  #$(borg-backup-job-initialize-repository? config))
            (cons 'archive-name-format
                  #$(borg-backup-job-effective-archive-name-format config))
            (cons 'archive-match-pattern
                  #$(borg-backup-job-effective-archive-match-pattern config))
            (cons 'schedule
                  #$(borg-backup-job-schedule config))
            (cons 'check-schedule
                  #$(and (maybe-value-set? check-schedule) check-schedule))
            (cons 'paths
                  (list #$@paths))
            (cons 'patterns
                  '#$(borg-backup-job-patterns config))
            (cons 'patterns-files
                  (list #$@patterns-files))
            (cons 'exclude-patterns
                  '#$(borg-backup-job-exclude-patterns config))
            (cons 'exclude-files
                  (list #$@exclude-files))
            (cons 'exclude-caches?
                  #$(borg-backup-job-exclude-caches? config))
            (cons 'exclude-if-present
                  '#$(borg-backup-job-exclude-if-present config))
            (cons 'keep-exclude-tags?
                  #$(borg-backup-job-keep-exclude-tags? config))
            (cons 'one-file-system?
                  #$(borg-backup-job-one-file-system? config))
            (cons 'compression
                  #$(borg-backup-job-compression config))
            (cons 'verbose?
                  #$(borg-backup-job-verbose? config))
            (cons 'stats?
                  #$(borg-backup-job-stats? config))
            (cons 'progress?
                  #$(borg-backup-job-progress? config))
            (cons 'show-rc?
                  #$(borg-backup-job-show-rc? config))
            (cons 'lock-wait
                  #$(borg-backup-job-lock-wait config))
            (cons 'remote-path
                  #$(let ((value (borg-backup-job-remote-path config)))
                      (and (maybe-value-set? value) value)))
            (cons 'rsh-command
                  #$(let ((value (borg-backup-job-rsh-command config)))
                      (and (maybe-value-set? value) value)))
            (cons 'environment-variables
                  (list #$@environment-variables))
            (cons 'extra-options
                  (list #$@extra-options))
            (cons 'extra-init-flags
                  (list #$@extra-init-flags))
            (cons 'extra-create-flags
                  (list #$@extra-create-flags))
            (cons 'prune?
                  #$(borg-backup-job-prune? config))
            (cons 'prune-keep-within
                  #$(let ((value (borg-backup-job-prune-keep-within config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-last
                  #$(let ((value (borg-backup-job-prune-keep-last config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-secondly
                  #$(let ((value (borg-backup-job-prune-keep-secondly config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-minutely
                  #$(let ((value (borg-backup-job-prune-keep-minutely config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-hourly
                  #$(let ((value (borg-backup-job-prune-keep-hourly config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-daily
                  #$(let ((value (borg-backup-job-prune-keep-daily config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-weekly
                  #$(let ((value (borg-backup-job-prune-keep-weekly config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-monthly
                  #$(let ((value (borg-backup-job-prune-keep-monthly config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-quarterly-13weekly
                  #$(let ((value (borg-backup-job-prune-keep-quarterly-13weekly
                                  config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-quarterly-3monthly
                  #$(let ((value (borg-backup-job-prune-keep-quarterly-3monthly
                                  config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-keep-yearly
                  #$(let ((value (borg-backup-job-prune-keep-yearly config)))
                      (and (maybe-value-set? value) value)))
            (cons 'prune-save-space?
                  #$(borg-backup-job-prune-save-space? config))
            (cons 'extra-prune-flags
                  (list #$@extra-prune-flags))
            (cons 'compact?
                  #$(borg-backup-job-compact? config))
            (cons 'compact-threshold
                  #$(let ((value (borg-backup-job-compact-threshold config)))
                      (and (maybe-value-set? value) value)))
            (cons 'compact-cleanup-commits?
                  #$(borg-backup-job-compact-cleanup-commits? config))
            (cons 'extra-compact-flags
                  (list #$@extra-compact-flags))
            (cons 'check-repository?
                  #$(borg-backup-job-check-repository? config))
            (cons 'check-archives?
                  #$(borg-backup-job-check-archives? config))
            (cons 'check-verify-data?
                  #$(borg-backup-job-check-verify-data? config))
            (cons 'check-max-duration
                  #$(let ((value (borg-backup-job-check-max-duration config)))
                      (and (maybe-value-set? value) value)))
            (cons 'extra-check-flags
                  (list #$@extra-check-flags)))))

(define borg-program
  #~(lambda (action job . action-args)
      (use-modules (ice-9 format)
                   (ice-9 match)
                   (ice-9 textual-ports)
                   (srfi srfi-1))

      (define (job-ref field)
        (assoc-ref job field))

      (define (stringify value)
        (if (number? value)
            (number->string value) value))

      (define (maybe-option flag value)
        (if value
            (list flag
                  (stringify value))
            '()))

      (define (maybe-flag flag enabled?)
        (if enabled?
            (list flag)
            '()))

      (define (repeated-option flag values)
        (append-map (lambda (value)
                      (list flag value)) values))

      (define (render-command command)
        (string-join (map (lambda (argument)
                            (if (or (string-contains argument " ")
                                    (string-contains argument "\t"))
                                (format #f "~s" argument) argument)) command)
                     " "))

      (define (set-name=value-environment-variable variable)
        (let ((separator (string-index variable #\=)))
          (unless separator
            (error "Environment variable must use NAME=VALUE syntax" variable))
          (setenv (substring variable 0 separator)
                  (substring variable
                             (+ separator 1)))))

      (define (apply-job-environment!)
        (for-each set-name=value-environment-variable
                  (job-ref 'environment-variables))
        (setenv "BORG_REPO"
                (job-ref 'repository))
        (when (job-ref 'remote-path)
          (setenv "BORG_REMOTE_PATH"
                  (job-ref 'remote-path)))
        (when (job-ref 'rsh-command)
          (setenv "BORG_RSH"
                  (job-ref 'rsh-command))))

      (define (command-exit-status status)
        (or (status:exit-val status)
            (let ((signal (status:term-sig status)))
              (and signal
                   (+ 128 signal))) 1))

      (define (borg-command-prefix)
        (append (list (job-ref 'borg))
                (maybe-flag "--info"
                            (job-ref 'verbose?))
                (maybe-flag "--progress"
                            (job-ref 'progress?))
                (maybe-flag "--show-rc"
                            (job-ref 'show-rc?))
                (maybe-option "--lock-wait"
                              (job-ref 'lock-wait))
                (maybe-option "--remote-path"
                              (job-ref 'remote-path))
                (maybe-option "--rsh"
                              (job-ref 'rsh-command))
                (job-ref 'extra-options)))

      (define (archive-location)
        (string-append (job-ref 'repository) "::"
                       (job-ref 'archive-name-format)))

      (define (init-command)
        (append (borg-command-prefix)
                (list "init" "--encryption"
                      (job-ref 'encryption))
                (maybe-flag "--append-only"
                            (job-ref 'append-only?))
                (maybe-flag "--make-parent-dirs"
                            (job-ref 'make-parent-directories?))
                (job-ref 'extra-init-flags)
                (list (job-ref 'repository))))

      (define (create-command)
        (append (borg-command-prefix)
                (list "create")
                (maybe-flag "--stats"
                            (job-ref 'stats?))
                (maybe-option "--compression"
                              (job-ref 'compression))
                (maybe-flag "--exclude-caches"
                            (job-ref 'exclude-caches?))
                (maybe-flag "--keep-exclude-tags"
                            (job-ref 'keep-exclude-tags?))
                (maybe-flag "--one-file-system"
                            (job-ref 'one-file-system?))
                (repeated-option "--pattern"
                                 (job-ref 'patterns))
                (repeated-option "--patterns-from"
                                 (job-ref 'patterns-files))
                (repeated-option "--exclude"
                                 (job-ref 'exclude-patterns))
                (repeated-option "--exclude-from"
                                 (job-ref 'exclude-files))
                (repeated-option "--exclude-if-present"
                                 (job-ref 'exclude-if-present))
                (job-ref 'extra-create-flags)
                (list (archive-location))
                (job-ref 'paths)))

      (define (prune-command)
        (append (borg-command-prefix)
                (list "prune")
                (maybe-flag "--stats"
                            (job-ref 'stats?))
                (maybe-option "--glob-archives"
                              (job-ref 'archive-match-pattern))
                (maybe-option "--keep-within"
                              (job-ref 'prune-keep-within))
                (maybe-option "--keep-last"
                              (job-ref 'prune-keep-last))
                (maybe-option "--keep-secondly"
                              (job-ref 'prune-keep-secondly))
                (maybe-option "--keep-minutely"
                              (job-ref 'prune-keep-minutely))
                (maybe-option "--keep-hourly"
                              (job-ref 'prune-keep-hourly))
                (maybe-option "--keep-daily"
                              (job-ref 'prune-keep-daily))
                (maybe-option "--keep-weekly"
                              (job-ref 'prune-keep-weekly))
                (maybe-option "--keep-monthly"
                              (job-ref 'prune-keep-monthly))
                (maybe-option "--keep-13weekly"
                              (job-ref 'prune-keep-quarterly-13weekly))
                (maybe-option "--keep-3monthly"
                              (job-ref 'prune-keep-quarterly-3monthly))
                (maybe-option "--keep-yearly"
                              (job-ref 'prune-keep-yearly))
                (maybe-flag "--save-space"
                            (job-ref 'prune-save-space?))
                (job-ref 'extra-prune-flags)
                (list (job-ref 'repository))))

      (define (compact-command)
        (append (borg-command-prefix)
                (list "compact")
                (maybe-option "--threshold"
                              (job-ref 'compact-threshold))
                (maybe-flag "--cleanup-commits"
                            (job-ref 'compact-cleanup-commits?))
                (job-ref 'extra-compact-flags)
                (list (job-ref 'repository))))

      (define (check-command)
        (append (borg-command-prefix)
                (list "check")
                (cond
                  ((and (job-ref 'check-repository?)
                        (not (job-ref 'check-archives?)))
                   '("--repository-only"))
                  ((and (not (job-ref 'check-repository?))
                        (job-ref 'check-archives?))
                   '("--archives-only"))
                  (else '()))
                (maybe-flag "--verify-data"
                            (job-ref 'check-verify-data?))
                (maybe-option "--max-duration"
                              (job-ref 'check-max-duration))
                (if (job-ref 'check-archives?)
                    (maybe-option "--glob-archives"
                                  (job-ref 'archive-match-pattern))
                    '())
                (job-ref 'extra-check-flags)
                (list (job-ref 'repository))))

      (define (job-prune-enabled?)
        (and (job-ref 'prune?)
             (or (job-ref 'prune-keep-within)
                 (job-ref 'prune-keep-last)
                 (job-ref 'prune-keep-secondly)
                 (job-ref 'prune-keep-minutely)
                 (job-ref 'prune-keep-hourly)
                 (job-ref 'prune-keep-daily)
                 (job-ref 'prune-keep-weekly)
                 (job-ref 'prune-keep-monthly)
                 (job-ref 'prune-keep-quarterly-13weekly)
                 (job-ref 'prune-keep-quarterly-3monthly)
                 (job-ref 'prune-keep-yearly)
                 (pair? (job-ref 'extra-prune-flags)))))

      (define (job-compact-enabled?)
        (and (job-ref 'compact?)
             (or (job-prune-enabled?)
                 (job-ref 'compact-threshold)
                 (job-ref 'compact-cleanup-commits?)
                 (pair? (job-ref 'extra-compact-flags)))))

      (define (local-repository? repository)
        (and (not (string-contains repository "://"))
             (or (string-prefix? "/" repository)
                 (string-prefix? "./" repository)
                 (string-prefix? "../" repository)
                 (string-prefix? "~/" repository)
                 (not (string-index repository #\:)))))

      (define (expand-home repository)
        (if (and (string-prefix? "~/" repository)
                 (getenv "HOME"))
            (string-append (getenv "HOME")
                           (substring repository 1)) repository))

      (define (repository-needs-initialization?)
        (let ((repository (job-ref 'repository)))
          (and (job-ref 'initialize-repository?)
               (local-repository? repository)
               (not (file-exists? (string-append (expand-home repository)
                                                 "/config"))))))

      (define (run-borg-command command)
        (when (job-ref 'verbose?)
          (format #t "Running ~a~%"
                  (render-command command)))
        (let ((pid (primitive-fork)))
          (if (zero? pid)
              (begin
                (apply-job-environment!)
                (when (job-ref 'passcommand)
                  (setenv "BORG_PASSCOMMAND"
                          (job-ref 'passcommand)))
                (when (job-ref 'passphrase-file)
                  (let ((port (open-input-file (job-ref 'passphrase-file))))
                    (setenv "BORG_PASSPHRASE_FD"
                            (number->string (fileno port)))
                    (apply execlp
                           (car command) command)))
                (apply execlp
                       (car command) command))
              (command-exit-status (cdr (waitpid pid))))))

      (define (run-sequence commands)
        (let loop
          ((remaining commands))
          (match remaining
            (() 0)
            ((command . rest) (let ((status (run-borg-command command)))
                                (if (zero? status)
                                    (loop rest) status))))))

      (define (backup-status)
        (let ((prelude-status (if (repository-needs-initialization?)
                                  (begin
                                    (format #t
                                     "Initializing missing Borg repository for job '~a'.~%"
                                     (job-ref 'name))
                                    (run-borg-command (init-command))) 0)))
          (if (zero? prelude-status)
              (run-sequence (append (list (create-command))
                                    (if (job-prune-enabled?)
                                        (list (prune-command))
                                        '())
                                    (if (job-compact-enabled?)
                                        (list (compact-command))
                                        '()))) prelude-status)))

      (define (usage-error)
        (error (string-append "Unknown action: " action "\n\n"
                "Possible actions are: backup, run, init, create, prune, compact, check, borg")))

      (match action
        ((or "backup" "run")
         (backup-status))
        ("init" (run-borg-command (init-command)))
        ("create" (run-borg-command (create-command)))
        ("prune" (if (job-prune-enabled?)
                     (run-borg-command (prune-command))
                     (error "No prune policy configured for Borg job"
                            (job-ref 'name))))
        ("compact" (if (job-compact-enabled?)
                       (run-borg-command (compact-command))
                       (error "No compact action configured for Borg job"
                              (job-ref 'name))))
        ("check" (run-borg-command (check-command)))
        ("borg" (if (null? action-args)
                    (error "borg action requires a Borg command"
                           (job-ref 'name))
                    (run-borg-command (append (borg-command-prefix)
                                              action-args))))
        (_ (usage-error)))))

(define (borg-backup-job-program config)
  (program-file "borg-backup"
                #~(begin
                    (use-modules (ice-9 match))

                    (define borg-exec
                      #$borg-program)
                    (define job
                      #$(lower-borg-backup-job config))

                    (match (command-line)
                      ((_)
                       (exit (borg-exec "backup" job)))
                      ((_ action args ...)
                       (exit (apply borg-exec action job args)))))))

(define (borg-guix jobs)
  (program-file "borg-guix"
                #~(begin
                    (use-modules (ice-9 match)
                                 (srfi srfi-1))

                    (define names
                      '#$(map borg-backup-job-name jobs))
                    (define programs
                      '#$(map borg-backup-job-program jobs))

                    (define (get-program name)
                      (define idx
                        (list-index (lambda (candidate)
                                      (string=? candidate name)) names))
                      (unless idx
                        (error (string-append "Unknown job name " name "\n\n"
                                              "Possible job names are: "
                                              (string-join names " "))))
                      (list-ref programs idx))

                    (define (dispatch name action arguments)
                      (define program
                        (get-program name))
                      (apply execlp program
                             (append (list program action) arguments)))

                    (define (usage command)
                      (string-append "Usage: "
                                     command
                                     " backup NAME\n"
                                     "       "
                                     command
                                     " run NAME\n"
                                     "       "
                                     command
                                     " init NAME\n"
                                     "       "
                                     command
                                     " create NAME\n"
                                     "       "
                                     command
                                     " prune NAME\n"
                                     "       "
                                     command
                                     " compact NAME\n"
                                     "       "
                                     command
                                     " check NAME\n"
                                     "       "
                                     command
                                     " borg NAME COMMAND [ARGS...]"))

                    (define (require-name args)
                      (when (< (length args) 3)
                        (error (usage (basename (car args))))))

                    (define (main args)
                      (require-name args)
                      (match args
                        ((_ (or "backup"
                                "run"
                                "init"
                                "create"
                                "prune"
                                "compact"
                                "check") name)
                         (dispatch name
                                   (second args)
                                   '()))
                        ((_ "borg" name command more ...)
                         (dispatch name "borg"
                                   (cons command more)))
                        ((_ . _) (error (usage (basename (car args)))))))

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

(define* (borg-backup-job-command name action
                                  #:key (home-service? #f))
  (if home-service?
      #~(list "borg-guix"
              #$action
              #$name)
      #~(list (string-append #$bash-minimal "/bin/bash") "-l" "-c"
              (string-append "borg-guix "
                             #$action " "
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
          ;; For %user-log-dir.
          '((shepherd support))
          '())))

(define (borg-backup-job-service-symbol name role)
  (string->symbol (string-append "borg-" role "-" name)))

(define* (borg-backup-job->shepherd-service config
                                            #:key (home-service? #f))
  (let ((schedule (borg-backup-job-schedule config))
        (name (borg-backup-job-name config))
        (user (borg-backup-job-user config))
        (group (borg-backup-job-group config))
        (max-duration (borg-backup-job-max-duration config))
        (wait-for-termination? (borg-backup-job-wait-for-termination? config))
        (log-file (borg-job-log-file config
                                     #:home-service? home-service?))
        (requirement (borg-job-requirement config
                                           #:home-service? home-service?)))
    (shepherd-service (provision (list (borg-backup-job-service-symbol name
                                        "backup")))
                      (requirement requirement)
                      (documentation "Run Borg backups on a regular basis.")
                      (modules (borg-backup-job-modules #:home-service?
                                                        home-service?))
                      (start #~(make-timer-constructor (if (string? #$schedule)
                                                           (cron-string->calendar-event #$schedule)
                                                           #$schedule)
                                                       (command #$(borg-backup-job-command
                                                                   name
                                                                   "backup"
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
                                                                             (getpwnam #$user))))))
                                                                #:log-file #$log-file
                                                                #:wait-for-termination? #$wait-for-termination?
                                                                #:max-duration #$
                                                                (and (maybe-value-set?
                                                                      max-duration)
                                                                 max-duration))))
                      (stop #~(make-timer-destructor))
                      (actions (list (shepherd-action (inherit
                                                       shepherd-trigger-action)
                                                      (documentation
                                                       "Manually trigger a backup without waiting for the schedule.")))))))

(define* (borg-check-job->shepherd-service config
                                           #:key (home-service? #f))
  (let ((schedule (borg-backup-job-check-schedule config))
        (name (borg-backup-job-name config))
        (user (borg-backup-job-user config))
        (group (borg-backup-job-group config))
        (max-duration (borg-backup-job-max-duration config))
        (wait-for-termination? (borg-backup-job-wait-for-termination? config))
        (log-file (borg-job-log-file config
                                     #:home-service? home-service?))
        (requirement (borg-job-requirement config
                                           #:home-service? home-service?)))
    (and (maybe-value-set? schedule)
         (shepherd-service (provision (list (borg-backup-job-service-symbol
                                             name "check")))
                           (requirement requirement)
                           (documentation
                            "Run Borg integrity checks on a regular basis.")
                           (modules (borg-backup-job-modules #:home-service?
                                                             home-service?))
                           (start #~(make-timer-constructor (if (string? #$schedule)
                                                                (cron-string->calendar-event #$schedule)
                                                                #$schedule)
                                                            (command #$(borg-backup-job-command
                                                                        name
                                                                        "check"
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
                                                                             (getpwnam #$user))))))
                                                                     #:log-file #$log-file
                                                                     #:wait-for-termination? #$wait-for-termination?
                                                                     #:max-duration #$
                                                                     (and (maybe-value-set?
                                                                           max-duration)
                                                                      max-duration))))
                           (stop #~(make-timer-destructor))
                           (actions (list (shepherd-action (inherit
                                                            shepherd-trigger-action)
                                                           (documentation
                                                            "Manually trigger a Borg integrity check."))))))))

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
    (synopsis "Interact with Guix-configured Borg backup jobs")
    (description
     "This package provides the @command{borg-guix} helper managed by
@code{borg-backup-service-type}.  It can trigger configured backup jobs,
initialize repositories, run scheduled integrity checks on demand, or execute
arbitrary Borg commands with the job's repository and authentication settings.")
    (license license:gpl3+)))

(define (borg-backup-service-profile config)
  (define jobs
    (validated-borg-backup-jobs config))
  (if (pair? jobs)
      (list (borg-guix-wrapper-package jobs))
      '()))

(define borg-backup-service-type
  (service-type (name 'borg-backup)
                (extensions (list (service-extension profile-service-type
                                   borg-backup-service-profile)
                                  (service-extension
                                   shepherd-root-service-type
                                   (match-record-lambda <borg-backup-configuration>
                                     (jobs home-service?)
                                     (append-map (lambda (job)
                                                   (let ((check-service (borg-check-job->shepherd-service
                                                                         job
                                                                         #:home-service?
                                                                         home-service?)))
                                                     (append (list (borg-backup-job->shepherd-service
                                                                    job
                                                                    #:home-service?
                                                                    home-service?))
                                                             (if check-service
                                                              (list
                                                               check-service)
                                                              '()))))
                                                 (validate-borg-backup-jobs
                                                  jobs))))))
                (compose concatenate)
                (extend (lambda (config jobs)
                          (borg-backup-configuration (inherit config)
                                                     (jobs (append (borg-backup-configuration-jobs
                                                                    config)
                                                                   jobs)))))
                (default-value (borg-backup-configuration))
                (description
                 "This service configures Shepherd timers for production-oriented Borg
backups and optional scheduled repository checks.")))
