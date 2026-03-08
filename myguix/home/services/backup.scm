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

(define-module (myguix home services backup)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (myguix services backup)
  #:export (home-borg-backup-service-type)
  #:re-export (borg-backup-job borg-backup-job?
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

                               borg-backup-configuration
                               borg-backup-configuration?
                               borg-backup-configuration-jobs))

;;; Commentary:
;;;
;;; Home wrapper for the Borg backup service.
;;;
;;; Code:

(define home-borg-backup-service-type
  (service-type (inherit (system->home-service-type borg-backup-service-type))
                (extend (lambda (config jobs)
                          (for-home (borg-backup-configuration (inherit config)
                                                               (jobs (append (borg-backup-configuration-jobs
                                                                              config)
                                                                      jobs))))))
                (default-value (for-home (borg-backup-configuration)))))
