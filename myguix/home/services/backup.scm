;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Ayan Das <bvits@riseup.net>
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
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (myguix services backup)
  #:re-export (borg-backup-job borg-backup-job?
                               borg-backup-job-name
                               borg-backup-job-repository
                               borg-backup-job-passphrase-file
                               borg-backup-job-encryption
                               borg-backup-job-archive-name-format
                               borg-backup-job-schedule
                               borg-backup-job-paths
                               borg-backup-job-exclude-patterns
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

                               borg-backup-configuration
                               borg-backup-configuration?
                               borg-backup-configuration-jobs)
  #:export (home-borg-backup-service-type))

;;; Commentary:
;;;
;;; This module provides a home service wrapper for the Borg backup service.
;;;
;;; Code:

(define home-borg-backup-service-type
  (service-type (inherit (system->home-service-type borg-backup-service-type))
                (default-value (for-home (borg-backup-configuration)))))
