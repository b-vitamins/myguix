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

(define-module (myguix home services sops)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (myguix services sops)
  #:export (home-sops-secrets-service-type))

;;; Commentary:
;;;
;;; This module provides a home service wrapper for the SOPS secrets service.
;;; It uses the system->home-service-type pattern to run SOPS in the user context.
;;;
;;; Code:

(define home-sops-secrets-service-type
  (service-type (inherit (system->home-service-type sops-secrets-service-type))
                (default-value #f)))
