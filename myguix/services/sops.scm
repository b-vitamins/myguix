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

(define-module (myguix services sops)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (myguix packages sops)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (sops-secret sops-secret?
                        sops-secret-key
                        sops-secret-file
                        sops-secret-user
                        sops-secret-group
                        sops-secret-permissions
                        sops-secret-output-type
                        sops-secret-path

                        sops-service-configuration
                        sops-service-configuration?
                        sops-service-configuration-sops
                        sops-service-configuration-gnupg
                        sops-service-configuration-config
                        sops-service-configuration-gnupg-home
                        sops-service-configuration-age-key-file
                        sops-service-configuration-generate-key?
                        sops-service-configuration-host-ssh-key
                        sops-service-configuration-secrets-directory
                        sops-service-configuration-secrets
                        sops-service-configuration-verbose?
                        sops-service-configuration-home-service?

                        sops-secrets-service-type
                        key->file-name))

;;; Commentary:
;;;
;;; This module provides a service for managing secrets with SOPS (Secrets OPerationS).
;;; It supports decrypting secrets at boot time and placing them in a tmpfs/ramfs
;;; filesystem so that cleartext secrets never hit the disk.
;;;
;;; Code:

(define-record-type* <sops-secret> sops-secret make-sops-secret
  sops-secret?
  (key sops-secret-key) ;string or list - the key in the secrets file
  (file sops-secret-file) ;file-like - the encrypted secrets file
  (user sops-secret-user
        (default "root")) ;string - owner user
  (group sops-secret-group
         (default "root")) ;string - owner group
  (permissions sops-secret-permissions
               (default 288)) ;number - file permissions
  (output-type sops-secret-output-type
               (default #f)) ;string or #f - json, yaml, dotenv, binary
  (path sops-secret-path
        (default #f)))
; string or #f - optional symlink path

(define-record-type* <sops-service-configuration> sops-service-configuration
                     make-sops-service-configuration
  sops-service-configuration?
  (sops sops-service-configuration-sops
        (default sops)) ;package
  (gnupg sops-service-configuration-gnupg
         (default (file-append gnupg "/bin/gpg"))) ;file-like
  (config sops-service-configuration-config) ;file-like - .sops.yaml config
  (gnupg-home sops-service-configuration-gnupg-home
              (default "/root/.gnupg")) ;string
  (age-key-file sops-service-configuration-age-key-file
                (default "/root/.config/sops/age/keys.txt")) ;string
  (generate-key? sops-service-configuration-generate-key?
                 (default #f)) ;boolean
  (host-ssh-key sops-service-configuration-host-ssh-key
                (default "/etc/ssh/ssh_host_rsa_key")) ;string
  (secrets-directory sops-service-configuration-secrets-directory
                     (default "/run/secrets")) ;string
  (secrets sops-service-configuration-secrets
           (default '())) ;list of <sops-secret>
  (verbose? sops-service-configuration-verbose?
            (default #f)) ;boolean
  (home-service? sops-service-configuration-home-service?
                 (default #f) ;boolean
                 (innate)))

(define (key->file-name key)
  "Convert a SOPS key to a file name."
  (match key
    ((? string? key)
     ;; Handle string keys like "[\"wireguard\"][\"private\"]"
     (string-join (filter-map (lambda (sub-key)
                                (and (not (string-null? sub-key))
                                     (string-trim-both (string-trim-both
                                                        sub-key #\[) #\])))
                              (string-split (string-filter (lambda (c)
                                                             (not (char=? c
                                                                          #\")))
                                                           key) #\])) "/"))
    ((? list? key)
     ;; Handle list keys like '("wireguard" "private")
     (string-join key "/"))))

(define (sops-activation config)
  "Return the activation script for SOPS secrets."
  (match-record config <sops-service-configuration>
    (sops gnupg
          gnupg-home
          age-key-file
          generate-key?
          host-ssh-key
          secrets-directory
          secrets
          verbose?
          home-service?)

    (with-imported-modules '((guix build utils))
                           #~(begin
                               (use-modules (guix build utils)
                                            (ice-9 match))

                               (define (decrypt-secret key
                                                       file
                                                       user
                                                       group
                                                       permissions
                                                       output-type
                                                       path)
                                 (let* ((key-str (match key
                                                   ((? string? k)
                                                    k)
                                                   ((? list? k)
                                                    (string-append (string-join
                                                                    (map (lambda 
                                                                                 (s)
                                                                           (string-append
                                                                            "[\""
                                                                            s
                                                                            "\"]"))
                                                                         k) "")))))
                                        (file-name #$(if home-service?
                                                         #~(string-append
                                                            "/run/user/"
                                                            (number->string (getuid))
                                                            "/secrets/"
                                                            (key->file-name
                                                             key))
                                                         #~(string-append #$secrets-directory
                                                                          "/"
                                                                          (key->file-name
                                                                           key))))
                                        (output-args (if output-type
                                                         (list "--output-type"
                                                          output-type)
                                                         '())))
                                   
                                   (mkdir-p (dirname file-name))

                                   ;; Decrypt the secret
                                   (apply invoke
                                          #$(file-append sops "/bin/sops")
                                          "-d"
                                          "--extract"
                                          key-str
                                          "--output"
                                          file-name
                                          `(,@output-args ,file))

                                   ;; Set ownership (only for system service)
                                   #$(if (not home-service?)
                                         #~(begin
                                             (let ((uid (passwd:uid (getpwnam
                                                                     user)))
                                                   (gid (group:gid (getgrnam
                                                                    group))))
                                               (chown file-name uid gid)))
                                         #~#t)

                                   ;; Set permissions
                                   (chmod file-name permissions)

                                   ;; Create symlink if requested
                                   (when path
                                     (symlink file-name path))))

                               ;; Set up environment
                               (setenv "GNUPGHOME"
                                       #$gnupg-home)
                               (setenv "SOPS_GPG_EXEC"
                                       #$gnupg)
                               (when (file-exists? #$age-key-file)
                                 (setenv "SOPS_AGE_KEY_FILE"
                                         #$age-key-file))

                               #$(if verbose?
                                     #~(begin
                                         (format #t
                                          "SOPS activation starting...~%")
                                         (format #t "GNUPGHOME: ~a~%"
                                                 (getenv "GNUPGHOME"))
                                         (format #t "SOPS_GPG_EXEC: ~a~%"
                                                 (getenv "SOPS_GPG_EXEC"))
                                         (when (getenv "SOPS_AGE_KEY_FILE")
                                           (format #t
                                                   "SOPS_AGE_KEY_FILE: ~a~%"
                                                   (getenv "SOPS_AGE_KEY_FILE"))))
                                     #~#t)

                               ;; Generate key if requested
                               #$(if generate-key?
                                     #~(when (file-exists? #$host-ssh-key)
                                         (format #t
                                          "Generating SOPS key from SSH host key...~%")
                                         ;; This is simplified - in reality you'd need ssh-to-pgp
                                         ;; or ssh-to-age here
                                         (format #t
                                          "Key generation not implemented yet~%"))
                                     #~#t)

                               ;; Set up secrets directory
                               (let ((secrets-dir #$(if home-service?
                                                        #~(string-append
                                                           "/run/user/"
                                                           (number->string (getuid))
                                                           "/secrets")
                                                        secrets-directory)))
                                 (unless (file-exists? secrets-dir)
                                   (mkdir-p secrets-dir))

                                 ;; Symlink .sops.yaml
                                 (let ((sops-yaml-link (string-append
                                                        secrets-dir
                                                        "/.sops.yaml")))
                                   (when (file-exists? sops-yaml-link)
                                     (delete-file sops-yaml-link))
                                   (symlink #$(sops-service-configuration-config
                                               config) sops-yaml-link))

                                 ;; Decrypt each secret
                                 (for-each (match-lambda
                                             ((key file
                                                   user
                                                   group
                                                   permissions
                                                   output-type
                                                   path)
                                              (decrypt-secret key
                                                              file
                                                              user
                                                              group
                                                              permissions
                                                              output-type
                                                              path)))
                                           (list #$@(map (lambda (secret)
                                                           #~(list #$(sops-secret-key
                                                                      secret)
                                                                   #$(sops-secret-file
                                                                      secret)
                                                                   #$(sops-secret-user
                                                                      secret)
                                                                   #$(sops-secret-group
                                                                      secret)
                                                                   #$(sops-secret-permissions
                                                                      secret)
                                                                   #$(sops-secret-output-type
                                                                      secret)
                                                                   #$(sops-secret-path
                                                                      secret)))
                                                         secrets))))))))

(define (sops-shepherd-service config)
  "Return the Shepherd service for SOPS secrets."
  (match-record config <sops-service-configuration>
    (secrets-directory home-service?)
    (list (shepherd-service (provision '(sops-secrets))
                            (requirement (if home-service?
                                             '()
                                             `(user-processes ,(string->symbol
                                                                (string-append
                                                                 "file-system-"
                                                                 secrets-directory)))))
                            (one-shot? #t)
                            (documentation "Decrypt SOPS secrets at startup.")
                            (start #~(lambda _
                                       #$(sops-activation config) #t))
                            (stop #~(const #f))))))

(define (sops-file-systems config)
  "Return the file systems for SOPS secrets."
  (match-record config <sops-service-configuration>
    (secrets-directory home-service?)
    (if home-service?
        '() ;Home service uses tmpfs at /run/user/UID
        (list (file-system
                (device "none")
                (mount-point secrets-directory)
                (type "ramfs")
                (check? #f))))))

(define (sops-profile-packages config)
  "Return packages to add to the system profile."
  (match-record config <sops-service-configuration>
    (sops)
    (list sops)))

(define sops-secrets-service-type
  (service-type (name 'sops-secrets)
                (description
                 "Manage secrets with SOPS, decrypting them at boot time into a 
tmpfs/ramfs filesystem.")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   sops-shepherd-service)
                                  (service-extension file-system-service-type
                                                     sops-file-systems)
                                  (service-extension profile-service-type
                                                     sops-profile-packages)))
                (default-value #f)
                (compose concatenate)
                (extend (lambda (config secrets)
                          (sops-service-configuration (inherit config)
                                                      (secrets (append (sops-service-configuration-secrets
                                                                        config)
                                                                secrets)))))))
