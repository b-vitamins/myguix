;;; Nougat OCR Home Service for GNU Guix
;;; Copyright © 2024 Ayan Das <bvits@riseup.net>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This module provides a Guix Home service for running Nougat OCR
;;; (Neural Optical Understanding for Academic Documents) as an API server
;;; in user space using GNU Shepherd.
;;;
;;; Nougat is a Meta AI tool that converts academic PDFs to markdown format,
;;; particularly effective with scientific papers containing mathematical
;;; expressions and complex layouts.
;;;
;;; Usage:
;;;
;;; In your home configuration file:
;;;
;;;   (use-modules (gnu home)
;;;                (gnu home services)
;;;                (myguix home services nougat))
;;;
;;;   (home-environment
;;;     (services
;;;       (list (service home-nougat-service-type
;;;                      (nougat-configuration
;;;                       (model "0.1.0-base")
;;;                       (port 8080)
;;;                       (host "127.0.0.1")))
;;;             ;; other services...
;;;             )))
;;;
;;; After reconfiguration, control the service with:
;;;
;;;   herd start nougat-api
;;;   herd status nougat-api
;;;   herd stop nougat-api
;;;
;;; The API will be available at http://localhost:8080
;;;
;;; Code:

(define-module (myguix home services nougat)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages python-web)
  #:use-module (myguix packages nlp)
  #:use-module (ice-9 match)
  #:export (nougat-configuration nougat-configuration?
                                 nougat-configuration-package
                                 nougat-configuration-model
                                 nougat-configuration-host
                                 nougat-configuration-port
                                 nougat-configuration-batch-size
                                 nougat-configuration-workers
                                 nougat-configuration-cache-dir
                                 nougat-configuration-log-level
                                 nougat-configuration-extra-options
                                 home-nougat-service-type
                                 nougat-api-service))

;;; Configuration record definition

(define-configuration/no-serialization nougat-configuration
                                       (package
                                         (file-like python-nougat-ocr)
                                         "The nougat package to use. This should be a package object containing
the nougat OCR software. Defaults to @code{python-nougat-ocr}.")

                                       (model (string "0.1.0-base")
                                        "The nougat model to use for OCR processing. Available options are:
@itemize
@item @code{\"0.1.0-base\"} - Larger, more accurate model (~1.3GB)
@item @code{\"0.1.0-small\"} - Smaller, faster model (~956MB)
@end itemize
The base model provides better accuracy for complex academic documents
but requires more memory and processing time.")

                                       (host (string "127.0.0.1")
                                        "The host address to bind the API server to. Use @code{\"127.0.0.1\"}
for localhost-only access or @code{\"0.0.0.0\"} to accept connections
from any network interface. For security, localhost is recommended
unless you specifically need remote access.")

                                       (port (integer 8503)
                                        "The port number for the API server. Choose a port that doesn't
conflict with other services. Defaults to 8503 (nougat's default).")

                                       (batch-size (integer 1)
                                        "Batch size for processing PDF pages. Higher values can improve
throughput but require more memory. Start with 1 and increase if
you have sufficient RAM and are processing many documents.")

                                       (workers (integer 1)
                                        "Number of worker processes to spawn. Multiple workers can process
requests in parallel. Set this to the number of CPU cores for
optimal performance, but be aware that each worker loads the model
into memory.")

                                       (cache-dir (string "~/.cache/nougat")
                                        "Directory for caching downloaded models and temporary files.
The models are large (1GB+) so ensure sufficient disk space.
The directory will be created automatically if it doesn't exist.")

                                       (log-level (string "INFO")
                                        "Logging verbosity level. Available options:
@itemize
@item @code{\"DEBUG\"} - Most verbose, useful for troubleshooting
@item @code{\"INFO\"} - Standard operational messages
@item @code{\"WARNING\"} - Important warnings only
@item @code{\"ERROR\"} - Error messages only
@end itemize")

                                       (extra-options (list '())
                                        "List of additional command-line options to pass to nougat.
Each option should be a string. For example:
@code{(list \"--full-precision\" \"--no-skipping\")}"))

;;; Service implementation

(define (nougat-shepherd-service config)
  "Return a shepherd service configuration for running nougat in API mode."
  (let ((package
          (nougat-configuration-package config))
        (model (nougat-configuration-model config))
        (host (nougat-configuration-host config))
        (port (nougat-configuration-port config))
        (batch-size (nougat-configuration-batch-size config))
        (workers (nougat-configuration-workers config))
        (cache-dir (nougat-configuration-cache-dir config))
        (log-level (nougat-configuration-log-level config))
        (extra-options (nougat-configuration-extra-options config)))
    
    (list (shepherd-service (provision '(nougat-api))
                            (documentation
                             "Run nougat OCR in API mode for PDF to markdown conversion")
                            (requirement '())
                            (start #~(make-forkexec-constructor (append (if (= #$port
                                                                             8503)
                                                                            (list #$
                                                                             (file-append
                                                                              package
                                                                              "/bin/nougat_api"))
                                                                            (list #$
                                                                             (file-append
                                                                              package
                                                                              "/bin/uvicorn")
                                                                             "nougat.app:app"
                                                                             "--host"
                                                                             #$host
                                                                             "--port"
                                                                             #$
                                                                             (number->string
                                                                              port)
                                                                             "--workers"
                                                                             #$
                                                                             (number->string
                                                                              workers)))
                                                                        #$@extra-options)
                                                                #:log-file (string-append
                                                                            (or
                                                                             (getenv
                                                                              "XDG_LOG_HOME")
                                                                             (string-append
                                                                              (getenv
                                                                               "HOME")
                                                                              "/.local/var/log"))
                                                                            "/nougat-api.log")
                                                                #:environment-variables
                                                                (list (string-append
                                                                       "NOUGAT_CACHE_DIR="
                                                                       #$cache-dir)
                                                                      (string-append
                                                                       "NOUGAT_CHECKPOINT="
                                                                       #$model)
                                                                      (string-append
                                                                       "NOUGAT_BATCHSIZE="
                                                                       #$(number->string
                                                                          batch-size))
                                                                      (string-append
                                                                       "PYTHONPATH="
                                                                       #$(file-append
                                                                          package
                                                                          "/lib/python3.11/site-packages"))
                                                                      (string-append
                                                                       "LD_LIBRARY_PATH="
                                                                       (or (getenv
                                                                            "LIBRARY_PATH")
                                                                        "")))))
                            (stop #~(make-kill-destructor))
                            (actions (list (shepherd-action (name 'status-api)
                                                            (documentation
                                                             "Check API server status and display connection info")
                                                            (procedure #~(lambda (running . args)
                                                                           (if
                                                                            running
                                                                            (format
                                                                             #t
                                                                             "Nougat API is running on ~a:~a~%Model: ~a~%Log level: ~a~%Workers: ~a~%"
                                                                             #$host
                                                                             #$port
                                                                             #$model
                                                                             #$log-level
                                                                             #$workers)
                                                                            (format
                                                                             #t
                                                                             "Nougat API is not running~%"))
                                                                           running)))
                                           (shepherd-action (name 'show-config)
                                                            (documentation
                                                             "Display current configuration")
                                                            (procedure #~(lambda (running . args)
                                                                           (format
                                                                            #t
                                                                            "Nougat API Configuration:~%")
                                                                           (format
                                                                            #t
                                                                            "  Status: ~a~%"
                                                                            (if
                                                                             running
                                                                             "Running"
                                                                             "Stopped"))
                                                                           (format
                                                                            #t
                                                                            "  Host: ~a~%"
                                                                            #$host)
                                                                           (format
                                                                            #t
                                                                            "  Port: ~a~%"
                                                                            #$port)
                                                                           (format
                                                                            #t
                                                                            "  Model: ~a~%"
                                                                            #$model)
                                                                           (format
                                                                            #t
                                                                            "  Batch size: ~a~%"
                                                                            #$batch-size)
                                                                           (format
                                                                            #t
                                                                            "  Workers: ~a~%"
                                                                            #$workers)
                                                                           (format
                                                                            #t
                                                                            "  Cache dir: ~a~%"
                                                                            #$cache-dir)
                                                                           (format
                                                                            #t
                                                                            "  Log level: ~a~%"
                                                                            #$log-level)
                                                                           (format
                                                                            #t
                                                                            "  Extra options: ~a~%"
                                                                            '#$extra-options)
                                                                           #t)))
                                           (shepherd-action (name 'test-api)
                                                            (documentation
                                                             "Test API connectivity")
                                                            (procedure #~(lambda (running . args)
                                                                           (if
                                                                            running
                                                                            (begin
                                                                              
                                                                              
                                                                              (format
                                                                               #t
                                                                               "Testing API connectivity...~%")

                                                                              
                                                                              (format
                                                                               #t
                                                                               "Try: curl -X POST http://~a:~a/predict/ -F 'file=@document.pdf'~%"
                                                                               #$host
                                                                               #$port)

                                                                              
                                                                              (format
                                                                               #t
                                                                               "Or check root: curl http://~a:~a/~%"
                                                                               #$host
                                                                               #$port)
                                                                              #t)
                                                                            (begin
                                                                              
                                                                              
                                                                              (format
                                                                               #t
                                                                               "Nougat API is not running. Start it first with: herd start nougat-api~%")
                                                                              #f)))))))))))

;;; Service type definition

(define home-nougat-service-type
  (service-type (name 'home-nougat)
                (description
                 "Run nougat OCR as an API service in user space using GNU Shepherd.
Nougat is a neural optical understanding tool for academic documents
that converts PDF files to markdown format, preserving mathematical
expressions and document structure.")
                (extensions (list (service-extension
                                   home-shepherd-service-type
                                   nougat-shepherd-service)))
                (default-value (nougat-configuration))))

;;; Convenience procedures

(define* (nougat-api-service #:key (model "0.1.0-base")
                             (port 8503)
                             (host "127.0.0.1")
                             (batch-size 1)
                             (workers 1)
                             (log-level "INFO")
                             (cache-dir "~/.cache/nougat")
                             (extra-options '()))
  "Return a nougat API service with the specified configuration.

This is a convenience procedure for creating a nougat service without
having to construct a full @code{nougat-configuration} record.

@table @code
@item model
The nougat model to use (@code{\"0.1.0-base\"} or @code{\"0.1.0-small\"})
@item port
Port number for the API server (default: 8080)
@item host
Host address to bind to (default: \"127.0.0.1\")
@item batch-size
Number of PDF pages to process in each batch (default: 1)
@item workers
Number of worker processes (default: 1)
@item log-level
Logging verbosity (\"DEBUG\", \"INFO\", \"WARNING\", \"ERROR\")
@item cache-dir
Directory for caching models and temporary files
@item extra-options
List of additional command-line options
@end table

Example:
@lisp
(nougat-api-service #:model \"0.1.0-small\"
                   #:port 8503
                   #:workers 2
                   #:log-level \"DEBUG\")
@end lisp"
  (service home-nougat-service-type
           (nougat-configuration (model model)
                                 (port port)
                                 (host host)
                                 (batch-size batch-size)
                                 (workers workers)
                                 (log-level log-level)
                                 (cache-dir cache-dir)
                                 (extra-options extra-options))))

;;; Example usage and documentation
