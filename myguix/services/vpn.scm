;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Alexey Abramov <levenson@mmer.org>

(define-module (myguix services vpn)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (ice-9 match)
  #:use-module (myguix packages vpn)
  #:export (tailscale-configuration
            tailscale-configuration?
            tailscale-service-type
            tailscale-service
            zerotier-one-service))

(define-record-type* <tailscale-configuration>
  tailscale-configuration make-tailscale-configuration
  tailscale-configuration?
  (tailscale tailscale-configuration-tailscale
             (default tailscale))
  (state-directory tailscale-configuration-state-directory
                   (default "/var/lib/tailscale"))
  (socket tailscale-configuration-socket
          (default "/var/run/tailscale/tailscaled.sock"))
  (port tailscale-configuration-port
        (default "41641"))
  (extra-options tailscale-configuration-extra-options
                 (default '())))

(define (tailscale-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(tailscale-configuration-state-directory config))
      (mkdir-p #$(dirname (tailscale-configuration-socket config)))))

(define (tailscale-shepherd-service config)
  (match-record config <tailscale-configuration>
    (tailscale state-directory socket port extra-options)
    (list
     (shepherd-service
      (documentation "Run the Tailscale daemon.")
      (provision '(tailscaled))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append tailscale "/sbin/tailscaled")
                      #$(string-append "--state=" state-directory
                                       "/tailscaled.state")
                      #$(string-append "--socket=" socket)
                      #$(string-append "--port=" port)
                      #$@extra-options)
                #:log-file "/var/log/tailscaled.log"))
      (stop #~(make-kill-destructor))))))

(define tailscale-service-type
  (service-type
   (name 'tailscale)
   (description "Run the Tailscale mesh VPN daemon.")
   (extensions (list (service-extension activation-service-type
                                        tailscale-activation)
                     (service-extension profile-service-type
                                        (lambda (config)
                                          (list
                                           (tailscale-configuration-tailscale
                                            config))))
                     (service-extension shepherd-root-service-type
                                        tailscale-shepherd-service)))
   (default-value (tailscale-configuration))))

(define* (tailscale-service #:key (config (tailscale-configuration)))
  (service tailscale-service-type config))

(define %zerotier-action-join
  (shepherd-action (name 'join)
                   (documentation "Join a network")
                   (procedure #~(lambda (running network)
                                  (let* ((zerotier-cli (string-append #$zerotier
                                                        "/sbin/zerotier-cli"))
                                         (cmd (string-join (list zerotier-cli
                                                                 "join"
                                                                 network)))
                                         (port (open-input-pipe cmd))
                                         (str (get-string-all port)))
                                    (display str)
                                    (status:exit-val (close-pipe port)))))))

(define %zerotier-action-leave
  (shepherd-action (name 'leave)
                   (documentation "Leave a network")
                   (procedure #~(lambda (running network)
                                  (let* ((zerotier-cli (string-append #$zerotier
                                                        "/sbin/zerotier-cli"))
                                         (cmd (string-join (list zerotier-cli
                                                                 "leave"
                                                                 network)))
                                         (port (open-input-pipe cmd))
                                         (str (get-string-all port)))
                                    (display str)
                                    (status:exit-val (close-pipe port)))))))

(define zerotier-one-shepherd-service
  (lambda (config)
    (list (shepherd-service (documentation "ZeroTier One daemon.")
                            (provision '(zerotier-one))
                            (requirement '(networking))
                            (actions (list %zerotier-action-join
                                           %zerotier-action-leave))
                            (start #~(make-forkexec-constructor (list (string-append #$zerotier
                                                                       "/sbin/zerotier-one"))))
                            (stop #~(make-kill-destructor))))))

(define zerotier-one-service-type
  (service-type (name 'zerotier-one)
                (description "ZeroTier One daemon.")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   zerotier-one-shepherd-service)))))

(define* (zerotier-one-service #:key (config (list)))
  (service zerotier-one-service-type config))
