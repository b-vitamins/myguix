(define-module (myguix services mcron)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services mcron)
  #:export (%garbage-collector-job))

(define %garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  #~(job "5 0 * * *" "guix gc -F 20G"))
