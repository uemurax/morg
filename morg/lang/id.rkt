#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide (rename-out [get-current-id current-id]))

(module parameter typed/racket
  (provide current-id)

  (define current-id : (Parameterof String)
    (make-parameter "!!!Invalid id!!!")))

(require 'parameter)

(module+ test
  (require "../data/id.rkt")
  (check-exn
   exn:fail?
   (lambda ()
     (id (current-id)))))

(define (get-current-id)
  (current-id))
