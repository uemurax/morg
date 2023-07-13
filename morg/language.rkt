#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide current-id)

(define current-id : (Parameterof String)
  (make-parameter "!!!Invalid id!!!"))

(module+ test
  (require "data/id.rkt")
  (check-exn
   exn:fail?
   (lambda ()
     (id (current-id)))))
