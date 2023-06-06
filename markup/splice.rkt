#lang typed/racket

(require "../data/splice.rkt")

(provide ~)

(define #:forall (A)
        (~ . [xs : A *]) : (Splice A)
  (splice xs))
