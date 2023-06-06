#lang typed/racket

(require "../data/splice.rkt")

(provide ~
         when~)

(define #:forall (A)
        (~ . [xs : A *]) : (Splice A)
  (splice xs))

(define-syntax-rule (when~ cond body ...)
  (if cond
      (~ body ...)
      (~)))
