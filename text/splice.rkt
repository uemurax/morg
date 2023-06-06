#lang typed/racket

(require "../data/splice.rkt"
         "../markup/string.rkt")

(provide splice->text)

(define #:forall (X)
        ((splice->text [f : (X . -> . StringTree)])
         [x : (Splice X)]) : StringTree
  (string~ (splice-map f x)))
