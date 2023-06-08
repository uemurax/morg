#lang typed/racket

(require "../data/splice.rkt"
         "../markup/xexpr.rkt")

(provide splice->xexprs)

(define #:forall (X)
        ((splice->xexprs [f : (X . -> . XExprs)])
         [x : (Splice X)]) : XExprs
  (xexprs% (splice-map f x)))
