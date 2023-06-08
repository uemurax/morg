#lang typed/racket

(require "../data/splice.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../markup/tex.rkt")

(provide splice->latex)

(define #:forall (X)
        ((splice->latex [f : (X . -> . tex:TextTeX)])
         [x : (Splice X)]) : tex:TextTeX
  (text-tex% (splice-map f x)))
