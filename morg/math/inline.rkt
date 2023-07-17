#lang typed/racket

(require "markup.rkt"
         "format.rkt"
         (prefix-in i: "../data/inline.rkt"))

(provide math)

(define (math . [xs : MathTeX+Like *]) : i:Math
  (define st (state #t))
  (i:math ((format-math-tex+ st) (apply math-tex+% xs))))
