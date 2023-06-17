#lang typed/racket

(require "tex-plus.rkt"
         "markup.rkt"
         "format.rkt"
         (prefix-in i: "../data/inline.rkt"))

(provide make-math)

(define ((make-math [cfg : Config default-config])
         . [xs : MathTeX+Like *]) : i:Math
  (define st
    (state cfg
            (level #t 0)))
  (i:math ((format-math-tex+ st) (apply math-tex+% xs))))
