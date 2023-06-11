#lang typed/racket

(require "tex-plus.rkt"
         "markup.rkt"
         "format.rkt"
         (prefix-in i: "../data/inline.rkt"))

(provide make-math)

(define ((make-math [usr-cfg : UserConfig default-config])
         . [xs : MathTeX+Like *]) : i:Math
  (define cfg
    (config usr-cfg
            (level #t 0)))
  (i:math ((format-math-tex+ cfg) (apply math-tex+% xs))))
