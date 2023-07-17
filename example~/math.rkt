#lang at-exp typed/racket

(require morg/math)

(provide + * =
         math)

(define +
  (monoid #:level 0 "0" "+"))

(define *
  (monoid #:level 1 "1" (macro "times")))

(define =
  (binary #:level 2 "="))
