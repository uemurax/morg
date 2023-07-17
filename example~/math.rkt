#lang at-exp typed/racket

(require morg/math)

(provide + * =
         math)

(module levels typed/racket
  (require morg/math)
  (provide (all-defined-out))
  (define-levels
   *
   +
   =))

(require (prefix-in l: 'levels))

(define +
  (monoid #:level l:+ "0" "+"))

(define *
  (monoid #:level l:* "1" (macro "times")))

(define =
  (binary #:level l:= "="))
