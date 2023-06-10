#lang typed/racket

(require "../data/date.rkt")

(provide date%)

(define (date% [year : Integer]
               [month : (Option Exact-Positive-Integer) #f]
               [day : (Option Exact-Positive-Integer) #f])
  (date year month day))
