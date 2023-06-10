#lang typed/racket

(require "../data/date.rkt")

(provide date%)

(define (date% #:year [year : Integer]
               #:month [month : (Option Exact-Positive-Integer) #f]
               #:day [day : (Option Exact-Positive-Integer) #f])
  (date year month day))
