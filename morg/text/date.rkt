#lang at-exp typed/racket

(require "../data/date.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt")

(provide date->text)

(define (date->text [d : Date]) : StringTree
  (define yyyy (~r (date-year d) #:min-width 4 #:pad-string "0"))
  (define mm (date-month d))
  (define -mm : StringTreeLike
    @when%[mm]{-@(~r mm #:min-width 2 #:pad-string "0")})
  (define dd (date-day d))
  (define -dd : StringTreeLike
    @when%[dd]{-@(~r dd #:min-width 2 #:pad-string "0")})
  @string%{@|yyyy|@|-mm|@|-dd|})
