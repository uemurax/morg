#lang typed/racket

(require (prefix-in racket: typed/racket/base)
         (prefix-in racket: typed/racket/date))

(provide (except-out (struct-out date) date) Date
         (rename-out [make-date date])
         racket-date->date
         current-date)

(struct date
  ([year : Integer]
   [month : (Option Exact-Positive-Integer)]
   [day : (Option Exact-Positive-Integer)])
  #:transparent
  #:type-name Date)

(define (make-date [year : Integer]
                   [month : (Option Exact-Positive-Integer)]
                   [day : (Option Exact-Positive-Integer)])
  (cond
   [(and (not month) day)
    (error "Month is mandatory when day is given.")]
   [else (date year month day)]))

(define (racket-date->date [d : racket:date]) : Date
  (make-date (racket:date-year d)
             (cast (racket:date-month d) Exact-Positive-Integer)
             (cast (racket:date-day d) Exact-Positive-Integer)))

(define (current-date) : Date
  (racket-date->date (racket:current-date)))
