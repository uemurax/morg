#lang typed/racket

(require "../class.rkt")

(provide d-pad-class-name
         d-pad-previous-class-name
         d-pad-up-class-name
         d-pad-next-class-name)

(define d-pad-class-name (class-name "d-pad"))
(define d-pad-previous-class-name (class-name "d-pad-previous"))
(define d-pad-up-class-name (class-name "d-pad-up"))
(define d-pad-next-class-name (class-name "d-pad-next"))
