#lang typed/racket

(provide class-name)

(define class-prefix "morg-generated-")

(define (class-name [s : String]) : String
  (string-append class-prefix s))
