#lang typed/racket

(require "../class.rkt")

(provide paragraph-class-name
         print-index-class-name)

(define paragraph-class-name (class-name "paragraph"))
(define print-index-class-name (class-name "print-index"))
