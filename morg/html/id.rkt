#lang typed/racket

(require "../data/id.rkt")

(provide id->url)

(define (id->url [i : Id]) : String
  (id-contents i))
