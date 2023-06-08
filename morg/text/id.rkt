#lang at-exp typed/racket

(require "../data/id.rkt"
         "../markup/string.rkt")

(provide id->text)

(define (id->text [i : Id]) : StringTree
  @string%{[@(id-contents i)]})
