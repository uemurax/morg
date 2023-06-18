#lang at-exp typed/racket

(require "../data/id.rkt"
         "../markup/string.rkt")

(provide id->text
         anchor-id->text)

(define (id->text [i : Id]) : StringTree
  @string%{[@(id-contents i)]})

(define (anchor-id->text [node : Id] [anchor : Id]) : StringTree
  @string%{[@(id-contents node)#@(id-contents anchor)]})
