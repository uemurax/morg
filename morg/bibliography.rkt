#lang at-exp typed/racket

(require "bibliography/markup.rkt"
         "bibliography/format.rkt"
         "bibliography/bib-item.rkt"
         (prefix-in x: "markup/article.rkt")
         "markup/inline.rkt"
         "markup/block.rkt"
         "markup/splice.rkt")

(provide
 bibliography
 bibliography/curried
 (rename-out [eprint% eprint]
             [article% article]
             [book% book]))

(define default-header
  "Bibliography item")

(define ((bibliography/curried
          #:header [header : PureInlineLike default-header])
         #:id [maybe-id : String]
         [b : BibItem])
  @x:article%[
    #:id maybe-id
    #:header header
    @paragraph%{
      @(format-bib-item b)
    }
  ])

(define (bibliography
         #:header [header : PureInlineLike default-header]
         #:id [maybe-id : String]
         [b : BibItem])
  ((bibliography/curried #:header header) #:id maybe-id b))
