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
 (rename-out [bibliography/curried make-bibliography])
 (rename-out [eprint% eprint]
             [article% article]
             [thesis% thesis]
             [misc% misc]
             [arXiv% arXiv]
             [inbook% inbook]
             [book% book]))

(define default-header
  "Bibliography item")

(define ((bibliography/curried
          . [header : PureInlineLike *])
         #:id [maybe-id : String]
         [b : BibItem])
  @x:article%[
    #:id maybe-id
    #:header (apply pure-inline% header)
    @paragraph%{
      @(format-bib-item b)
    }
  ])

(define (bibliography
         #:header [header : PureInlineLike default-header]
         #:id [maybe-id : String]
         [b : BibItem])
  ((bibliography/curried header) #:id maybe-id b))
