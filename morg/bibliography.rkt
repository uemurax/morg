#lang at-exp typed/racket

(require "bibliography/markup.rkt"
         "bibliography/format.rkt"
         "bibliography/bib-item.rkt"
         (prefix-in x: "markup/article.rkt")
         "markup/inline.rkt"
         "markup/block.rkt"
         "markup/splice.rkt")

(provide
 bib
 (rename-out [eprint% eprint]
             [article% article]
             [book% book]))

(define (bib #:header [header : InlineLike @%{Bibliography item}]
             [maybe-id : String]
             [b : BibItem])
  @x:article%[
    #:id maybe-id
    #:header header
    @paragraph%{
      @(format-bib-item b)
    }
  ])
