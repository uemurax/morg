#lang at-exp typed/racket

(require "bibliography/markup.rkt"
         "bibliography/format.rkt"
         "bibliography/bib-item.rkt"
         "markup/article.rkt"
         "markup/inline.rkt"
         "markup/block.rkt"
         "markup/splice.rkt"
         "data/article.rkt")

(provide
 bib
 (rename-out [eprint% eprint]
             [book% book]))

(define (bib #:header [header : InlineLike @%{Bibliography item}]
             [maybe-id : String]
             [b : BibItem]) : Article
  @article%[
    #:id maybe-id
    #:header header
    @paragraph%{
      @(format-bib-item b)
    }
  ])
