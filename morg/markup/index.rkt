#lang at-exp typed/racket

(require "../data/index.rkt"
         "../data/index-table.rkt"
         "../data/inline.rkt"
         "../data/article.rkt"
         "inline.rkt")

(provide index-list->inline
         index%)

(define (index-list->inline [il : IndexList]) : Inline
  @inline%{
    @(apply unordered-list%
            (map (lambda ([ii : IndexItem])
                   (define i (index-item-index ii))
                   (define a (index-item-article ii))
                   @list-item%{@(index-display i) → @(ref (article-id a))})
                 il))
  })

(define (index% #:key [key : String]
                #:type [type : Symbol 'index]
                . [xs : InlineLike *]) : Index
  (index key (apply inline% xs) type))
