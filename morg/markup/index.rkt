#lang at-exp typed/racket

(require "../data/index.rkt"
         "../data/index-table.rkt"
         "../data/inline.rkt"
         "../data/article.rkt"
         "../data/block.rkt"
         "inline.rkt")

(provide index-list->inline
         (rename-out [index%/curried make-index])
         index%)

(define (make-index-type)
  (idx-type))

(define (index-list->inline
         #:less-than? [less-than? : (IndexItem IndexItem . -> . Boolean)
                                  index-item<?]
         [il : IndexList]) : Inline
  @inline%{
    @(apply unordered-list%
            (map (lambda ([ii : IndexItem])
                   (define i (index-item-index ii))
                   (define a (index-item-article ii))
                   @list-item%{@(index-display i) → @(ref (article-id a))})
                 (index-list-sort il #:less-than? less-than?)))
  })

(define (index%/curried)
  (define type (make-index-type))
  (define (idx #:key [key : String]
               . [xs : PureInlineLike *])
    (index key (apply pure-inline% xs) type))
  (define (prt)
    (print-index type))
  (values idx prt))

(define-values (index% print-index%) (index%/curried))

(module* print #f
  (provide print-index%))
