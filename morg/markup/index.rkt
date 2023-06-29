#lang at-exp typed/racket

(require "../data/index.rkt"
         "../data/index-table.rkt"
         "../data/inline.rkt"
         "../data/article.rkt"
         "inline.rkt")

(provide index-list->inline
         default-index-type
         index%/curried
         index%)

(define default-index-type
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

(define ((index%/curried #:type [type : IndexType default-index-type])
         #:key [key : String]
         . [xs : PureInlineLike *]) : Index
  (index key (apply pure-inline% xs) type))

(define (index% #:key [key : String]
                #:type [type : IndexType default-index-type]
                . [xs : PureInlineLike *]) : Index
  ((index%/curried #:type type) #:key key (apply pure-inline% xs)))
