#lang typed/racket

(require "../data/section.rkt"
         "../data/id.rkt"
         "../data/article.rkt"
         "inline.rkt"
         "block.rkt")

(provide section%)

(define-type SectionElementLike
  (U Article BlockLike))

(define (section-element-like->section-element [x : SectionElementLike]) : SectionElement
  (cond
   [(article? x) x]
   [((make-predicate BlockLike) x)
    (block% x)]))

(define (section% #:id [maybe-id : String]
                  #:title [title : PureInlineLike]
                  #:subsections [subsections : (Listof Section) (list)]
                  . [contents : SectionElementLike *]) : Section
  (section (id maybe-id)
           (pure-inline% title)
           (map section-element-like->section-element contents)
           subsections))
