#lang typed/racket

(require "../data/section.rkt"
         "../data/id.rkt"
         "../data/article.rkt"
         "../util/option.rkt"
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
                  #:description [description : (Option PureInlineLike) #f]
                  #:subsections [subsections : (Listof Section) (list)]
                  . [contents : SectionElementLike *]) : Section
  (section (id maybe-id)
           (pure-inline% title)
           (option-map pure-inline% description)
           (map section-element-like->section-element contents)
           subsections))
