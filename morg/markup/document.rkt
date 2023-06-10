#lang typed/racket

(require "../data/document.rkt"
         "../data/id.rkt"
         "../data/section.rkt"
         "block.rkt"
         "inline.rkt")

(provide document%)

(define (document% #:id [maybe-id : String]
                   #:author [author : (Listof InlineLike)]
                   #:title [title : InlineLike]
                   #:contents [contents : BlockLike (block%)]
                   #:front [front : (Listof Section) (list)]
                   #:back [back : (Listof Section) (list)]
                   . [main : Section *]) : Document
  (document (id maybe-id)
            (map inline% author)
            (inline% title)
            (block% contents)
            front
            main
            back))
