#lang typed/racket

(require "../data/document.rkt"
         "../data/id.rkt"
         "../data/section.rkt"
         "../data/date.rkt"
         "block.rkt"
         "inline.rkt")

(provide document%)

(define (document% #:id [maybe-id : String]
                   #:author [author : (Listof InlineLike)]
                   #:title [title : InlineLike]
                   #:date [d : Date (current-date)]
                   #:contents [contents : BlockLike (block%)]
                   #:front [front : (Listof Section) (list)]
                   #:back [back : (Listof Section) (list)]
                   . [main : Section *]) : Document
  (document (id maybe-id)
            (map inline% author)
            (inline% title)
            d
            (block% contents)
            front
            main
            back))
