#lang typed/racket

(require typed/racket/date
         "../data/document.rkt"
         "../data/id.rkt"
         "../data/date.rkt"
         "../data/section.rkt"
         "block.rkt"
         "inline.rkt")

(provide document%)

(define (document% #:id [maybe-id : String]
                   #:author [author : (Listof InlineLike)]
                   #:title [title : InlineLike]
                   #:date [date : Date (current-date)]
                   #:contents [contents : BlockLike (block%)]
                   #:front [front : (Listof Section) (list)]
                   #:back [back : (Listof Section) (list)]
                   . [main : Section *]) : Document
  (document (id maybe-id)
            (map inline% author)
            (inline% title)
            date
            (block% contents)
            front
            main
            back))
