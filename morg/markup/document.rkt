#lang typed/racket

(require "../data/document.rkt"
         "../data/id.rkt"
         "../data/section.rkt"
         "../data/date.rkt"
         "../util/option.rkt"
         "block.rkt"
         "inline.rkt")

(provide document%)

(define (document% #:id [maybe-id : String]
                   #:author [author : (Listof PureInlineLike)]
                   #:title [title : PureInlineLike]
                   #:description [description : (Option PureInlineLike) #f]
                   #:date [d : Date (current-date)]
                   #:contents [contents : BlockLike (block%)]
                   #:front [front : (Listof Section) (list)]
                   #:back [back : (Listof Section) (list)]
                   . [main : Section *]) : Document
  (document (id maybe-id)
            (map pure-inline% author)
            (pure-inline% title)
            (option-map pure-inline% description)
            d
            (block% contents)
            front
            main
            back))
