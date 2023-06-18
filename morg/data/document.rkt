#lang typed/racket

(require "id.rkt"
         "inline.rkt"
         "block.rkt"
         "date.rkt"
         "section.rkt")

(provide (struct-out document) Document)

(struct document
  ([id : Id]
   [author : (Listof PureInline)]
   [title : PureInline]
   [date : Date]
   [contents : Block]
   [front : (Listof Section)]
   [main : (Listof Section)]
   [back : (Listof Section)])
  #:transparent
  #:type-name Document)
