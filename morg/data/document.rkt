#lang typed/racket

(require "id.rkt"
         "inline.rkt"
         "block.rkt"
         "section.rkt")

(provide (struct-out document) Document)

(struct document
  ([id : Id]
   [author : (Listof Inline)]
   [title : Inline]
   [date : date]
   [contents : Block]
   [front : (Listof Section)]
   [main : (Listof Section)]
   [back : (Listof Section)])
  #:transparent
  #:type-name Document)
