#lang typed/racket

(require "id.rkt"
         "inline.rkt"
         "section.rkt")

(provide (struct-out document) Document)

(struct document
  ([id : Id]
   [author : (Listof Inline)]
   [title : Inline]
   [front : (Listof Section)]
   [main : (Listof Section)]
   [appendix : (Listof Section)]
   [back : (Listof Section)])
  #:transparent
  #:type-name Document)
