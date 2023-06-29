#lang typed/racket

(require "inline.rkt"
         "index.rkt"
         "splice.rkt")

(provide (struct-out block) Block
         (struct-out print-index) PrintIndex
         (struct-out paragraph) Paragraph)

(struct block
  ([contents : (BlockElement Block)])
  #:transparent
  #:type-name Block)

(define-type (BlockElement X)
  (U (Splice X)
     PrintIndex
     Paragraph))

(struct paragraph
  ([contents : Inline])
  #:transparent
  #:type-name Paragraph)

(struct print-index
  ([type : IndexType])
  #:transparent
  #:type-name PrintIndex)
