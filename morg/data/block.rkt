#lang typed/racket

(require "inline.rkt"
         "splice.rkt")

(provide (struct-out block) Block
         (struct-out paragraph) Paragraph)

(struct block
  ([contents : (BlockElement Block)])
  #:transparent
  #:type-name Block)

(define-type (BlockElement X)
  (U (Splice X)
     Paragraph))

(struct paragraph
  ([contents : Inline])
  #:transparent
  #:type-name Paragraph)
