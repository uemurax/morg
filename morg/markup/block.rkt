#lang typed/racket

(require "../data/block.rkt"
         "../data/splice.rkt"
         "inline.rkt"
         (submod "index.rkt" print))

(provide BlockLike
         block-like->block
         block%
         print-index%
         paragraph%)

(define-type BlockLike
  (Rec X (U Block
            (Splice X)
            PrintIndex
            Paragraph)))

(define (block-like->block [x : BlockLike]) : Block
  (cond
   [(block? x) x]
   [(splice? x)
    (block (splice-map block-like->block x))]
   [else (block x)]))

(define (block% . [xs : BlockLike *]) : Block
  (block-like->block (splice xs)))

(define (paragraph% . [xs : InlineLike *]) : Paragraph
  (paragraph (apply inline% xs)))
