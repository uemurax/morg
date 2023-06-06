#lang typed/racket

(require "../data/block.rkt"
         "../data/splice.rkt"
         "inline.rkt")

(provide BlockLike
         block-like->block
         block~
         paragraph~)

(define-type BlockLike
  (Rec X (U Block
            (Splice X)
            Paragraph)))

(define (block-like->block [x : BlockLike]) : Block
  (cond
   [(block? x) x]
   [(splice? x)
    (block (splice-map block-like->block x))]
   [else (block x)]))

(define (block~ . [xs : BlockLike *]) : Block
  (block-like->block (splice xs)))

(define (paragraph~ . [xs : InlineLike *]) : Paragraph
  (paragraph (apply inline~ xs)))
