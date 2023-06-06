#lang typed/racket

(require "../data/inline.rkt"
         "../data/splice.rkt")

(provide InlineLike
         inline-like->inline
         inline~)

(define-type InlineLike
  (Rec X (U Inline
            (Splice X)
            Text
            String)))

(define (inline-like->inline [x : InlineLike]) : Inline
  (cond
   [(inline? x) x]
   [(splice? x)
    (inline (splice-map inline-like->inline x))]
   [(string? x) (inline (text x))]
   [else (inline x)]))

(define (inline~ . [xs : InlineLike *]) : Inline
  (inline-like->inline (splice xs)))
