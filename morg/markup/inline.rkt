#lang typed/racket

(require "../data/inline.rkt"
         "../data/id.rkt"
         "../data/splice.rkt"
         "tex.rkt")

(provide InlineLike
         inline-like->inline
         ref~
         math~
         inline~)

(define-type InlineLike
  (Rec X (U Inline
            (Splice X)
            Text
            Ref
            Math
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

(define (ref~ [maybe-id : String]) : Ref
  (ref (id maybe-id)))

(define (math~ . [xs : MathTeXLike *]) : Math
  (math (apply math-tex~ xs)))
