#lang typed/racket

(require "splice.rkt"
         "id.rkt"
         (only-in "tex.rkt" MathTeX))

(provide (struct-out inline) Inline
         (struct-out ref) Ref
         (struct-out text) Text
         (struct-out math) Math)

(struct inline
  ([contents : (InlineElement Inline)])
  #:transparent
  #:type-name Inline)

(define-type (InlineElement X)
  (U (Splice X)
     Ref
     Math
     Text))

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)

(struct ref
  ([id : Id])
  #:transparent
  #:type-name Ref)

(struct math
  ([contents : MathTeX])
  #:transparent
  #:type-name Math)
