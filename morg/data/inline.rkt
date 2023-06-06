#lang typed/racket

(require "splice.rkt"
         "id.rkt")

(provide (struct-out inline) Inline
         (struct-out ref) Ref
         (struct-out text) Text)

(struct inline
  ([contents : (InlineElement Inline)])
  #:transparent
  #:type-name Inline)

(define-type (InlineElement X)
  (U (Splice X)
     Ref
     Text))

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)

(struct ref
  ([id : Id])
  #:transparent
  #:type-name Ref)
