#lang typed/racket

(require "splice.rkt")

(provide (struct-out inline) Inline
         (struct-out text) Text)

(struct inline
  ([contents : (InlineElement Inline)])
  #:transparent
  #:type-name Inline)

(define-type (InlineElement X)
  (U (Splice X)
     Text))

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)
