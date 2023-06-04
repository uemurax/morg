#lang typed/racket

(provide (struct-out splice) Splice)

(struct (X) splice
  ([contents : X])
  #:transparent
  #:type-name Splice)
