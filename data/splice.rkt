#lang typed/racket

(provide (struct-out splice) Splice
         splice-map)

(struct (X) splice
  ([contents : (Listof X)])
  #:transparent
  #:type-name Splice)

(define #:forall (X Y)
        (splice-map [f : (X . -> . Y)] [a : (Splice X)]) : (Splice Y)
  (splice (map f (splice-contents a))))
