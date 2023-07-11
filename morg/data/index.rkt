#lang typed/racket

(require "inline.rkt")

(provide (struct-out index) Index
         (struct-out idx-type) IndexType
         index<?)

(struct idx-type ()
  #:transparent
  #:type-name IndexType)

(struct index
  ([key : String]
   [display : PureInline]
   [type : IndexType])
  #:transparent
  #:type-name Index)

(define (index<? [i1 : Index] [i2 : Index]) : Boolean
  ((index-key i1) . string<? . (index-key i2)))
