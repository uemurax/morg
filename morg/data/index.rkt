#lang typed/racket

(require "inline.rkt")

(provide (struct-out index) Index
         index<?)

(struct index
  ([key : String]
   [display : PureInline]
   [type : Symbol])
  #:transparent
  #:type-name Index)

(define (index<? [i1 : Index] [i2 : Index]) : Boolean
  ((index-key i1) . string<? . (index-key i2)))
