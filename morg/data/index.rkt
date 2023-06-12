#lang typed/racket

(require "inline.rkt")

(provide (struct-out index) Index)

(struct index
  ([key : String]
   [display : Inline])
  #:transparent
  #:type-name Index)
