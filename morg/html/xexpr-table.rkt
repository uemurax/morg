#lang typed/racket

(require "../markup/xexpr.rkt"
         "../data/id.rkt")

(provide XExprTable)

(define-type XExprTable (HashTable Id XExprs))
