#lang typed/racket

(require typed/xml
         "../data/splice.rkt")

(provide XExprs XExprsLike
         xexprs-like->xexprs
         xexprs%
         tagged%)

(define-type XExprs (Listof XExpr))

(define-type XExprsLike
  (U XExprs
     XExpr
     (Splice XExprsLike)))

(define (xexprs-like->xexprs [x : XExprsLike]) : XExprs
  (cond
   [((make-predicate XExprs) x) x]
   [((make-predicate XExpr) x) (list x)]
   [(splice? x) 
    (apply append (map xexprs-like->xexprs (splice-contents x)))]))

(define (xexprs% . [xs : XExprsLike *]) : XExprs
  (xexprs-like->xexprs (splice xs)))

(define (tagged% [tag : Symbol]
                 #:attrs [attrs : (Listof (List Symbol String)) (list)]
                 . [xs : XExprsLike *]) : XExprs
  (xexprs%
   `(,tag ,attrs
          ,@(apply xexprs% xs))))
