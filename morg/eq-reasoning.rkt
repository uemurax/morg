#lang at-exp typed/racket

(require "data/extension.rkt"
         "markup/xexpr.rkt"
         "markup/inline.rkt")

(module+ test
  (require typed/rackunit))

(define cls (ext-class))

(define-type EqReasoningUnit
  (List InlineLike))

(define-type EqReasoningCons
  (List* InlineLike InlineLike InlineLike EqReasoning))

(define-type EqReasoning
  (U EqReasoningUnit EqReasoningCons))

(: eq-reasoning:list : (EqReasoning . -> . (Listof InlineLike)))

(define (eq-reasoning:list x)
  (match x
   [(list x) (list "" x)]
   [(list* term rel reason rst)
    (list* "" term
           rel @inline%{ {reason}}
           (eq-reasoning:list rst))]))

(define (eq-reasoning:fun [e : EqReasoning]) : InlineLike
  (extension cls (eq-reasoning:list e)))

(define-syntax-rule (eq-reasoning body ...)
  (eq-reasoning:fun (list body ...)))

(module+ test
  (check-true (assert-typecheck-fail
               (eq-reasoning)
               #:result #t))
  (check-true (assert-typecheck-fail
               (eq-reasoning
                "a" "=" "b")
               #:result #t))
  (check-false (not (eq-reasoning
                     "1 + 1"
                     "=" "definition"
                     "2"))))

(define (eq-reasoning->xexprs [xs : (Listof XExprs)]) : XExprs
  (apply tagged% 'span
         '((style "display: grid; grid-template-columns: max-content max-content; grid-column-gap: 1em;"))
         (map (lambda ([x : XExprs])
                (tagged% 'span '() x))
              xs)))
