#lang at-exp typed/racket

(require "data/extension.rkt"
         "util/list.rkt"
         "markup/xexpr.rkt"
         "markup/tex.rkt"
         "markup/inline.rkt")

(module+ test
  (require typed/rackunit))

(provide eq-reasoning)

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

(module* latex-config #f
  (require "latex/config.rkt")

  (provide config-update)

  (define (eq-reasoning->latex [xs : (Listof TextTeXLike)]) : TextTeXLike
    (define xss (list-group xs 2))
    (define yss
      (map (lambda ([ws : (Listof TextTeXLike)])
             (list-join-1 ws @special%["&"]))
           xss))
    (define zs
      (list-join yss (list @macro%["\\"])))
    (text-tex%
     ((inst environment% TextTeXLike)
      "longtable"
      #:arguments (list @argument%{ll})
      (apply text-tex% zs))))

  (define (config-update [cfg : Config]) : Config
    (struct-copy config cfg
     [render-extension
      (ext-hash-set (config-render-extension cfg)
                    cls eq-reasoning->latex)])))

(module* html-config #f
  (require "html/config.rkt")

  (provide config-update)

  (define (eq-reasoning->xexprs [xs : (Listof XExprs)]) : XExprs
    (apply tagged% 'span
           '((style "display: grid; grid-template-columns: max-content max-content; grid-column-gap: 1em;"))
           (map (lambda ([x : XExprs])
                  (tagged% 'span '() x))
                xs)))

  (define (config-update [cfg : Config]) : Config
    (struct-copy config cfg
     [render-extension
      (ext-hash-set (config-render-extension cfg)
                    cls eq-reasoning->xexprs)])))
