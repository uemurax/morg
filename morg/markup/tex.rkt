#lang typed/racket

(require "../data/tex.rkt"
         "string.rkt"
         "../util/option.rkt")

(provide MathTeXLike
         math-tex-like->math-tex
         math-tex~
         group~ 
         sub-sup~)

(define-type MathTeXLike
  (Rec X (U MathTeX
            Text
            (Macro X)
            (Group X)
            SubSup
            StringTreeLike)))

(define (math-tex-like->math-tex [x : MathTeXLike]) : MathTeX
  (cond
   [(math-tex? x) x]
   [((make-predicate StringTreeLike) x)
    (math-tex (text (string-tree-like->string x)))]
   [(group? x)
    (math-tex ((group-map math-tex-like->math-tex) x))]
   [(macro? x)
    (math-tex ((macro-map math-tex-like->math-tex) x))]
   [else (math-tex x)]))

(define (math-tex~ [x : MathTeXLike]) : MathTeX
  (math-tex-like->math-tex x))

(define #:forall (X)
        (group~ . [xs : X *]) : (Group X)
  (group xs))

(define (sub-sup~ [base : MathTeXLike]
                  #:sub [sub : (Option MathTeXLike)]
                  #:sup [sup : (Option MathTeXLike)]) : SubSup
  (sub-sup (math-tex-like->math-tex base)
           (option-map math-tex-like->math-tex sub)
           (option-map math-tex-like->math-tex sup)))
