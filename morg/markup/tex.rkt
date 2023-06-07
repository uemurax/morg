#lang typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt"
         "string.rkt"
         "../util/option.rkt")

(provide MathTeXLike
         math-tex-like->math-tex
         math-tex~
         group~ 
         sub-sup~
         TextTeXLike
         text-tex-like->text-tex
         text-tex~)

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

(define-type TextTeXLike
  (Rec X (U TextTeX
            Text
            (Macro X)
            (Group X)
            (Splice X)
            Math
            StringTreeLike)))
  
(define (text-tex-like->text-tex [x : TextTeXLike]) : TextTeX
  (cond
   [(text-tex? x) x]
   [((make-predicate StringTreeLike) x)
    (text-tex (text (string-tree-like->string x)))]
   [(group? x) (text-tex ((group-map text-tex-like->text-tex) x))]
   [(macro? x) (text-tex ((macro-map text-tex-like->text-tex) x))]
   [(splice? x) (text-tex (splice-map text-tex-like->text-tex x))]
   [else (text-tex x)]))

(define (text-tex~ . [xs : TextTeXLike *]) : TextTeX
  (text-tex-like->text-tex (splice xs)))
