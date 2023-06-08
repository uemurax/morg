#lang typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt"
         "string.rkt"
         "../util/option.rkt")

(provide MathTeXLike
         math-tex-like->math-tex
         math-tex~
         sub-sup~
          TextTeXLike
         text-tex-like->text-tex
         text-tex~)

(define-type TextLike
  (U Text StringTreeLike))

(define-type (AtomLike X)
  (U (Atom X)
     TextLike
     (Macro X)
     (Group X)))

(define-type MathTeXAtomLike
  (AtomLike MathTeX))

(define-type MathTeXLike
  (U MathTeX
     (AtomLike MathTeX)
     (Splice MathTeXLike)
     SubSup))

(define (text-like->text [x : TextLike]) : Text
  (cond
   [(text? x) x]
   [((make-predicate StringTreeLike) x)
    (text (string-tree-like->string x))]))

(define #:forall (X)
        (atom-like->atom [x : (AtomLike X)]) : (Atom X)
  (cond
   [(atom? x) x]
   [((make-predicate TextLike) x)
    (atom (text-like->text x))]
   [else (atom x)]))

(define (math-tex-like->math-tex [x : MathTeXLike]) : MathTeX
  (cond
   [(math-tex? x) x]
   [((make-predicate (AtomLike MathTeX)) x)
    (math-tex (atom-like->atom x))]
   [(splice? x)
    (math-tex (splice-map math-tex-like->math-tex x))]
   [else (math-tex x)]))

(define (math-tex~ . [xs : MathTeXLike *]) : MathTeX
  (math-tex-like->math-tex (splice xs)))

(define (sub-sup~ [base : (AtomLike MathTeX)]
                  #:sub [sub : (Option MathTeXLike)]
                  #:sup [sup : (Option MathTeXLike)]) : SubSup
  (sub-sup (atom-like->atom base)
           (option-map math-tex-like->math-tex sub)
           (option-map math-tex-like->math-tex sup)))

(define-type TextTeXLike
  (U TextTeX
     (AtomLike TextTeX)
     (Splice TextTeXLike)
     Math))

(define (text-tex-like->text-tex [x : TextTeXLike]) : TextTeX
  (cond
   [(text-tex? x) x]
   [((make-predicate (AtomLike TextTeX)) x)
    (text-tex (atom-like->atom x))]
   [(splice? x) (text-tex (splice-map text-tex-like->text-tex x))]
   [else (text-tex x)]))

(define (text-tex~ . [xs : TextTeXLike *]) : TextTeX
  (text-tex-like->text-tex (splice xs)))
