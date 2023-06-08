#lang at-exp typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt"
         "splice.rkt"
         "string.rkt"
         "../util/option.rkt")

(provide MathTeXLike
         math-tex-like->math-tex
         math-tex%
         sub-sup%
         TextTeXLike
         text-tex-like->text-tex
         text-tex%
         argument%
         optional-argument%
         macro%
         environment%)

(define-type TextLike
  (U Text StringTreeLike))

(define-type (AtomLike X)
  (U (Atom X)
     TextLike
     (Macro X)
     (Group X)))

(define-type MathTeXAtomLike
  (AtomLike MathTeXLike))

(define-type MathTeXLike
  (U MathTeX
     MathTeXAtomLike
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

(define (math-tex-atom-like->math-tex-atom [x : MathTeXAtomLike]) : (Atom MathTeX)
  ((atom-map math-tex-like->math-tex) (atom-like->atom x)))

(define (math-tex-like->math-tex [x : MathTeXLike]) : MathTeX
  (cond
   [(math-tex? x) x]
   [((make-predicate MathTeXAtomLike) x)
    (math-tex (math-tex-atom-like->math-tex-atom x))]
   [(splice? x)
    (math-tex (splice-map math-tex-like->math-tex x))]
   [else (math-tex x)]))

(define (math-tex% . [xs : MathTeXLike *]) : MathTeX
  (math-tex-like->math-tex (splice xs)))

(define (sub-sup% [base : MathTeXAtomLike]
                  #:sub [sub : (Option MathTeXLike)]
                  #:sup [sup : (Option MathTeXLike)]) : SubSup
  (sub-sup (math-tex-atom-like->math-tex-atom base)
           (option-map math-tex-like->math-tex sub)
           (option-map math-tex-like->math-tex sup)))

(define-type TextTeXAtomLike
  (AtomLike TextTeXLike))

(define-type TextTeXLike
  (U TextTeX
     TextTeXAtomLike
     (Splice TextTeXLike)
     Math))

(define (text-tex-atom-like->text-tex-atom [x : TextTeXAtomLike]) : (Atom TextTeX)
  ((atom-map text-tex-like->text-tex) (atom-like->atom x)))

(define (text-tex-like->text-tex [x : TextTeXLike]) : TextTeX
  (cond
   [(text-tex? x) x]
   [((make-predicate TextTeXAtomLike) x)
    (text-tex (text-tex-atom-like->text-tex-atom x))]
   [(splice? x) (text-tex (splice-map text-tex-like->text-tex x))]
   [else (text-tex x)]))

(define (text-tex% . [xs : TextTeXLike *]) : TextTeX
  (text-tex-like->text-tex (splice xs)))

(define #:forall (X)
        (argument% [x : X]
                   #:parentheses [parens : (Pairof String String) '("" . "")])
        : (Argument X)
  (argument x parens))

(define #:forall (X)
        (optional-argument% [x : X]) : (Argument X)
  (argument% x #:parentheses '("[" . "]")))

(define #:forall (X)
        (macro% [head : StringTreeLike]
                . [args : (Argument X) *]) : (Macro X)
  (macro (string-tree-like->string head) args))

(define #:forall (X)
        (environment% [name : StringTreeLike]
                      #:arguments [args : (Listof (Argument X)) (list)]
                      . [body : X *])
  (define arg-1 (argument% name))
  @%{
    @(apply (inst macro% (U X StringTreeLike)) "begin" arg-1 args)
    @(apply % body)
    @((inst macro% (U X StringTreeLike)) "end" arg-1)
  })
