#lang at-exp typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt"
         "splice.rkt"
         "string.rkt"
         "../util/list.rkt")

(provide MathTeXLike
         AtomLike
         atom-like->atom
         math-tex-like->math-tex
         math-tex%
         sub-sup%
         TextTeXLike
         text-tex-like->text-tex
         text-tex%
         argument%
         optional-argument%
         star-argument%
         macro%
         macro-1%
         group%
         options%
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
     (SubSup MathTeXAtomLike MathTeXLike)
     (Splice MathTeXLike)))

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
   [(sub-sup? x) 
    (math-tex
     ((sub-sup-map math-tex-atom-like->math-tex-atom math-tex-like->math-tex) x))]
   [else (math-tex x)]))

(define (math-tex% . [xs : MathTeXLike *]) : MathTeX
  (math-tex-like->math-tex (splice xs)))

(define #:forall (A X)
        (sub-sup% [base : A]
                  #:_ [sub : (Option X) #f]
                  #:^ [sup : (Option X) #f]) : (U (SubSup A X) A)
  (cond
   [(or sub sup) (sub-sup base sub sup)]
   [else base]))

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
        (argument% #:parentheses [parens : (Pairof String String) '("{" . "}")]
                  . [xs : X *])
  (argument (splice xs) parens))

(define #:forall (X)
        (optional-argument% . [xs : X *])
  (argument (splice xs) '("[" . "]")))

(define star-argument%
  (argument "*" '("" . "")))

(define #:forall (X)
        (macro% [head : StringTreeLike]
                . [args : (Argument X) *]) : (Macro X)
  (macro (string-tree-like->string head) args))

(define #:forall (X)
        ((macro-1% [head : StringTreeLike])
         . [xs : X *]) : (Macro (Splice X))
  (macro% head (apply argument% xs)))

(define #:forall (X)
        (group% . [xs : X *]) : (Group (Splice X))
  (group (splice xs)))

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

(define #:forall (X)
        (option% [x : (U String (Pairof String X))])
  (cond
   [(string? x) @%{@|x|}]
   [(pair? x) @%{@(car x)=@(cdr x)}]))

(define #:forall (X)
        (options% . [xs : (U String (Pairof String X)) *])
  (apply % (list-join-1 (map (inst option% X) xs) ",")))
