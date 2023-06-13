#lang typed/racket

(require "tex-plus.rkt"
         "../data/tex.rkt"
         "../data/splice.rkt"
         "../util/list.rkt"
         "../markup/tex.rkt")

(provide Atom+Like
         MathTeXAtom+Like
         MathTeX+Like
         atom+-like->atom+
         math-tex+-like->math-tex+
         paren%
         paren%/curried
         dec-degree%
         binary%
         monoid%
         big-op%
         apply-with-parens%
         math-tex+%)

(define-type (Atom+Like X)
  (U (AtomLike X)
     (Paren X)))

(define-type MathTeXAtom+Like
  (Atom+Like MathTeX+Like))

(define-type MathTeX+Like
  (U MathTeX+
     MathTeXAtom+Like
     (Splice MathTeX+Like)
     (SubSup MathTeXAtom+Like MathTeX+Like)))

(define #:forall (X)
        (atom+-like->atom+ [x : (Atom+Like X)]) : (Atom+ X)
  (cond
   [(paren? x) (atom+ x)]
   [else (atom+ (atom-like->atom x))]))

(define (math-tex-atom+-like->math-tex-atom+ [x : MathTeXAtom+Like]) : (Atom+ MathTeX+)
  ((atom+-map math-tex+-like->math-tex+) (atom+-like->atom+ x)))

(define (math-tex+-like->math-tex+ [x : MathTeX+Like]) : MathTeX+
  (cond
   [(math-tex+? x) x]
   [((make-predicate MathTeXAtom+Like) x)
    (math-tex+ (math-tex-atom+-like->math-tex-atom+ x))]
   [(splice? x)
    (math-tex+ (splice-map math-tex+-like->math-tex+ x))]
   [(sub-sup? x)
    (math-tex+
     ((sub-sup-map math-tex-atom+-like->math-tex-atom+
                   math-tex+-like->math-tex+)
      x))]
   [else (math-tex+ x)]))

(define (math-tex+% . [xs : MathTeX+Like *]) : MathTeX+
  (math-tex+-like->math-tex+ (splice xs)))

(define ((paren%/curried #:level [lv : (U Symbol #f #t) #t]
                         #:left [left : MathTeX+Like "("]
                         #:right [right : MathTeX+Like ")"])
         . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (paren (level lv 0) left right (splice xs)))

(define (paren% #:level [lv : (U Symbol #f #t) #t]
                #:left [left : MathTeX+Like "("]
                #:right [right : MathTeX+Like ")"]
                . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (apply (paren%/curried #:level lv #:left left #:right right) xs))

(define (dec-degree% . [xs : MathTeX+Like *]) : MathTeX+
  (math-tex+-dec-degree (apply math-tex+% xs)))

(define ((binary% #:level [lv : Symbol '?]
                  #:assoc [assoc : (U 'left 'right 'none) 'none]
                  [op : MathTeX+Like])
         [a : MathTeX+Like] [b : MathTeX+Like]) : (Paren MathTeX+Like)
  (define-values (l r)
    (case assoc
     [(left) (values (dec-degree% a) b)]
     [(right) (values a (dec-degree% b))]
     [else (values a b)]))
  (paren% #:level lv
          l op r))

(define ((monoid% #:level [lv : Symbol '?]
                  [unit : MathTeX+Like]
                  [bin : MathTeX+Like])
         . [xs : MathTeX+Like *]) : MathTeX+Like
  (define n (length xs))
  (cond
   [(eq? n 0) unit]
   [(eq? n 1) (list-ref xs 0)]
   [else
    (apply (paren%/curried #:level lv) (list-join-1 xs bin))]))

(define ((big-op% #:level [lv : Symbol '?]
                  [op : MathTeXAtom+Like])
         #:_ [sub : (Option MathTeX+Like) #f]
         #:^ [sup : (Option MathTeX+Like) #f]
         . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (paren% #:level lv
          ((inst sub-sup% MathTeXAtom+Like MathTeX+Like) op #:_ sub #:^ sup)
          (apply dec-degree% xs)))

(define ((apply-with-parens% #:left [left : MathTeX+Like "("]
                             #:right [right : MathTeX+Like ")"])
         [f : MathTeX+Like] [x : MathTeX+Like]) : (Paren MathTeX+Like)
  (paren% #:level #f
          (dec-degree% (group% f))
          (paren% #:left left #:right right x)))
