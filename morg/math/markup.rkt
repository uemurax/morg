#lang typed/racket

(require "tex-plus.rkt"
         "level.rkt"
         "../data/tex.rkt"
         "../data/splice.rkt"
         "../util/list.rkt"
         "../markup/splice.rkt"
         "../markup/tex.rkt")

(provide MathTeXAtom+Like
         MathTeX+Like
         math-tex+-like->math-tex+
         paren%
         paren%/curried
         dec-degree%
         binary%
         unary%
         monoid%
         big-op%
         delimiter%
         apply-with-parens%
         sup-op%
         math-tex+%)

(define-type MathTeXAtom+Like
  (AtomLike MathTeX+Like))

(define-type MathTeX+Like
  (U MathTeX+
     MathTeXAtom+Like
     (Splice MathTeX+Like)
     (Paren MathTeX+Like)
     (SubSup MathTeXAtom+Like MathTeX+Like)))

(define (math-tex-atom+-like->math-tex-atom+ [x : MathTeXAtom+Like]) : (Atom MathTeX+)
  ((atom-map math-tex+-like->math-tex+)
   (atom-like->atom x)))

(define (math-tex+-like->math-tex+ [x : MathTeX+Like]) : MathTeX+
  (cond
   [(math-tex+? x) x]
   [((make-predicate MathTeXAtom+Like) x)
    (math-tex+ (math-tex-atom+-like->math-tex-atom+ x))]
   [(splice? x)
    (math-tex+ (splice-map math-tex+-like->math-tex+ x))]
   [(paren? x)
    (math-tex+ ((paren-map math-tex+-like->math-tex+) x))]
   [(sub-sup? x)
    (math-tex+
     ((sub-sup-map math-tex-atom+-like->math-tex-atom+
                   math-tex+-like->math-tex+)
      x))]
   [else (math-tex+ x)]))

(define (math-tex+% . [xs : MathTeX+Like *]) : MathTeX+
  (math-tex+-like->math-tex+ (splice xs)))

(define ((paren%/curried #:level [lv : Level #t]
                         #:left [left : MathTeX+Like "("]
                         #:right [right : MathTeX+Like ")"])
         . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (paren lv left right (splice xs)))

(define (paren% #:level [lv : Level #t]
                #:left [left : MathTeX+Like "("]
                #:right [right : MathTeX+Like ")"]
                . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (apply (paren%/curried #:level lv #:left left #:right right) xs))

(define (dec-degree% . [xs : MathTeX+Like *]) : MathTeX+
  (math-tex+-dec-degree (apply math-tex+% xs)))

(define-type OpLevel Exact-Rational)

(define ((binary% #:level [lv : OpLevel]
                  #:assoc [assoc : (U 'left 'right 'none) 'none]
                  [op : MathTeX+Like])
         [a : MathTeX+Like] [b : MathTeX+Like]) : (Paren MathTeX+Like)
  (define-values (l r)
    (case assoc
     [(left) (values (dec-degree% a) b)]
     [(right) (values a (dec-degree% b))]
     [else (values a b)]))
  (paren% #:level (make-level lv)
          l op r))

(define ((unary% #:level [lv : OpLevel]
                 [op : MathTeX+Like])
         . [xs : MathTeX+Like *])
  (paren% #:level (make-level lv)
          op (apply dec-degree% xs)))

(define ((monoid% #:level [lv : OpLevel]
                  [unit : MathTeX+Like]
                  [bin : MathTeX+Like])
         . [xs : MathTeX+Like *]) : MathTeX+Like
  (define n (length xs))
  (cond
   [(eq? n 0) unit]
   [(eq? n 1) (list-ref xs 0)]
   [else
    (apply (paren%/curried #:level (make-level lv)) (list-join-1 xs bin))]))

(define ((big-op% #:level [lv : OpLevel]
                  [op : MathTeXAtom+Like])
         #:_ [sub : (Option MathTeX+Like) #f]
         #:^ [sup : (Option MathTeX+Like) #f]
         . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (paren% #:level (make-level lv)
          ((inst sub-sup% MathTeXAtom+Like MathTeX+Like) op #:_ sub #:^ sup)
          (apply dec-degree% xs)))

(define ((delimiter% #:left [left : MathTeX+Like]
                     #:right [right : MathTeX+Like])
         . [xs : MathTeX+Like *]) : MathTeX+Like
  (% left
     (apply (paren%/curried #:level #t #:left "" #:right "") xs)
     right))

(define ((apply-with-parens% #:left [left : MathTeX+Like "("]
                             #:right [right : MathTeX+Like ")"]
                             #:level [lv : OpLevel])
         [f : MathTeX+Like] . [xs : MathTeX+Like *]) : (Paren MathTeX+Like)
  (paren% #:level (make-level lv)
          (dec-degree% f)
          (apply (delimiter% #:left left #:right right) xs)))

(define ((sup-op% #:level [lv : OpLevel]
                  [op : MathTeX+Like])
         . [xs : MathTeX+Like *])
  (paren% #:level (make-level lv)
          ((inst sub-sup% MathTeXAtom+Like MathTeX+Like) (apply group% xs) #:^ op)))
