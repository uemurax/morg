#lang typed/racket

(require "tex-plus.rkt"
         "../data/tex.rkt"
         "../data/splice.rkt"
         "../markup/tex.rkt")

(provide Atom+Like
         MathTeXAtom+Like
         MathTeX+Like
         atom+-like->atom+
         math-tex+-like->math-tex+
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
