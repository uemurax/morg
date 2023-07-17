#lang typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt"
         "level.rkt")

(provide (struct-out paren) Paren
         (struct-out math-tex+) MathTeX+
         paren-map
         math-tex+-dec-degree)

(struct (X) paren
  ([level : Level]
   [left : X]
   [right : X]
   [contents : X])
  #:transparent
  #:type-name Paren)

(struct math-tex+
  ([contents : (U (Atom MathTeX+)
                  (Splice MathTeX+)
                  (Paren MathTeX+)
                  (SubSup (Atom MathTeX+) MathTeX+))])
  #:transparent
  #:type-name MathTeX+)

(define #:forall (X Y)
        ((paren-map [f : (X . -> . Y)])
         [p : (Paren X)]) : (Paren Y)
  (paren (paren-level p)
         (f (paren-left p))
         (f (paren-right p))
         (f (paren-contents p))))

(define #:forall (X)
        ((paren-dec-degree [f : (X . -> . X)])
         [p : (Paren X)]) : (Paren X)
  (paren (level-dec-degree (paren-level p))
         (f (paren-left p))
         (f (paren-right p))
         (f (paren-contents p))))

(define (math-tex+-dec-degree [m : MathTeX+]) : MathTeX+
  (define f math-tex+-dec-degree)
  (define g (atom-map f))
  (define x (math-tex+-contents m))
  (cond
   [(atom? x) (math-tex+ (g x))]
   [(splice? x) (math-tex+ (splice-map f x))]
   [(paren? x) (math-tex+ ((paren-dec-degree f) x))]
   [(sub-sup? x) (math-tex+ ((sub-sup-map g f) x))]))
