#lang typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt")

(provide (struct-out level) Level
         (struct-out paren) Paren
         (struct-out math-tex+) MathTeX+
         paren-map
         math-tex+-dec-degree
         ComparisonResult
         level-compare)

(module+ test
  (require typed/rackunit))

(struct level
  ([symbol : (U Symbol #f #t)]
   [degree : Nonpositive-Integer])
  #:transparent
  #:type-name Level)

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

(define (level-dec-degree [lv : Level]) : Level
  (level (level-symbol lv) (- (level-degree lv) 1)))

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

(define-type ComparisonResult
  (U '< '= '> '?))

(define ((level-compare:symbol [ss : (Listof Symbol)])
         [x : (U Symbol #f #t)] [y : (U Symbol #f #t)]) : ComparisonResult
  (cond
   [(eq? x #f) (if (eq? y #f) '= '<)]
   [(eq? x #t) (if (eq? y #t) '= '>)]
   [else
    (cond
     [(eq? y #f) '>]
     [(eq? y #t) '<]
     [else
      (define nx (index-of ss x))
      (define ny (index-of ss y))
      (cond
       [(and nx ny)
        (cond 
         [(nx . < . ny) '<]
         [(nx . > . ny) '>]
         [else '=])]
       [else '?])])]))

(module+ test
  (define ss '(* + bin comma))
  (define compare (level-compare:symbol ss))
  (check-equal? ('+ . compare . '+) '=)
  (check-equal? ('+ . compare . #f) '>)
  (check-equal? ('+ . compare . #t) '<)
  (check-equal? ('+ . compare . '*) '>)
  (check-equal? ('+ . compare . 'comma) '<)
  (check-equal? ('~ . compare . '+) '?)
  (check-equal? ('~ . compare . #t) '<))

(define ((level-compare [ss : (Listof Symbol)])
         [x : Level] [y : Level]) : ComparisonResult
  (define cs
    ((level-symbol x) . (level-compare:symbol ss) . (level-symbol y)))
  (case cs
   [(< > ?) cs]
   [else
    (define nx (level-degree x))
    (define ny (level-degree y))
    (cond
     [(nx . < . ny) '<]
     [(nx . > . ny) '>]
     [else '=])]))
