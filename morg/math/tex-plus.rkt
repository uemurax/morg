#lang typed/racket

(require "../data/tex.rkt"
         "../data/splice.rkt")

(provide (struct-out level) Level
         (struct-out paren) Paren
         (struct-out atom+) Atom+
         (struct-out math-tex+) MathTeX+
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
   [contents : X])
  #:transparent
  #:type-name Paren)

(struct (X) atom+
  ([contents : (U Text (Macro X) (Group X) (Paren X))])
  #:transparent
  #:type-name Atom+)

(struct math-tex+
  ([contents : (U (Atom+ MathTeX+)
                  (Splice MathTeX+)
                  (SubSup MathTeX+))])
  #:transparent
  #:type-name MathTeX+)

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
