#lang typed/racket

(module+ test
  (require typed/rackunit))

(require (for-syntax racket
                     syntax/parse))

(provide Level
         (rename-out [rational->level make-level])
         level-dec-degree
         level-compare
         define-levels
         ComparisonResult)

(struct id
  ()
  #:type-name Id)

(struct level
  ([value : Exact-Rational]
   [id : Id]
   [degree : Nonpositive-Integer])
  #:transparent
  #:type-name Level-)

(define-type LevelBottom #f)
(define-type LevelTop #t)
(define-type Level
  (U Level- LevelBottom LevelTop))

(define (rational->level [r : Exact-Rational])
  (level r (id) 0))

(define (level-dec-degree [x : Level])
  (case x
   [(#f #t) x]
   [else
    (level (level-value x)
           (level-id x)
           (- (level-degree x) 1))]))

(define-type ComparisonResult
  (U '< '= '> '?))

(define (level-compare-
         [x : Level-]
         [y : Level-]) : ComparisonResult
  (define x:v (level-value x))
  (define y:v (level-value y))
  (cond
   [(x:v . < . y:v) '<]
   [(x:v . > . y:v) '>]
   [else
    (define x:i (level-id x))
    (define y:i (level-id y))
    (cond
     [(eq? x:i y:i)
      (define x:d (level-degree x))
      (define y:d (level-degree y))
      (cond
       [(x:d . < . y:d) '<]
       [(x:d . > . y:d) '>]
       [else '=])]
     [else '?])]))

(define (level-compare
         [x : Level]
         [y : Level]) : ComparisonResult
  (case x
   [(#f)
    (case y
     [(#f) '=]
     [else '<])]
   [(#t)
    (case y
     [(#t) '=]
     [else '>])]
   [else
    (case y
     [(#f) '>]
     [(#t) '<]
     [else
      (level-compare- x y)])]))

(module+ test
  (define x1 (rational->level 1/2))
  (check-equal?
   (x1 . level-compare . x1)
   '=)
  (define y1 (rational->level 3/4))
  (check-equal?
   (x1 . level-compare . y1)
   '<)
  (define z1 (rational->level 1/2))
  (check-equal?
   (x1 . level-compare . z1)
   '?)
  (define x2 (level-dec-degree x1))
  (check-equal?
   (x1 . level-compare . x2)
   '>))

(define-syntax (define-levels stx)
  (syntax-parse stx
   [(_ (~alt (~optional (~seq #:init init:number))
             (~optional (~seq #:step step:number)))
       ...
       id:identifier ...)
    (define n
      (length (syntax->list #'(id ...))))
    (define rng (range n))
    (define x0 (syntax->datum #'(~? init 0)))
    (define dx (syntax->datum #'(~? step 1)))
    (with-syntax ([(value ...)
                   (map (lambda (i) (+ x0 (* i dx))) rng)])
      #'(begin
          (define id : Exact-Rational value)
          ...))]))
