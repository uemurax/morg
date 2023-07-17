#lang typed/racket

(module+ test
  (require typed/rackunit))

(provide Level
         (rename-out [rational->level make-level])
         level-dec-degree
         level-compare
         ComparisonResult)

(struct id
  ()
  #:type-name Id)

(struct level
  ([value : Nonnegative-Exact-Rational]
   [id : Id]
   [degree : Nonpositive-Integer])
  #:transparent
  #:type-name Level)

(define (make-level [value : Nonnegative-Exact-Rational]
                    [id : Id]
                    [degree : Nonpositive-Integer]) : Level
  (if (value . <= . 1)
      (level value id degree)
      (error "value must be <= 1.")))

(define (rational->level [r : Nonnegative-Exact-Rational])
  (make-level r (id) 0))

(define (level-dec-degree [x : Level])
  (level (level-value x)
         (level-id x)
         (- (level-degree x) 1)))

(define-type ComparisonResult
  (U '< '= '> '?))

(define (level-compare
         [x : Level]
         [y : Level]) : ComparisonResult
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
