#lang typed/racket

(provide list-join list-join-1
         list-group
         list-map)

(module+ test
  (require typed/rackunit))

(: list-join : (All (X) ((Listof (Listof X)) (Listof X) . -> . (Listof X))))

(define (list-join xss sep)
  (match xss
   [(list) (list)]
   [(list xs) xs]
   [(list* xs ys xss)
    (append xs sep (list-join (list* ys xss) sep))]))

(define #:forall (X)
        (list-join-1 [xs : (Listof X)] [sep : X]) : (Listof X)
  (list-join (map (lambda ([x : X]) (list x)) xs) (list sep)))

(define #:forall (X Y)
        ((list-map [f : (X . -> . Y)])
         [x : (Listof X)]) : (Listof Y)
  (map f x))

(: list-group:aux (All (X) ((Listof X) Exact-Positive-Integer (Listof (Listof X)) . -> . (Listof (Listof X)))))

(define (list-group:aux ls n acc)
  (cond
   [(null? ls) (reverse acc)]
   [else
    (define-values (h t) (split-at ls n))
    (list-group:aux t n (list* h acc))]))

(define #:forall (X)
        (list-group [ls : (Listof X)] [n : Exact-Positive-Integer]) : (Listof (Listof X))
  (list-group:aux ls n (list)))

(module+ test
  (check-equal? (list-group '(0 1 2 3 4 5) 2)
                '((0 1) (2 3) (4 5)))
  (check-equal? (list-group '(0 1 2 3 4 5) 3)
                '((0 1 2) (3 4 5)))
  (check-exn exn:fail?
             (lambda ()
               (list-group '(0 1 2 3 4) 2)))
  (check-true (assert-typecheck-fail
               (list-group '(0 1 2 3) 0)
               #:result #t)))
