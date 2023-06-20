#lang typed/racket

(provide list-join list-join-1
         list-map)

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
