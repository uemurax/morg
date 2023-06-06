#lang typed/racket

(provide option-map)

(define #:forall (X Y)
        (option-map [f : (X . -> . Y)] [x : (Option X)]) : (Option Y)
  (if x (f x) #f))
