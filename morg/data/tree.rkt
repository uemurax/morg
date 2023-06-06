#lang typed/racket

(provide (struct-out leaf) Leaf
         (struct-out node) Node
         Tree
         tree-flatten)

(struct (A) leaf
  ([contents : A])
  #:transparent
  #:type-name Leaf)

(struct (A) node
  ([contents : (Listof (Tree A))])
  #:transparent
  #:type-name Node)

(define-type (Tree A)
  (U (Leaf A) (Node A)))

(: tree-flatten : (All (A) ((Tree A) . -> . (Listof A))))

(define (tree-flatten t)
  (match t
   [(leaf t) (list t)]
   [(node t)
    (apply append (map (inst tree-flatten A) t))]))
