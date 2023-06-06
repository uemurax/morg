#lang typed/racket

(require "../data/tree.rkt"
         "../data/splice.rkt")

(provide StringTree StringTreeLike
         string-tree-like->string
         string~)

(define-type StringTree
  (Tree String))

(define-type StringTreeLike
  (Rec X (U StringTree String (Splice X))))

(define (string-tree-like->string [x : StringTreeLike]) : StringTree
  (cond
   [((make-predicate StringTree) x) x]
   [(string? x) (leaf x)]
   [(splice? x)
    (node (map string-tree-like->string (splice-contents x)))]))

(define (string~ . [xs : StringTreeLike *]) : StringTree
  (string-tree-like->string (splice xs)))
