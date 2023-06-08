#lang typed/racket

(require "../data/tree.rkt"
         "../data/splice.rkt")

(provide StringTree StringTreeLike
         string-tree-like->string-tree
         string-tree-like->string
         string%)

(define-type StringTree
  (Tree String))

(define-type StringTreeLike
  (Rec X (U StringTree String (Splice X))))

(define (string-tree-like->string-tree [x : StringTreeLike]) : StringTree
  (cond
   [((make-predicate StringTree) x) x]
   [(string? x) (leaf x)]
   [(splice? x)
    (node (map string-tree-like->string-tree (splice-contents x)))]))

(define (string% . [xs : StringTreeLike *]) : StringTree
  (string-tree-like->string-tree (splice xs)))

(define (string-tree->string [x : StringTree]) : String
  (apply string-append (tree-flatten x)))

(define (string-tree-like->string [x : StringTreeLike]) : String
  (string-tree->string (string-tree-like->string-tree x)))
