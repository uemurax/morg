#lang typed/racket

(require "../data/inline.rkt"
         "../data/id.rkt"
         "../data/splice.rkt"
         "string.rkt"
         "tex.rkt")

(provide InlineLike
         inline-like->inline
         ref%
         math%
         list-item%
         unordered-list%
         href%
         inline%)

(define-type InlineLike
  (Rec X (U Inline
            (Splice X)
            Text
            Ref
            Math
            UnorderedList
            HRef
            StringTreeLike)))

(define (inline-like->inline [x : InlineLike]) : Inline
  (cond
   [(inline? x) x]
   [(splice? x)
    (inline (splice-map inline-like->inline x))]
   [((make-predicate StringTreeLike) x)
    (inline (text (string-tree-like->string x)))]
   [else (inline x)]))

(define (inline% . [xs : InlineLike *]) : Inline
  (inline-like->inline (splice xs)))

(define (ref% [maybe-id : String]) : Ref
  (ref (id maybe-id)))

(define (math% . [xs : MathTeXLike *]) : Math
  (math (apply math-tex% xs)))

(define (list-item% . [xs : InlineLike *]) : ListItem
  (list-item (apply inline% xs)))

(define (unordered-list% . [xs : ListItem *]) : UnorderedList
  (unordered-list xs))

(define (href% [url : String] . [xs : InlineLike *]) : HRef
  (define contents
    (if (null? xs)
        #f
        (apply inline% xs)))
  (href url contents))
