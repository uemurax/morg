#lang at-exp typed/racket

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
         ordered-list%
         href%
         emph%
         display%
         code%
         dfn%
         anchor%
         anchor-ref%
         inline%)

(define-type InlineLike
  (Rec X (U Inline
            (Splice X)
            Text
            Ref
            Math
            UnorderedList
            OrderedList
            HRef
            Emph
            Display
            Code
            Dfn
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

(define (list-item% #:head [head : StringTreeLike "-"]
                    . [xs : InlineLike *]) : ListItem
  (list-item (string-tree-like->string head) (apply inline% xs)))

(define (unordered-list% . [xs : ListItem *]) : UnorderedList
  (unordered-list xs))

(define (ordered-list%:default-format [n : Natural])
  @string%{@(number->string n)})

(define ((ordered-list%:modify-item [fmt : (Natural . -> . StringTreeLike)])
        [i : ListItem] [n : Natural])
  (list-item
   (string-tree-like->string (fmt (+ n 1)))
   (list-item-contents i)))

(define (ordered-list% #:format [fmt : (Natural . -> . StringTreeLike)
                                  ordered-list%:default-format]
                       . [xs : ListItem *]) : OrderedList
  (define rng (range (length xs)))
  (define ys
    (map (ordered-list%:modify-item fmt) xs rng))
  (ordered-list ys))

(define (href% [url : String] . [xs : InlineLike *]) : HRef
  (define contents
    (if (null? xs)
        #f
        (apply inline% xs)))
  (href url contents))

(define (emph% . [xs : InlineLike *]) : Emph
  (emph (apply inline% xs)))

(define (display% . [xs : InlineLike *]) : Display
  (display (apply inline% xs)))

(define (code% . [xs : InlineLike *]) : Code
  (code (apply inline% xs)))

(define (dfn% . [xs : InlineLike *]) : Dfn
  (dfn (apply inline% xs)))

(define (anchor% #:id [maybe-id : String] . [xs : InlineLike *]) : Anchor
  (anchor (id maybe-id) (apply inline% xs)))

(define (anchor-ref% #:anchor [maybe-anchor : String]
                     #:node [maybe-node : String]) : AnchorRef
  (anchor-ref (id maybe-anchor) (id maybe-node)))
