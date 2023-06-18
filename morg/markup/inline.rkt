#lang at-exp typed/racket

(require "../data/inline.rkt"
         "../data/id.rkt"
         "../data/splice.rkt"
         "string.rkt"
         "tex.rkt")

(provide InlineLike
         inline-like->inline
         PureInlineLike
         pure-inline-like->pure-inline
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
  (U Inline
     (Splice InlineLike)
     (InlineElement InlineLike)
     StringTreeLike))

(define-type PureInlineLike
  (U PureInline
     (Splice PureInlineLike)
     (PureInlineElement PureInlineLike)
     StringTreeLike))

(define (inline-like->inline [x : InlineLike]) : Inline
  (cond
   [(inline? x) x]
   [(splice? x)
    (inline (splice-map inline-like->inline x))]
   [((make-predicate StringTreeLike) x)
    (inline (text (string-tree-like->string x)))]
   [else (inline ((inline-element-map inline-like->inline) x))]))

(define (pure-inline-like->pure-inline [x : PureInlineLike]) : PureInline
  (cond
   [(pure-inline? x) x]
   [(splice? x)
    (pure-inline (splice-map pure-inline-like->pure-inline x))]
   [((make-predicate StringTreeLike) x)
    (pure-inline (text (string-tree-like->string x)))]
   [else (pure-inline ((pure-inline-element-map pure-inline-like->pure-inline) x))]))

(define (inline% . [xs : InlineLike *]) : Inline
  (inline-like->inline (splice xs)))

(define (pure-inline% . [xs : PureInlineLike *]) : PureInline
  (pure-inline-like->pure-inline (splice xs)))

(define (ref% [maybe-id : String]) : Ref
  (ref (id maybe-id)))

(define (math% . [xs : MathTeXLike *]) : Math
  (math (apply math-tex% xs)))

(define #:forall (Inline)
        (list-item% #:head [head : (U StringTreeLike Inline) "-"]
                    . [xs : Inline *])
  (list-item head (splice xs)))

(define #:forall (Inline)
        (unordered-list% . [xs : (ListItem Inline) *])
  (unordered-list xs))

(define (ordered-list%:default-format [n : Natural])
  @(number->string n))

(define #:forall (Inline)
        ((ordered-list%:modify-item [fmt : (Natural . -> . Inline)])
         [i : (ListItem Inline)] [n : Natural])
  (list-item
   (fmt (+ n 1))
   (list-item-contents i)))

(define #:forall (Inline)
        (ordered-list% #:format [fmt : (Natural . -> . (U StringTreeLike Inline))
                                  ordered-list%:default-format]
                       . [xs : (ListItem Inline) *])
  (define rng (range (length xs)))
  (define ys
    (map (ordered-list%:modify-item fmt) xs rng))
  (ordered-list ys))

(define #:forall (Inline)
        (href% [url : String] . [xs : Inline *])
  (define contents
    (if (null? xs)
        #f
        (splice xs)))
  (href url contents))

(define #:forall (Inline)
        (emph% . [xs : Inline *])
  (emph (splice xs)))

(define #:forall (Inline)
        (display% . [xs : Inline *])
  (display (splice xs)))

(define #:forall (Inline) 
        (code% . [xs : Inline *])
  (code (splice xs)))

(define #:forall (Inline)
        (dfn% . [xs : Inline *])
  (dfn (splice xs)))

(define #:forall (Inline)
        (anchor% #:id [maybe-id : String] . [xs : Inline *])
  (anchor (id maybe-id) (splice xs)))

(define (anchor-ref% #:anchor [maybe-anchor : String]
                     #:node [maybe-node : String]) : AnchorRef
  (anchor-ref (id maybe-anchor) (id maybe-node)))
