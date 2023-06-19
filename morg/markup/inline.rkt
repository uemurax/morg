#lang at-exp typed/racket

(require "../data/inline.rkt"
         "../data/id.rkt"
         "../data/splice.rkt"
         "../util/option.rkt"
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
         span%
         pure-inline%
         inline%)

(define-type InlineLike
  (U Inline
     PureInline
     (Splice InlineLike)
     (InlineElement PureInlineLike InlineLike)
     StringTreeLike))

(define-type PureInlineLike
  (U PureInline
     (Splice PureInlineLike)
     (PureInlineElement PureInlineLike)
     StringTreeLike))

(define (inline-like->inline [x : InlineLike]) : Inline
  (cond
   [(inline? x) x]
   [(pure-inline? x) (pure-inline->inline x)]
   [(splice? x)
    (inline (splice-map inline-like->inline x))]
   [((make-predicate StringTreeLike) x)
    (inline (text (string-tree-like->string x)))]
   [else
    (inline ((inline-element-map pure-inline-like->pure-inline
                                 inline-like->inline)
             x))]))

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

(struct (PureInline Inline) list-item+
  ([head : PureInline]
   [id : (Option Id)]
   [contents : Inline])
  #:transparent
  #:type-name ListItem+)

(define #:forall (PureInline Inline)
        (list-item% #:head [head : (U StringTreeLike PureInline) "-"]
                    #:id [maybe-id : (Option String) #f]
                    . [xs : Inline *])
  (list-item+ head (option-map id maybe-id) (splice xs)))

(define #:forall (PureInline Inline)
        (list-item+->list-item [li : (ListItem+ PureInline Inline)])
        : (ListItem (U Inline PureInline (Anchor PureInline)))
  (define id (list-item+-id li))
  (define head (list-item+-head li))
  (define contents (list-item+-contents li))
  (if id
      (list-item (anchor id head) contents)
      (list-item head contents)))
  
(define #:forall (PureInline Inline)
        (unordered-list% . [xs : (ListItem+ PureInline Inline) *])
  (unordered-list
   (map (inst list-item+->list-item PureInline Inline) xs)))

(define (ordered-list%:default-format [n : Natural])
  (number->string n))

(define #:forall (PureInline Inline)
        ((ordered-list%:modify-item [fmt : (Natural . -> . PureInline)])
         [i : (ListItem+ PureInline Inline)] [n : Natural])
  (list-item+->list-item
   (list-item+ (fmt (+ n 1))
               (list-item+-id i)
               (list-item+-contents i))))

(define #:forall (PureInline Inline)
        (ordered-list% #:format [fmt : (Natural . -> . (U StringTreeLike PureInline))
                                  ordered-list%:default-format]
                       . [xs : (ListItem+ PureInline Inline) *])
  (define rng (range (length xs)))
  (define ys
    (map ((inst ordered-list%:modify-item (U StringTreeLike PureInline) Inline) fmt) xs rng))
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

(define #:forall (Inline)
        (span% [class-id : SpanClass]
               . [xs : Inline *])
  (span class-id xs))
