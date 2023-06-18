#lang typed/racket

(require "splice.rkt"
         "id.rkt"
         "../util/option.rkt"
         (only-in "tex.rkt" MathTeX))

(provide InlineElement
         PureInlineElement
         pure-inline-element-map
         inline-element-map
         pure-inline->inline
         (struct-out inline) Inline
         (struct-out pure-inline) PureInline
         (struct-out ref) Ref
         (struct-out text) Text
         (struct-out list-item) ListItem
         (struct-out unordered-list) UnorderedList
         (struct-out ordered-list) OrderedList
         (struct-out href) HRef
         (struct-out emph) Emph
         (struct-out display) Display
         (struct-out code) Code
         (struct-out dfn) Dfn
         (struct-out anchor) Anchor
         (struct-out anchor-ref) AnchorRef
         (struct-out math) Math)

(struct pure-inline
  ([contents : (U (Splice PureInline) (PureInlineElement PureInline))])
  #:transparent
  #:type-name PureInline)

(struct inline
  ([contents : (U (Splice Inline) (InlineElement PureInline Inline))])
  #:transparent
  #:type-name Inline)

(define-type (PureInlineElement X)
  (U Text
     Math
     (UnorderedList X)
     (OrderedList X)
     (HRef X)
     (Emph X)
     (Display X)
     (Code X)
     (Dfn X)))

(define-type (InlineElement PureInline Inline)
  (U (PureInlineElement Inline)
     Ref
     (Anchor PureInline)
     AnchorRef))

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)

(struct ref
  ([id : Id])
  #:transparent
  #:type-name Ref)

(struct math
  ([contents : MathTeX])
  #:transparent
  #:type-name Math)

(struct (Inline) list-item
  ([head : Inline]
   [contents : Inline])
  #:transparent
  #:type-name ListItem)

(struct (Inline) unordered-list
  ([contents : (Listof (ListItem Inline))])
  #:transparent
  #:type-name UnorderedList)

(struct (Inline) ordered-list
  ([contents : (Listof (ListItem Inline))])
  #:transparent
  #:type-name OrderedList)

(struct (Inline) href
  ([url : String]
   [contents : (Option Inline)])
  #:transparent
  #:type-name HRef)

(struct (Inline) emph
  ([contents : Inline])
  #:transparent
  #:type-name Emph)

(struct (Inline) display
  ([contents : Inline])
  #:transparent
  #:type-name Display)

(struct (Inline) code
  ([contents : Inline])
  #:transparent
  #:type-name Code)

(struct (Inline) dfn
  ([contents : Inline])
  #:transparent
  #:type-name Dfn)

(struct (PureInline) anchor
  ([id : Id]
   [contents : PureInline])
  #:transparent
  #:type-name Anchor)

(struct anchor-ref
  ([anchor : Id]
   [node : Id])
  #:transparent
  #:type-name AnchorRef)

(define #:forall (X Y)
        ((list-item-map [f : (X . -> . Y)])
         [x : (ListItem X)]) : (ListItem Y)
  (list-item (f (list-item-head x))
             (f (list-item-contents x))))

(define #:forall (X Y)
        ((unordered-list-map [f : (X . -> . Y)])
         [x : (UnorderedList X)]) : (UnorderedList Y)
  (unordered-list (map (list-item-map f) (unordered-list-contents x))))

(define #:forall (X Y)
        ((ordered-list-map [f : (X . -> . Y)])
         [x : (OrderedList X)]) : (OrderedList Y)
  (ordered-list (map (list-item-map f) (ordered-list-contents x))))

(define #:forall (X Y)
        ((href-map [f : (X . -> . Y)])
         [x : (HRef X)]) : (HRef Y)
  (href (href-url x)
        (option-map f (href-contents x))))

(define #:forall (X Y)
        ((emph-map [f : (X . -> . Y)])
         [x : (Emph X)]) : (Emph Y)
  (emph (f (emph-contents x))))

(define #:forall (X Y)
        ((display-map [f : (X . -> . Y)])
         [x : (Display X)]) : (Display Y)
  (display (f (display-contents x))))

(define #:forall (X Y)
        ((code-map [f : (X . -> . Y)])
         [x : (Code X)]) : (Code Y)
  (code (f (code-contents x))))

(define #:forall (X Y)
        ((dfn-map [f : (X . -> . Y)])
         [x : (Dfn X)]) : (Dfn Y)
  (dfn (f (dfn-contents x))))

(define #:forall (X Y)
        ((anchor-map [f : (X . -> . Y)])
         [x : (Anchor X)]) : (Anchor Y)
  (anchor (anchor-id x)
          (f (anchor-contents x))))

(define #:forall (X Y)
        ((pure-inline-element-map [f : (X . -> . Y)])
         [x : (PureInlineElement X)]) : (PureInlineElement Y)
  (cond
   [(text? x) x]
   [(math? x) x]
   [(unordered-list? x) ((unordered-list-map f) x)]
   [(ordered-list? x) ((ordered-list-map f) x)]
   [(href? x) ((href-map f) x)]
   [(emph? x) ((emph-map f) x)]
   [(display? x) ((display-map f) x)]
   [(code? x) ((code-map f) x)]
   [(dfn? x) ((dfn-map f) x)]))

(define #:forall (X1 X2 Y1 Y2)
        ((inline-element-map [f : (X1 . -> . X2)]
                             [g : (Y1 . -> . Y2)])
         [x : (InlineElement X1 Y1)]) : (InlineElement X2 Y2)
  (cond
   [(ref? x) x]
   [(anchor? x) ((anchor-map f) x)]
   [(anchor-ref? x) x]
   [else ((pure-inline-element-map g) x)]))

(define (pure-inline->inline [x : PureInline]) : Inline
  (define y (pure-inline-contents x))
  (cond
   [(splice? y) (inline (splice-map pure-inline->inline y))]
   [else (inline ((pure-inline-element-map pure-inline->inline) y))]))
