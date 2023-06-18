#lang typed/racket

(require "splice.rkt"
         "id.rkt"
         (only-in "tex.rkt" MathTeX))

(provide (struct-out inline) Inline
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

(struct inline
  ([contents : (InlineElement Inline)])
  #:transparent
  #:type-name Inline)

(define-type (InlineElement X)
  (U (Splice X)
     Ref
     Math
     UnorderedList
     OrderedList
     HRef
     Emph
     Display
     Code
     Dfn
     Anchor
     AnchorRef
     Text))

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

(struct list-item
  ([head : String]
   [contents : Inline])
  #:transparent
  #:type-name ListItem)

(struct unordered-list
  ([contents : (Listof ListItem)])
  #:transparent
  #:type-name UnorderedList)

(struct ordered-list
  ([contents : (Listof ListItem)])
  #:transparent
  #:type-name OrderedList)

(struct href
  ([url : String]
   [contents : (Option Inline)])
  #:transparent
  #:type-name HRef)

(struct emph
  ([contents : Inline])
  #:transparent
  #:type-name Emph)

(struct display
  ([contents : Inline])
  #:transparent
  #:type-name Display)

(struct code
  ([contents : Inline])
  #:transparent
  #:type-name Code)

(struct dfn
  ([contents : Inline])
  #:transparent
  #:type-name Dfn)

(struct anchor
  ([id : Id]
   [contents : Inline])
  #:transparent
  #:type-name Anchor)

(struct anchor-ref
  ([anchor : Id]
   [node : Id])
  #:transparent
  #:type-name AnchorRef)
