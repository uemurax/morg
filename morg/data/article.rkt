#lang typed/racket

(require "inline.rkt"
         "block.rkt"
         "index.rkt"
         "id.rkt")

(provide (struct-out article) Article
         (struct-out proof) Proof)

(struct article
  ([id : Id]
   [header : Inline]
   [title : (Option Inline)]
   [indexes : (Listof Index)]
   [contents : Block]
   [proof : (Option Proof)])
  #:transparent
  #:type-name Article)

(struct proof
  ([header : Inline]
   [contents : Block])
  #:transparent
  #:type-name Proof)
