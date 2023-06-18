#lang typed/racket

(require "inline.rkt"
         "block.rkt"
         "index.rkt"
         "id.rkt")

(provide (struct-out article) Article
         (struct-out proof) Proof)

(struct article
  ([id : Id]
   [header : PureInline]
   [title : (Option PureInline)]
   [indexes : (Listof Index)]
   [contents : Block]
   [proof : (Option Proof)])
  #:transparent
  #:type-name Article)

(struct proof
  ([header : PureInline]
   [contents : Block])
  #:transparent
  #:type-name Proof)
