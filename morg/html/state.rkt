#lang typed/racket

(require "../data/index-table.rkt"
         "../data/id.rkt"
         "../data/anchor-table.rkt")

(provide (struct-out state) State)

(struct state
  ([id : Id]
   [index-table : IndexTable]
   [anchor-table : AnchorTable])
  #:transparent
  #:type-name State)
