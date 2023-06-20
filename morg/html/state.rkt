#lang typed/racket

(require "../data/index-table.rkt"
         "../data/id.rkt"
         "config.rkt"
         "../data/anchor-table.rkt")

(provide (struct-out state) State)

(struct state
  ([id : Id]
   [config : Config]
   [index-table : IndexTable]
   [anchor-table : AnchorTable])
  #:transparent
  #:type-name State)
