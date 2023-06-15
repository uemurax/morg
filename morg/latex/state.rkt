#lang typed/racket

(require "config.rkt"
         "../data/index-table.rkt"
         "../data/node.rkt")

(struct state
  ([config : Config]
   [index-table : IndexTable]
   [node-table : NodeTable]
   [unnumbered-node-table : NodeTable])
  #:transparent
  #:type-name State)
