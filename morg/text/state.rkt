#lang typed/racket

(require "config.rkt"
         "../data/index-table.rkt"
         "../data/node.rkt")

(provide (struct-out state) State)

(struct state
  ([config : Config]
   [index-table : IndexTable]
   [node-table : NodeTable])
  #:transparent
  #:type-name State)
