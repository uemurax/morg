#lang typed/racket

(require "../data/node.rkt")

(provide (struct-out config) Config)

(struct config
  ([node-table : NodeTable])
  #:transparent
  #:type-name Config)
