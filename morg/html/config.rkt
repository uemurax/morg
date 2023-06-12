#lang typed/racket

(require "../data/index-table.rkt")

(provide (struct-out config) Config)

(struct config
  ([index-table : IndexTable])
  #:transparent
  #:type-name Config)
