#lang typed/racket

(require "../data/index-table.rkt")

(provide (struct-out state) State)

(struct state
  ([index-table : IndexTable])
  #:transparent
  #:type-name State)
