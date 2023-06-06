#lang typed/racket

(require "../data/node.rkt")

(provide (struct-out config) Config
         (struct-out user-config) UserConfig)

(struct config
  ([user-config : UserConfig]
   [node-table : NodeTable])
  #:transparent
  #:type-name Config)

(struct user-config
  ()
  #:transparent
  #:type-name UserConfig)