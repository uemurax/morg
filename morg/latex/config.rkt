#lang typed/racket

(require "../data/node.rkt")

(provide (struct-out config) Config
         (struct-out user-config) UserConfig)

(struct config
  ([user-config : UserConfig]
   [node-table : NodeTable]
   [unnumbered-node-table : NodeTable])
  #:transparent
  #:type-name Config)

(struct user-config
  ([section-macros : (Listof String)]
   [section-macro-fallback : String])
  #:transparent
  #:type-name UserConfig)
