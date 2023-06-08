#lang typed/racket

(require "../data/node.rkt"
         "../data/tex.rkt")

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
   [section-macro-fallback : String]
   [class : String]
   [class-options : (Listof (U String (Pairof String TextTeX)))]
   [make-section-ref : (Natural TextTeX . -> . TextTeX)])
  #:transparent
  #:type-name UserConfig)
