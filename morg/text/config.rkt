#lang at-exp typed/racket

(require "../data/node.rkt"
         "../markup/string.rkt")

(provide (struct-out config) Config
         (struct-out user-config) UserConfig
         default-config)

(struct config
  ([user-config : UserConfig]
   [node-table : NodeTable])
  #:transparent
  #:type-name Config)

(struct user-config
  ([make-section-ref : (Natural StringTree . -> . StringTree)])
  #:transparent
  #:type-name UserConfig)

(define (default-config:make-section-ref [_depth : Natural] [num : StringTree]) : StringTree
  @string%{Section @|num|})

(define default-config
  (user-config
   default-config:make-section-ref))
