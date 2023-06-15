#lang at-exp typed/racket

(require "../data/node.rkt"
         "../data/index-table.rkt"
         "../markup/string.rkt")

(provide (struct-out config) Config
         default-config
         config-make-section-ref%)

(struct config
  ([make-section-ref : (Natural String . -> . StringTree)])
  #:transparent
  #:type-name Config)

(define (default-config:make-section-ref [_depth : Natural] [num : String]) : StringTree
  @string%{Section @|num|})

(define default-config
  (config
   default-config:make-section-ref))

(define ((config-make-section-ref% [cfg : Config])
         [depth : Natural] . [num : StringTreeLike *])
  ((config-make-section-ref cfg) depth (apply string-tree-like->string* num)))
