#lang typed/racket

(require "../data/node.rkt"
         "../data/document.rkt")

(provide (struct-out site-state) SiteState)

(struct site-state
  ([front : NodeTable]
   [main : NodeTable]
   [back : NodeTable]
   [root : Document])
  #:transparent
  #:type-name SiteState)
