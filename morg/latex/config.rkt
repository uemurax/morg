#lang at-exp typed/racket

(require "../data/node.rkt"
         "../data/tex.rkt"
         "../markup/string.rkt"
         "../markup/tex.rkt"
         (prefix-in text: "../text/config.rkt"))

(provide (struct-out config) Config
         (struct-out user-config) UserConfig
         default-config)

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
   [make-section-ref : (Natural StringTree . -> . TextTeX)])
  #:transparent
  #:type-name UserConfig)

(define (default-config:make-section-ref [depth : Natural] [num : StringTree]) : TextTeX
  @text-tex%{@((text:user-config-make-section-ref text:default-config) depth num)})

(define default-config
  (user-config
   '("section" "subsection" "subsubsection" "paragraph")
   "subparagraph"
   "article"
   '()
   default-config:make-section-ref))
