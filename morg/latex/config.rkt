#lang at-exp typed/racket

(require "../data/tex.rkt"
         "../data/extension.rkt"
         "../markup/string.rkt"
         "../markup/tex.rkt"
         (prefix-in text: "../text/config.rkt"))

(provide (struct-out config) Config
         config-make-section-ref%
         Option OptionList
         (struct-out package) Package
         default-config)

(define-type Option
  (U String (Pairof String TextTeXLike)))

(define-type OptionList
  (Listof Option))

(struct package
  ([name : String]
   [options : OptionList])
  #:transparent
  #:type-name Package)

(struct config
  ([section-macros : (Listof String)]
   [section-macro-fallback : String]
   [class : String]
   [class-options : OptionList]
   [packages : (Listof Package)]
   [make-section-ref : (Natural String . -> . TextTeXLike)]
   [render-extension : (ExtHash ((Listof TextTeXLike) . -> . TextTeXLike))]
   [index-num-columns : Exact-Positive-Integer]
   [front-matter : TextTeXLike]
   [main-matter : TextTeXLike]
   [back-matter : TextTeXLike])
  #:transparent
  #:type-name Config)

(define ((config-make-section-ref% [cfg : Config])
         [depth : Natural] . [num : StringTreeLike *])
  ((config-make-section-ref cfg) depth (apply string-tree-like->string* num)))

(define (default-config:make-section-ref [depth : Natural] [num : String])
  @text-tex%{@((text:config-make-section-ref text:default-config) depth num)})

(define default-config
  (config
   '("section" "subsection" "subsubsection" "paragraph")
   "subparagraph"
   "article"
   '()
   '()
   default-config:make-section-ref
   (empty-ext-hash)
   1
   @text-tex%{}
   @text-tex%{}
   @text-tex%{}))
