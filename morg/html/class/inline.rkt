#lang typed/racket

(require "../class.rkt")

(provide katex-class-name
         ref-class-name
         list-item-class-name
         list-item-head-class-name
         unordered-list-class-name
         ordered-list-class-name
         href-class-name
         emph-class-name
         display-class-name
         code-class-name
         dfn-class-name
         anchor-class-name
         anchor-ref-class-name
         inline-ext-class-name)

(define katex-class-name (class-name "katex"))
(define ref-class-name (class-name "ref"))
(define list-item-class-name (class-name "list-item"))
(define list-item-head-class-name (class-name "list-item-head"))
(define unordered-list-class-name (class-name "unordered-list"))
(define ordered-list-class-name (class-name "ordered-list"))
(define href-class-name (class-name "href"))
(define emph-class-name (class-name "emph"))
(define display-class-name (class-name "display"))
(define code-class-name (class-name "code"))
(define dfn-class-name (class-name "dfn"))
(define anchor-class-name (class-name "anchor"))
(define anchor-ref-class-name (class-name "anchor-ref"))
(define inline-ext-class-name (class-name "inline-ext"))
