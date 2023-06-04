#lang typed/racket

(require "id.rkt"
         "inline.rkt"
         "block.rkt"
         "article.rkt")

(provide (struct-out section) Section
         (struct-out section-element) SectionElement)

(struct section
  ([id : Id]
   [title : Inline]
   [contents : (Listof SectionElement)]
   [subsections : (Listof Section)])
  #:transparent
  #:type-name Section)

(struct section-element
  ([contents : (U Article Block)])
  #:transparent
  #:type-name SectionElement)
