#lang typed/racket

(require "id.rkt"
         "inline.rkt"
         "block.rkt"
         "article.rkt")

(provide (struct-out section) Section
         SectionElement
         section-articles)

(struct section
  ([id : Id]
   [title : Inline]
   [contents : (Listof SectionElement)]
   [subsections : (Listof Section)])
  #:transparent
  #:type-name Section)

(define-type SectionElement
  (U Article Block))

(define (section-articles [s : Section]) : (Listof Article)
  (filter article? (section-contents s)))
