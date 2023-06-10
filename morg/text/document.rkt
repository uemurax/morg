#lang at-exp typed/racket

(require "../data/document.rkt"
         "../markup/string.rkt"
         "../data/node.rkt"
         "../util/list.rkt"
         "section.rkt"
         "inline.rkt"
         "block.rkt"
         "config.rkt")

(provide document->text)

(define ((document->text [user-cfg : UserConfig]) [doc : Document]) : StringTree
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define cfg
    (config user-cfg
            (make-node-table main)))
  (define f (inline->text cfg))
  (define g (section->text cfg))
  (define h (block->text cfg))
  @string%{
    @(f (document-title doc))
    ========================================

    @(apply string%
            (list-join-1
             (map f (document-author doc))
             ", "))
    @(h (document-contents doc))
    @(apply string% (map g front))
    @(apply string% (map g main))
    @(apply string% (map g back))
  })
