#lang at-exp typed/racket

(require "../data/document.rkt"
         "../markup/string.rkt"
         "../data/node.rkt"
         "../data/index-table.rkt"
         "../util/list.rkt"
         "section.rkt"
         "inline.rkt"
         "block.rkt"
         "date.rkt"
         "state.rkt"
         "config.rkt")

(provide document->text)

(define ((document->text [cfg : Config]) [doc : Document]) : StringTree
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define st
    (state cfg
           (make-index-table doc)
           (make-node-table main)))
  (define f pure-inline->text)
  (define g (section->text st))
  (define h (block->text st))
  @string%{
    @(f (document-title doc))
    ========================================

    @(apply string%
            (list-join-1
             (map f (document-author doc))
             ", "))

    @(date->text (document-date doc))
    @(h (document-contents doc))
    @(apply string% (map g front))
    @(apply string% (map g main))
    @(apply string% (map g back))
  })
