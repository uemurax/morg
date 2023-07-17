#lang typed/racket

(require "../data/document.rkt"
         "../data/section.rkt"
         "../data/article.rkt"
         "../data/node.rkt"
         "../data/index-table.rkt"
         "../data/anchor-table.rkt"
         "../markup/string.rkt"
         "document.rkt"
         "section.rkt"
         "article.rkt"
         "config.rkt"
         "state.rkt")

(provide ->text)

(define (->text #:config [cfg : Config default-config] [doc : (U Document Section Article)]) : String
  (define x
    (cond
     [(document? doc) ((document->text cfg) doc)]
     [(section? doc)
      (define st
        (state cfg empty-index-table
               empty-anchor-table
               (make-node-table (list doc))))
      ((section->text st) doc)]
     [(article? doc)
      (define st
        (state cfg empty-index-table
               empty-anchor-table
               (make-node-table (list))))
      ((article->text st) doc)]))
  (string-tree->string x))
