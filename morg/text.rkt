#lang typed/racket

(require "data/document.rkt"
         "data/section.rkt"
         "data/article.rkt"
         "data/node.rkt"
         "data/index-table.rkt"
         "text/document.rkt"
         "text/section.rkt"
         "text/article.rkt"
         "text/config.rkt"
         (only-in "data/tree.rkt" tree-flatten))

(provide ->text)

(define (->text #:config [cfg : UserConfig default-config] [doc : (U Document Section Article)]) : String
  (define x
    (cond
     [(document? doc) ((document->text cfg) doc)]
     [(section? doc)
      (define cfg-1
        (config cfg empty-index-table (make-node-table (list doc))))
      ((section->text cfg-1) doc)]
     [(article? doc)
      (define cfg-1
        (config cfg empty-index-table (make-node-table (list))))
      ((article->text cfg-1) doc)]))
  (apply string-append (tree-flatten x)))
