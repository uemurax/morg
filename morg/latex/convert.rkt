#lang typed/racket

(require "../data/document.rkt"
         "document.rkt"
         "config.rkt"
         "../text/tex.rkt"
         "../markup/string.rkt")

(provide ->latex)

(define (->latex #:config [cfg : Config default-config]
                 [doc : (U Document)]) : String
  (define x
    (cond
     [(document? doc) ((document->latex cfg) doc)]))
  (define y (text-tex->text x))
  (string-tree->string y))
