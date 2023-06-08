#lang typed/racket

(require "data/document.rkt"
         "latex/document.rkt"
         "latex/config.rkt"
         "text/tex.rkt"
         (only-in "data/tree.rkt" tree-flatten))

(provide ->latex)

(define (->latex #:config [cfg : UserConfig default-config]
                 [doc : (U Document)]) : String
  (define x
    (cond
     [(document? doc) ((document->latex cfg) doc)]))
  (define y (text-tex->text x))
  (apply string-append (tree-flatten y)))
