#lang typed/racket

(require "data/document.rkt"
         "latex/document.rkt"
         "latex/config.rkt"
         "text/tex.rkt"
         "markup/string.rkt")

(provide ->latex)

(define (->latex #:config [cfg : UserConfig default-config]
                 [doc : (U Document)]) : String
  (define x
    (cond
     [(document? doc) ((document->latex cfg) doc)]))
  (define y (text-tex->text x))
  (string-tree->string y))
