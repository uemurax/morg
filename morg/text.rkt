#lang typed/racket

(require "data/document.rkt"
         "text/document.rkt"
         "text/config.rkt"
         "data/tree.rkt")

(provide ->text)

(define (->text #:config [cfg : UserConfig default-config] [doc : Document]) : String
  (apply string-append (tree-flatten ((document->text cfg) doc))))
