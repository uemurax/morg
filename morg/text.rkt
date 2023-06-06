#lang typed/racket

(require "data/document.rkt"
         "text/document.rkt"
         "text/config.rkt"
         "data/tree.rkt")

(provide (struct-out user-config) UserConfig
         default-config ->text)

(define default-config
  (user-config))

(define (->text #:config [cfg : UserConfig default-config] [doc : Document]) : String
  (apply string-append (tree-flatten ((document->text cfg) doc))))
