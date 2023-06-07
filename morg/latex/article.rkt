#lang typed/racket

(require "../data/article.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "config.rkt")

(provide article->latex)

(define ((article->latex [cfg : Config])
         [a : Article]) : tex:TextTeX
  (error "Unimplemented."))
