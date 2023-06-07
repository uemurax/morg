#lang typed/racket

(require "../data/document.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "config.rkt")

(provide document->latex)

(define ((document->latex [cfg : UserConfig])
         [doc : Document]) : tex:TextTeX
  (error "Unimplemented"))
