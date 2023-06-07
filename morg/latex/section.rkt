#lang typed/racket

(require "../data/section.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "config.rkt")

(provide section->latex)

(define ((section->latex [cfg : Config])
         [s : Section]) : tex:TextTeX
  (error "Unimplemented."))
