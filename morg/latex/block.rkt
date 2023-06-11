#lang at-exp typed/racket

(require "../data/block.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/splice.rkt"
         "../markup/tex.rkt"
         "inline.rkt"
         "splice.rkt"
         "config.rkt")

(provide block->latex)

(: block->latex : (Config . -> . (Block . -> . tex:TextTeX)))

(define ((paragraph->latex [cfg : Config])
          [x : Paragraph]) : tex:TextTeX
  @text-tex%{
    
    
    @((inline->latex cfg) (paragraph-contents x))
  })

(define ((block->latex cfg) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x) ((paragraph->latex cfg) x)]
   [(splice? x) ((splice->latex (block->latex cfg)) x)]
   [else (error "Unimplemented.")]))
