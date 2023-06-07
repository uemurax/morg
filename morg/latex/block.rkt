#lang typed/racket

(require "../data/block.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "config.rkt")

(provide block->latex)

(: block->latex : (Config . -> . (Block . -> . tex:TextTeX)))

(define ((block->latex cfg) b)
  (cond
   [else (error "Unimplemented.")]))
