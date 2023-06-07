#lang typed/racket

(require "../data/inline.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "config.rkt")

(provide inline->latex)

(: inline->latex : (Config . -> . (Inline . -> . tex:TextTeX)))

(define ((inline->latex cfg) i)
  (cond
   [else (error "Unimplemented.")]))
