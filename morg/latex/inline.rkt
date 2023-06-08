#lang at-exp typed/racket

(require "../data/inline.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../markup/tex.rkt"
         "config.rkt")

(provide inline->latex)

(: inline->latex : (Config . -> . (Inline . -> . tex:TextTeX)))

(define ((text->latex [_cfg : Config])
          [x : Text]) : tex:TextTeX
  @text-tex%{@(text-contents x)})

(define ((math->latex [_cfg : Config])
         [x : Math]) : tex:TextTeX
  (tex:text-tex (tex:math (math-contents x))))

(define ((inline->latex cfg) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->latex cfg) x)]
   [(math? x) ((math->latex cfg) x)]
   [else (error "Unimplemented.")]))
