#lang typed/racket

(require "../data/block.rkt"
         "../markup/xexpr.rkt"
         "../data/splice.rkt"
         "class.rkt"
         "inline.rkt"
         "splice.rkt"
         "config.rkt")

(define paragraph-class-name (class-name "paragraph"))

(define ((paragraph->xexprs [cfg : Config])
         [x : Paragraph]) : XExprs
  (xexprs%
   (apply tagged%
          'p
          `((class ,paragraph-class-name))
          ((inline->xexprs cfg) (paragraph-contents x)))))

(: block->xexprs : (Config . -> . (Block . -> . XExprs)))

(define ((block->xexprs cfg) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x) ((paragraph->xexprs cfg) x)]
   [(splice? x) ((splice->xexprs (block->xexprs cfg)) x)]
   [else (error "Unimplemented.")]))
