#lang typed/racket

(require "../data/block.rkt"
         "../markup/xexpr.rkt"
         "../data/splice.rkt"
         "class.rkt"
         "inline.rkt"
         "splice.rkt")

(provide block->xexprs
         paragraph-class-name)

(define paragraph-class-name (class-name "paragraph"))

(define (paragraph->xexprs [x : Paragraph]) : XExprs
  (tagged% 'p
           `((class ,paragraph-class-name))
           (inline->xexprs (paragraph-contents x))))

(define (block->xexprs [b : Block]) : XExprs
  (define x (block-contents b))
  (cond
   [(paragraph? x) (paragraph->xexprs x)]
   [(splice? x) ((splice->xexprs block->xexprs) x)]
   [else (error "Unimplemented.")]))
