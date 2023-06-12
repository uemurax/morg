#lang typed/racket

(require "../data/block.rkt"
         "../markup/xexpr.rkt"
         "../markup/index.rkt"
         "../data/splice.rkt"
         "../data/index-table.rkt"
         "class.rkt"
         "config.rkt"
         "inline.rkt"
         "splice.rkt")

(provide block->xexprs
         paragraph-class-name)

(: block->xexprs : (Config . -> . (Block . -> . XExprs)))

(define paragraph-class-name (class-name "paragraph"))

(define (paragraph->xexprs [x : Paragraph]) : XExprs
  (tagged% 'p
           `((class ,paragraph-class-name))
           (inline->xexprs (paragraph-contents x))))

(define print-index-class-name (class-name "print-index"))

(define ((print-index->xexprs [cfg : Config]) [p : PrintIndex]) : XExprs
  (define tbl (config-index-table cfg))
  (define type (print-index-type p))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    (tagged% 'div
             `((class ,print-index-class-name))
             (inline->xexprs (index-list->inline (index-table-ref tbl type))))]
   [else (xexprs%)]))

(define ((block->xexprs cfg) b)
  (define x (block-contents b))
  (define f (block->xexprs cfg))
  (cond
   [(paragraph? x) (paragraph->xexprs x)]
   [(print-index? x) ((print-index->xexprs cfg) x)]
   [(splice? x) ((splice->xexprs f) x)]
   [else (error "Unimplemented.")]))
