#lang at-exp typed/racket

(require "../data/block.rkt"
         "../markup/xexpr.rkt"
         "../markup/index.rkt"
         "../data/splice.rkt"
         "../data/index-table.rkt"
         "class/block.rkt"
         "state.rkt"
         "inline.rkt"
         "splice.rkt")

(provide block->xexprs)

(: block->xexprs : (State . -> . (Block . -> . XExprs)))

(define ((paragraph->xexprs [st : State]) [x : Paragraph]) : XExprs
  (tagged% 'p
           `((class ,paragraph-class-name))
           ((inline->xexprs st) (paragraph-contents x))))

(define ((print-index->xexprs [st : State]) [p : PrintIndex]) : XExprs
  (define tbl (state-index-table st))
  (define type (print-index-type p))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    (tagged% 'div
             `((class ,print-index-class-name))
             ((inline->xexprs st) (index-list->inline (index-table-ref tbl type))))]
   [else (xexprs%)]))

(define ((block->xexprs st) b)
  (define x (block-contents b))
  (define f (block->xexprs st))
  (cond
   [(paragraph? x) ((paragraph->xexprs st) x)]
   [(print-index? x) ((print-index->xexprs st) x)]
   [(splice? x) ((splice->xexprs f) x)]
   [else (error "Unimplemented.")]))
