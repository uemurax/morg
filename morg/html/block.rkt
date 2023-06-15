#lang at-exp typed/racket

(require "../data/block.rkt"
         "../markup/xexpr.rkt"
         "../markup/index.rkt"
         "../data/splice.rkt"
         "../data/index-table.rkt"
         "class.rkt"
         "config.rkt"
         "inline.rkt"
         "splice.rkt")

(provide block->xexprs)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")
  (provide paragraph-class-name
           print-index-class-name
           block-css)

  (define paragraph-class-name (class-name "paragraph"))
  (define print-index-class-name (class-name "print-index"))

  (define block-css
    @string%{
    }))

(require 'style)

(: block->xexprs : (Config . -> . (Block . -> . XExprs)))

(define (paragraph->xexprs [x : Paragraph]) : XExprs
  (tagged% 'p
           `((class ,paragraph-class-name))
           (inline->xexprs (paragraph-contents x))))

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
