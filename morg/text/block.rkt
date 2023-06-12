#lang at-exp typed/racket

(require "../data/block.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "../markup/index.rkt"
         "../data/index-table.rkt"
         "splice.rkt"
         "inline.rkt"
         "config.rkt")

(provide block->text)

(: block->text : (Config . -> . (Block . -> . StringTree)))

(define ((paragraph->text [cfg : Config]) [p : Paragraph]) : StringTree
  @string%{

    @((inline->text cfg) (paragraph-contents p))

  })

(define ((print-index->text [cfg : Config]) [p : PrintIndex]) : StringTree
  (define type (print-index-type p))
  (define tbl (config-index-table cfg))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    ((inline->text cfg) (index-list->inline (index-table-ref tbl type)))]
   [else @string%{}]))

(define ((block->text cfg) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x)
    ((paragraph->text cfg) x)]
   [(print-index? x)
    ((print-index->text cfg) x)]
   [(splice? x)
    ((splice->text (block->text cfg)) x)]
   [else (error "Unimplemented.")]))
