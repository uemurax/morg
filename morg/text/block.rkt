#lang at-exp typed/racket

(require "../data/block.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "../markup/index.rkt"
         "../data/index-table.rkt"
         "splice.rkt"
         "inline.rkt"
         "state.rkt")

(provide block->text)

(: block->text : (State . -> . (Block . -> . StringTree)))

(define ((paragraph->text [st : State]) [p : Paragraph]) : StringTree
  @string%{

    @((inline->text st) (paragraph-contents p))

  })

(define ((print-index->text [st : State]) [p : PrintIndex]) : StringTree
  (define type (print-index-type p))
  (define tbl (state-index-table st))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    ((inline->text st) (index-list->inline (index-table-ref tbl type)))]
   [else @string%{}]))

(define ((block->text st) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x)
    ((paragraph->text st) x)]
   [(print-index? x)
    ((print-index->text st) x)]
   [(splice? x)
    ((splice->text (block->text st)) x)]
   [else (error "Unimplemented.")]))
