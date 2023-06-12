#lang at-exp typed/racket

(require "../data/block.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/splice.rkt"
         "../markup/tex.rkt"
         "../markup/index.rkt"
         "../data/index-table.rkt"
         "inline.rkt"
         "splice.rkt"
         "config.rkt")

(provide block->latex)

(: block->latex : (Config . -> . (Block . -> . tex:TextTeX)))

(define ((paragraph->latex [cfg : Config])
          [x : Paragraph]) : tex:TextTeX
  @text-tex%{
    
    
    @((inline->latex cfg) (paragraph-contents x))
  })

(define ((print-index->latex [cfg : Config])
         [p : PrintIndex]) : tex:TextTeX
  (define usr-cfg (config-user-config cfg))
  (define n (user-config-index-num-columns usr-cfg))
  (define tbl (config-index-table cfg))
  (define type (print-index-type p))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    @text-tex%{
      @((inst environment% TextTeXLike) "multicols"
        #:arguments (list @argument%[(number->string n)])
        @((inline->latex cfg) (index-list->inline (index-table-ref tbl type))))
    }]
   [else @text-tex%{}]))

(define ((block->latex cfg) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x) ((paragraph->latex cfg) x)]
   [(print-index? x) ((print-index->latex cfg) x)]
   [(splice? x) ((splice->latex (block->latex cfg)) x)]
   [else (error "Unimplemented.")]))
