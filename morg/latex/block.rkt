#lang at-exp typed/racket

(require "../data/block.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/splice.rkt"
         "../markup/tex.rkt"
         "../markup/index.rkt"
         "../data/index-table.rkt"
         "inline.rkt"
         "splice.rkt"
         "state.rkt"
         "config.rkt")

(provide block->latex
         strip-first-par)

(: block->latex : (State . -> . (Block . -> . tex:TextTeX)))

(define par @macro%["par"])

(define ((paragraph->latex [st : State])
          [x : Paragraph]) : tex:TextTeX
  @text-tex%{
    @|par|@((inline->latex st) (paragraph-contents x))
  })

(define ((print-index->latex [st : State])
         [p : PrintIndex]) : tex:TextTeX
  (define cfg (state-config st))
  (define n (config-index-num-columns cfg))
  (define tbl (state-index-table st))
  (define type (print-index-type p))
  (define in? (index-table-has-key? tbl type))
  (cond
   [in?
    @text-tex%{
      @((inst environment% TextTeXLike) "multicols"
        #:arguments (list @argument%[(number->string n)])
        @((inline->latex st) (index-list->inline (index-table-ref tbl type))))
    }]
   [else @text-tex%{}]))

(define ((block->latex st) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x) ((paragraph->latex st) x)]
   [(print-index? x) ((print-index->latex st) x)]
   [(splice? x) ((splice->latex (block->latex st)) x)]
   [else (error "Unimplemented.")]))

(: strip-first-par : (tex:TextTeX . -> . tex:TextTeX))

(define (strip-first-par x)
  (define c (tex:text-tex-contents x))
  (match c
   [(tex:atom a)
    #:when (a . equal? . par)
    @text-tex%{}]
   [(splice (list* y r))
    (apply text-tex% (strip-first-par y) r)]
   [_ x]))
