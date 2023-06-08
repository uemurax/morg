#lang at-exp typed/racket

(require "../data/article.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../markup/tex.rkt"
         "../markup/splice.rkt"
         "../data/node.rkt"
         "../text/numbering.rkt"
         "block.rkt"
         "inline.rkt"
         "id.rkt"
         "config.rkt")

(provide article->latex)

(define (header-style . [xs : TextTeXLike *])
  @macro%["textbf" (argument% (apply % xs))])

(define ((proof->latex [cfg : Config])
         [p : Proof]) : tex:TextTeX
  (define arg
    @optional-argument%{@((inline->latex cfg) (proof-header p))@macro%["quad"]})
  @text-tex%{
    @environment%["trivlist"]{
      @macro%["item" arg]
      @((block->latex cfg) (proof-contents p))
    }
  })

(define ((article->latex [cfg : Config])
         [a : Article]) : tex:TextTeX
  (define tbl (config-node-table cfg))
  (define id (article-id a))
  (define in? (node-table-has-key? tbl id))
  (define num : TextTeXLike
    (cond
     [in?
      (define nd (cast (node-table-ref tbl id) ArticleNode))
      (define n (article-node-format-index nd))
      @%{@header-style{@|n|} }]
     [else @%{}]))
  (define h : TextTeXLike
    @%{@header-style{@((inline->latex cfg) (article-header a))}})
  (define title (article-title a))
  (define t : TextTeXLike
    @when%[title]{ (@((inline->latex cfg) title))})
  (define arg
    @optional-argument%{@|num|@|h|@|t|@macro%["quad"]})
  (define pf
    (article-proof a))
  @text-tex%{
    @environment%["trivlist"]{
      @macro%["item" arg]
      @(id->hypertarget id)@(id->latex/margin id)
      @((block->latex cfg) (article-contents a))
    }
    @when%[pf]{@((proof->latex cfg) pf)}
  })
