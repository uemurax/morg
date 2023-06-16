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
         "state.rkt")

(provide article->latex)

(define (header-style . [xs : TextTeXLike *])
  @macro%["textbf" (argument% (apply % xs))])

(define label-skip
  @macro%["hspace" @argument%{@macro%["labelsep"]}])

(define ((proof->latex [st : State])
         [p : Proof]) : tex:TextTeX
  (define arg
    @optional-argument%{@|label-skip|@header-style{@((inline->latex st) (proof-header p))}:})
  @text-tex%{
    @environment%["trivlist"]{
      @macro%["item" arg]
      @((block->latex st) (proof-contents p))
    }
  })

(define ((article->latex [st : State])
         [a : Article]) : tex:TextTeX
  (define tbl (state-node-table st))
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
    @%{@header-style{@((inline->latex st) (article-header a))}})
  (define title (article-title a))
  (define t : TextTeXLike
    @when%[title]{ (@((inline->latex st) title))})
  (define arg
    @optional-argument%{@|label-skip|@|num|@|h|@|t|:})
  (define pf
    (article-proof a))
  @text-tex%{
    @environment%["trivlist"]{
      @macro%["item" arg]
      @(id->hypertarget id)@(id->latex/margin id)
      @((block->latex st) (article-contents a))
    }
    @when%[pf]{@((proof->latex st) pf)}
  })
