#lang at-exp typed/racket

(require "../data/article.rkt"
         "../markup/string.rkt"
         "../data/node.rkt"
         "../markup/splice.rkt"
         "block.rkt"
         "inline.rkt"
         "id.rkt"
         "numbering.rkt"
         "config.rkt")

(provide article->text)

(define ((article->text [cfg : Config]) [a : Article]) : StringTree
  @string%{

    @(head a cfg)
    @(body a cfg)
  })

(define (head [a : Article] [cfg : Config]) : StringTree
  (define tbl (config-node-table cfg))
  (define id (article-id a))
  (define in? (node-table-has-key? tbl id))
  (define h ((inline->text cfg) (article-header a)))
  (define title (article-title a))
  (define num
    @string%{@when%[in?]{@(article-node-format-index (cast (node-table-ref tbl id) ArticleNode)) }})
  (define tt
    @string%{@when%[title]{ (@((inline->text cfg) title))}})
  (define i
    (id->text id))
  @string%{
    @|num|@|i| @|h|@|tt|
  })

(define (body [a : Article] [cfg : Config]) : StringTree
  (define pf (article-proof a))
  @string%{
    @((block->text cfg) (article-contents a))
    @when%[pf]{
      @((proof->text cfg) pf)
    }
  })

(define ((proof->text [cfg : Config]) [p : Proof]) : StringTree
  @string%{

    @((inline->text cfg) (proof-header p))
    @((block->text cfg) (proof-contents p))
  })
