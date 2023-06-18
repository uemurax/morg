#lang at-exp typed/racket

(require "../data/article.rkt"
         "../markup/string.rkt"
         "../data/node.rkt"
         "../markup/splice.rkt"
         "block.rkt"
         "inline.rkt"
         "id.rkt"
         "numbering.rkt"
         "state.rkt")

(provide article->text)

(define ((article->text [st : State]) [a : Article]) : StringTree
  @string%{

    @(head a st)
    @(body a st)
  })

(define (head [a : Article] [st : State]) : StringTree
  (define tbl (state-node-table st))
  (define id (article-id a))
  (define in? (node-table-has-key? tbl id))
  (define h (pure-inline->text (article-header a)))
  (define title (article-title a))
  (define num
    @string%{@when%[in?]{@(article-node-format-index (cast (node-table-ref tbl id) ArticleNode)) }})
  (define tt
    @string%{@when%[title]{ (@(pure-inline->text title))}})
  (define i
    (id->text id))
  @string%{
    @|num|@|i| @|h|@|tt|
  })

(define (body [a : Article] [st : State]) : StringTree
  (define pf (article-proof a))
  @string%{
    @((block->text st) (article-contents a))
    @when%[pf]{
      @((proof->text st) pf)
    }
  })

(define ((proof->text [st : State]) [p : Proof]) : StringTree
  @string%{

    @(pure-inline->text (proof-header p))
    @((block->text st) (proof-contents p))
  })
