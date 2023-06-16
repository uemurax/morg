#lang at-exp typed/racket

(require "../data/section.rkt"
         "../data/node.rkt"
         "../data/article.rkt"
         "../data/block.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "state.rkt"
         "inline.rkt"
         "block.rkt"
         "article.rkt"
         "id.rkt"
         "numbering.rkt")

(provide section->text)

(define ((section->text [st : State]) [s : Section]) : StringTree
  @string%{

    @(header s st)

    @(body s st)

    @(sub s st)
  })

(define (header [s : Section] [st : State]) : StringTree
  (define tbl (state-node-table st))
  (define id (section-id s))
  (define in? (node-table-has-key? tbl id))
  (define title ((inline->text st) (section-title s)))
  (define num
    @string%{@when%[in?]{@(section-node-format-index (cast (node-table-ref tbl id) SectionNode)) }})
  (define i
    (id->text id))
  @string%{
    @|num|@|i| @|title|
    ----------------------------------------
  })

(define ((section-element->text [st : State]) [e : SectionElement]) : StringTree
  (cond
   [(article? e) ((article->text st) e)]
   [(block? e) ((block->text st) e)]))

(define (body [s : Section] [st : State]) : StringTree
  (apply string%
         (map (section-element->text st) (section-contents s))))

(define (sub [s : Section] [st : State]) : StringTree
  (apply string%
         (map (section->text st) (section-subsections s))))
