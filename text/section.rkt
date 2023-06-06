#lang at-exp typed/racket

(require "../data/section.rkt"
         "../data/node.rkt"
         "../data/article.rkt"
         "../data/block.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "config.rkt"
         "inline.rkt"
         "block.rkt"
         "article.rkt"
         "numbering.rkt")

(provide section->text)

(define ((section->text [cfg : Config]) [s : Section]) : StringTree
  @string~{

    @(header s cfg)

    @(body s cfg)

    @(sub s cfg)
  })

(define (header [s : Section] [cfg : Config]) : StringTree
  (define tbl (config-node-table cfg))
  (define id (section-id s))
  (define in? (node-table-has-key? tbl id))
  (define title ((inline->text cfg) (section-title s)))
  (define num
    @string~{@when~[in?]{@(section-node-format-index (cast (node-table-ref tbl id) SectionNode)) }})
  @string~{
    @|num|@|title|
    ----------------------------------------
  })

(define ((section-element->text [cfg : Config]) [e : SectionElement]) : StringTree
  (cond
   [(article? e) ((article->text cfg) e)]
   [(block? e) ((block->text cfg) e)]))

(define (body [s : Section] [cfg : Config]) : StringTree
  (apply string~
         (map (section-element->text cfg) (section-contents s))))

(define (sub [s : Section] [cfg : Config]) : StringTree
  (apply string~
         (map (section->text cfg) (section-subsections s))))
