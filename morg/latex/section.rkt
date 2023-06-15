#lang at-exp typed/racket

(require "../data/section.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/node.rkt"
         "../data/article.rkt"
         "../data/block.rkt"
         "../markup/tex.rkt"
         "../markup/splice.rkt"
         "../text/numbering.rkt"
         "id.rkt"
         "inline.rkt"
         "block.rkt"
         "article.rkt"
         "config.rkt")

(provide section->latex)

(define ((section-element->latex [cfg : Config])
         [x : SectionElement]) : tex:TextTeX
  (cond
   [(article? x) ((article->latex cfg) x)]
   [(block? x) ((block->latex cfg) x)]))

(: section->latex : (Config . -> . (Section . -> . tex:TextTeX)))

(define ((section->latex cfg) s)
  (define usr-cfg (config-user-config cfg))
  (define id (section-id s))
  (define tbl (config-node-table cfg))
  (define untbl (config-unnumbered-node-table cfg))
  (define place
    (if (node-table-has-key? tbl id)
        'numbered
        'unnumbered))
  (define nd
    (cast (node-table-ref (case place
                           [(numbered) tbl]
                           [(unnumbered) untbl])
                          id)
          SectionNode))
  (define depth
    (- (length (node-trace nd)) 1))
  (define sms (user-config-section-macros usr-cfg))
  (define fbk (user-config-section-macro-fallback usr-cfg))
  (define sec-macro
    (if (< depth (length sms))
        (list-ref sms depth)
        fbk))
  (define title ((inline->latex cfg) (section-title s)))
  (define n : TextTeXLike
    @when%[(eq? place 'numbered)]{@(section-node-format-index nd)@macro%["enskip"]})
  (define t
    @%{@|n|@|title|})
  @text-tex%{
    @macro%[sec-macro @optional-argument%{@|t|} @argument%{@|t|@(id->latex/margin id)}]
    @(id->hypertarget id)
    @(apply % (map (section-element->latex cfg) (section-contents s)))
    @(apply % (map (section->latex cfg) (section-subsections s)))
  })
