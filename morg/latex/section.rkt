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
         "state.rkt"
         "config.rkt")

(provide section->latex)

(define ((section-element->latex [st : State])
         [x : SectionElement]) : tex:TextTeX
  (cond
   [(article? x) ((article->latex st) x)]
   [(block? x) ((block->latex st) x)]))

(: section->latex : (State . -> . (Section . -> . tex:TextTeX)))

(define ((section->latex st) s)
  (define cfg (state-config st))
  (define id (section-id s))
  (define tbl (state-node-table st))
  (define untbl (state-unnumbered-node-table st))
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
  (define sms (config-section-macros cfg))
  (define fbk (config-section-macro-fallback cfg))
  (define sec-macro
    (if (< depth (length sms))
        (list-ref sms depth)
        fbk))
  (define title (pure-inline->latex (section-title s)))
  (define n : TextTeXLike
    @when%[(eq? place 'numbered)]{@(section-node-format-index nd)@macro%["enskip"]})
  (define t
    @%{@|n|@|title|})
  @text-tex%{
    @macro%[sec-macro @optional-argument%{@|t|} @argument%{@|t|@(id->latex/margin id)}]
    @(id->hypertarget id)
    @(apply % (map (section-element->latex st) (section-contents s)))
    @(apply % (map (section->latex st) (section-subsections s)))
  })
