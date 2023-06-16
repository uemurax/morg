#lang at-exp typed/racket

(require "../data/section.rkt"
         "../data/article.rkt"
         "../data/block.rkt"
         "../data/id.rkt"
         "../markup/xexpr.rkt"
         "article.rkt"
         "class.rkt"
         "inline.rkt"
         "block.rkt"
         "toc.rkt"
         "id.rkt"
         "state.rkt"
         "xexpr-table.rkt")

(provide section->xexprs)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")

  (provide section-class-name
           section-title-class-name
           section-body-class-name
           section-toc-class-name
           section-css)

  (define section-class-name (class-name "section"))
  (define section-title-class-name (class-name "section-title"))
  (define section-body-class-name (class-name "section-body"))
  (define section-toc-class-name (class-name "section-toc"))

  (define section-css
    @string%{
    }))

(require 'style)

(define ((article->xexprs* [cfg : State]) [a : Article] [xtbl : XExprTable]) : XExprTable
  (hash-set xtbl (article-id a) ((article->xexprs cfg) a)))

(define ((section-element->xexprs [cfg : State] [xtbl : XExprTable])
         [e : SectionElement]) : XExprs
  (cond
   [(article? e) (hash-ref xtbl (article-id e))]
   [(block? e) ((block->xexprs cfg) e)]))

(: section->xexprs : (State . -> . (Section XExprTable . -> . XExprTable)))

(define ((section->xexprs cfg) s xtbl)
  (define xtbl-1
    (foldl (article->xexprs* cfg) xtbl (section-articles s)))
  (define xtbl-2
    (foldl (section->xexprs cfg) xtbl-1 (section-subsections s)))
  (define i (section-id s))
  (define this-xexpr
    (tagged% 'section
             `((class ,section-class-name))
             (tagged% 'h1
                      `((class ,section-title-class-name))
                      (id->xexprs/a i)
                      " "
                      (inline->xexprs (section-title s)))
             (apply tagged%
                    'div
                    `((class ,section-body-class-name))
                    (map (section-element->xexprs cfg xtbl-2)
                         (section-contents s)))
             (tagged% 'nav
                      `((class ,section-toc-class-name))
                      (make-toc (section-subsections s)))))
  (hash-set xtbl-2 i this-xexpr))
