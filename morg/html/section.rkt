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

(define ((article->xexprs* [st : State]) [a : Article] [xtbl : XExprTable]) : XExprTable
  (hash-set xtbl (article-id a) ((article->xexprs st) a)))

(define ((section-element->xexprs [st : State] [xtbl : XExprTable])
         [e : SectionElement]) : XExprs
  (cond
   [(article? e) (hash-ref xtbl (article-id e))]
   [(block? e) ((block->xexprs st) e)]))

(: section->xexprs : (State . -> . (Section XExprTable . -> . XExprTable)))

(define ((section->xexprs st-1) s xtbl)
  (define i (section-id s))
  (define st
    (struct-copy state st-1
     [id i]))
  (define xtbl-1
    (foldl (article->xexprs* st) xtbl (section-articles s)))
  (define xtbl-2
    (foldl (section->xexprs st) xtbl-1 (section-subsections s)))
  (define this-xexpr
    (tagged% 'section
             `((class ,section-class-name))
             (tagged% 'h1
                      `((class ,section-title-class-name))
                      (id->xexprs/a i)
                      " "
                      (pure-inline->xexprs (section-title s)))
             (apply tagged%
                    'div
                    `((class ,section-body-class-name))
                    (map (section-element->xexprs st xtbl-2)
                         (section-contents s)))
             (tagged% 'nav
                      `((class ,section-toc-class-name))
                      (make-toc (section-subsections s)))))
  (hash-set xtbl-2 i this-xexpr))
