#lang typed/racket

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
         "xexpr-table.rkt")

(provide section->xexprs
         section-class-name
         section-title-class-name
         section-body-class-name
         section-toc-class-name)

(define (article->xexprs* [a : Article] [xtbl : XExprTable]) : XExprTable
  (hash-set xtbl (article-id a) (article->xexprs a)))

(define ((section-element->xexprs [xtbl : XExprTable])
         [e : SectionElement]) : XExprs
  (cond
   [(article? e) (hash-ref xtbl (article-id e))]
   [(block? e) (block->xexprs e)]))

(define section-class-name (class-name "section"))
(define section-title-class-name (class-name "section-title"))
(define section-body-class-name (class-name "section-body"))
(define section-toc-class-name (class-name "section-toc"))

(define (section->xexprs [s : Section] [xtbl : XExprTable]) : XExprTable
  (define xtbl-1
    (foldl article->xexprs* xtbl (section-articles s)))
  (define xtbl-2
    (foldl section->xexprs xtbl-1 (section-subsections s)))
  (define i (section-id s))
  (define this-xexpr
    (tagged% 'section
             `((class ,section-class-name))
             (tagged% 'h1
                      `((class ,section-title-class-name))
                      (id->xexprs/a i)
                      (inline->xexprs (section-title s)))
             (apply tagged%
                    'div
                    `((class ,section-body-class-name))
                    (map (section-element->xexprs xtbl-2)
                         (section-contents s)))
             (tagged% 'nav
                      `((class ,section-toc-class-name))
                      (make-toc (section-subsections s)))))
  (hash-set xtbl-2 i this-xexpr))
