#lang at-exp typed/racket

(require "../data/document.rkt"
         "../data/section.rkt"
         "../markup/xexpr.rkt"
         "../text/date.rkt"
         "../data/index-table.rkt"
         "../data/anchor-table.rkt"
         "state.rkt"
         "inline.rkt"
         "toc.rkt"
         "section.rkt"
         "block.rkt"
         "xexpr-table.rkt"
         "config.rkt"
         "class/document.rkt")

(provide document->xexprs)

(define ((document->xexprs [cfg : Config]) [doc : Document]) : XExprTable
  (define title (document-title doc))
  (define author (document-author doc))
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define st
    (state (document-id doc)
           cfg
           (make-index-table doc)
           (make-anchor-table doc)))
  (define tbl : XExprTable (hash))
  (define (f [x : XExprTable] [ss : (Listof Section)])
    (foldl (section->xexprs st) x ss))
  (define tbl-1 (f tbl front))
  (define tbl-2 (f tbl-1 main))
  (define tbl-3 (f tbl-2 back))
  (define this-xexpr
    (tagged% 'div
             `((class ,document-class-name))
             (tagged% 'h1
                      `((class ,document-title-class-name))
                      (pure-inline->xexprs title))
             (tagged% 'address
                      `((class ,document-address-class-name))
                      (tagged% 'ul
                               `((class ,document-author-list-class-name))
                               (apply tagged% 'li
                                      `((class ,document-author-class-name))
                                      (map pure-inline->xexprs
                                           author))))
             (tagged% 'div
                      `((class ,document-date-class-name))
                      (date->text (document-date doc)))
             ((block->xexprs st) (document-contents doc))
             (tagged% 'nav
                      `((class ,document-front-toc-class-name))
                      (make-toc front))
             (tagged% 'nav
                      `((class ,document-main-toc-class-name))
                      (make-toc main))
             (tagged% 'nav
                      `((class ,document-back-toc-class-name))
                      (make-toc back))))
  (hash-set tbl-3 (document-id doc) this-xexpr))
