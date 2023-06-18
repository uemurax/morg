#lang at-exp typed/racket

(require "../data/document.rkt"
         "../data/section.rkt"
         "../markup/xexpr.rkt"
         "../text/date.rkt"
         "../data/index-table.rkt"
         "state.rkt"
         "inline.rkt"
         "toc.rkt"
         "section.rkt"
         "block.rkt"
         "xexpr-table.rkt"
         "class.rkt")

(provide document->xexprs)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")
  (provide document-class-name
           document-title-class-name
           document-address-class-name
           document-author-list-class-name
           document-author-class-name
           document-date-class-name
           document-front-toc-class-name
           document-main-toc-class-name
           document-back-toc-class-name
           document-css)

  (define document-class-name (class-name "document"))
  (define document-title-class-name (class-name "document-title"))
  (define document-address-class-name (class-name "document-address"))
  (define document-author-list-class-name (class-name "document-author-list"))
  (define document-author-class-name (class-name "document-author"))
  (define document-front-toc-class-name (class-name "document-front-toc"))
  (define document-main-toc-class-name (class-name "document-main-toc"))
  (define document-back-toc-class-name (class-name "document-back-toc"))
  (define document-date-class-name (class-name "document-date"))

  (define document-css
    @string%{
      .@|document-author-list-class-name| {
        list-style-type: none;
        padding-inline-start: 0;
      }
    }))

(require 'style)

(define (document->xexprs [doc : Document]) : XExprTable
  (define title (document-title doc))
  (define author (document-author doc))
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define itbl (make-index-table doc))
  (define cfg (state itbl))
  (define tbl : XExprTable (hash))
  (define (f [x : XExprTable] [ss : (Listof Section)])
    (foldl (section->xexprs cfg) x ss))
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
             ((block->xexprs cfg) (document-contents doc))
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
