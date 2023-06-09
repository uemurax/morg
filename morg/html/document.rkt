#lang typed/racket

(require "../data/document.rkt"
         "../data/section.rkt"
         "../markup/xexpr.rkt"
         "inline.rkt"
         "toc.rkt"
         "section.rkt"
         "xexpr-table.rkt"
         "config.rkt"
         "class.rkt")

(provide document->xexprs
         document-class-name
         document-title-class-name
         document-address-class-name
         document-author-list-class-name
         document-author-class-name
         document-front-toc-class-name
         document-main-toc-class-name
         document-back-toc-class-name)

(define document-class-name (class-name "document"))
(define document-title-class-name (class-name "document-title"))
(define document-address-class-name (class-name "document-address"))
(define document-author-list-class-name (class-name "document-author-list"))
(define document-author-class-name (class-name "document-author"))
(define document-front-toc-class-name (class-name "document-front-toc"))
(define document-main-toc-class-name (class-name "document-main-toc"))
(define document-back-toc-class-name (class-name "document-back-toc"))

(define ((document->xexprs [cfg : Config])
         [doc : Document]) : XExprTable
  (define title (document-title doc))
  (define author (document-author doc))
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define toc (make-toc cfg))
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
                      ((inline->xexprs cfg) title))
             (tagged% 'address
                      `((class ,document-address-class-name))
                      (tagged% 'ul
                               `((class ,document-author-list-class-name))
                               (apply tagged% 'li
                                      `((class ,document-author-class-name))
                                      (map (inline->xexprs cfg)
                                           author))))
             (tagged% 'div
                      `((class ,document-front-toc-class-name))
                      (toc front))
             (tagged% 'div
                      `((class ,document-main-toc-class-name))
                      (toc main))
             (tagged% 'div
                      `((class ,document-back-toc-class-name))
                      (toc back))))
  (hash-set tbl-3 (document-id doc) this-xexpr))
