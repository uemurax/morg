#lang at-exp typed/racket

(require "../data/section.rkt"
         "../markup/xexpr.rkt"
         "class.rkt"
         "inline.rkt"
         "id.rkt")

(provide make-toc)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")

  (provide toc-class-name
           toc-node-class-name
           toc-edge-class-name
           toc-edge-details-class-name
           toc-edge-summary-class-name
           toc-edge-title-class-name
           toc-css)

  (define toc-class-name (class-name "toc"))
  (define toc-node-class-name (class-name "toc-node"))
  (define toc-edge-class-name (class-name "toc-edge"))
  (define toc-edge-details-class-name (class-name "toc-edge-details"))
  (define toc-edge-summary-class-name (class-name "toc-edge-summary"))
  (define toc-edge-title-class-name (class-name "toc-edge-title"))

  (define toc-css
    @string%{
      .@|toc-node-class-name| {
        list-style-type: none;
        padding-inline-start: 1em;
      }
    }))

(require 'style)

(define (make-toc [ss : (Listof Section)]) : XExprs
  (tagged% 'div
           `((class ,toc-class-name))
           (make-toc:aux ss)))

(define (make-toc:aux [ss : (Listof Section)]) : XExprs
  (apply tagged% 'ul
         `((class ,toc-node-class-name))
         (map make-toc:aux-1 ss)))

(define (make-toc:aux-1 [s : Section]) : XExprs
  (define i (section-id s))
  (define title (section-title s))
  (tagged% 'li
           `((class ,toc-edge-class-name))
           (tagged% 'details
                    `((class ,toc-edge-details-class-name))
                    (tagged% 'summary
                             `((class ,toc-edge-summary-class-name))
                             (tagged% 'a
                                      `((class ,toc-edge-title-class-name)
                                        (href ,(id->url i)))
                                      (pure-inline->xexprs title)))
                    (make-toc:aux (section-subsections s)))))
