#lang at-exp typed/racket

(require "../data/section.rkt"
         "../markup/xexpr.rkt"
         "class/toc.rkt"
         "inline.rkt"
         "id.rkt")

(provide make-toc)

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
