#lang at-exp typed/racket

(require "../data/section.rkt"
         "../data/id.rkt"
         "../markup/xexpr.rkt"
         "class/toc.rkt"
         "pure-inline.rkt"
         "id.rkt")

(provide make-toc)

(define (make-toc [ss : (Listof Section)]
                  [tr : (Option (Listof Id)) #f]) : XExprs
  (tagged% 'div
           `((class ,toc-class-name))
           ((make-toc:aux tr) ss)))

(: make-toc:aux : ((Option (Listof Id)) . -> . ((Listof Section) . -> . XExprs)))

(define ((make-toc:aux tr) ss)
  (apply tagged% 'ul
         `((class ,toc-node-class-name))
         (map (make-toc:aux-1 tr) ss)))

(: make-toc:aux-1 : ((Option (Listof Id)) . -> . (Section . -> . XExprs)))

(define ((make-toc:aux-1 tr) s)
  (define i (section-id s))
  (define title (section-title s))
  (define-values (c tr-1)
    (match tr
     [(list* j tr)
      #:when (eq? i j)
      (values
       (if (null? tr)
           'select
           'open)
       tr)]
     [_ (values #f #f)]))
  (tagged% 'li
           `((class ,toc-edge-class-name))
           (tagged% 'details
                    `((class ,toc-edge-details-class-name)
                      ,@(case c
                         [(open) `((open "true"))]
                         [else '()]))
                    (tagged% 'summary
                             `((class ,(case c
                                        [(select) toc-edge-summary-selected-class-name]
                                        [else toc-edge-summary-class-name])))
                             (tagged% 'a
                                      `((class ,toc-edge-title-class-name)
                                        (href ,(id->url i)))
                                      (pure-inline->xexprs title)))
                    ((make-toc:aux tr-1) (section-subsections s)))))
