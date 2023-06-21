#lang typed/racket

(require "../data/node.rkt"
         "../data/document.rkt"
         "../data/id.rkt"
         "site-state.rkt"
         "id.rkt"
         "../markup/xexpr.rkt"
         "class/breadcrumb.rkt")

(provide make-breadcrumb)

(define (make-breadcrumb [st : SiteState] [n : Node]) : XExprs
  (define top-id (document-id (site-state-root st)))
  (define ids (map node-id (node-trace n)))
  (define (f [i : Id])
    (tagged% 'li
             `((class ,breadcrumb-node-class-name))
             (id->xexprs/a i)))
  (tagged% 'ol
           `((class ,breadcrumb-class-name))
           (tagged% 'li
                    `((class ,breadcrumb-top-class-name))
                    (id->xexprs/a top-id))
           (apply xexprs%
                  (map f ids))))
