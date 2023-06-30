#lang typed/racket

(require "../data/document.rkt"
         "../data/id.rkt"
         "../markup/xexpr.rkt"
         "class/document-toc.rkt"
         "toc.rkt")

(provide make-document-toc)

(define (make-document-toc [doc : Document]
                           [tr : (Option (Listof Id)) #f]) : XExprs
  (tagged% 'ol
           `((class ,document-toc-class-name))
           (tagged% 'li
                    `((class ,document-front-toc-class-name))
                    (make-toc (document-front doc) tr))
           (tagged% 'li
                    `((class ,document-main-toc-class-name))
                    (make-toc (document-main doc) tr))
           (tagged% 'li
                    `((class ,document-back-toc-class-name))
                    (make-toc (document-back doc) tr))))
