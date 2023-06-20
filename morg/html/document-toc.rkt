#lang typed/racket

(require "../data/document.rkt"
         "../markup/xexpr.rkt"
         "class/document-toc.rkt"
         "toc.rkt")

(provide make-document-toc)

(define (make-document-toc [doc : Document]) : XExprs
  (tagged% 'ol
           `((class ,document-toc-class-name))
           (tagged% 'li
                    `((class ,document-front-toc-class-name))
                    (make-toc (document-front doc)))
           (tagged% 'li
                    `((class ,document-main-toc-class-name))
                    (make-toc (document-main doc)))
           (tagged% 'li
                    `((class ,document-back-toc-class-name))
                    (make-toc (document-back doc)))))
