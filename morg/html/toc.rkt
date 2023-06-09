#lang typed/racket

(require "../data/section.rkt"
         "../markup/xexpr.rkt"
         "class.rkt"
         "inline.rkt"
         "id.rkt"
         "config.rkt")

(provide make-toc
         toc-class-name
         toc-node-class-name
         toc-edge-class-name
         toc-edge-details-class-name
         toc-edge-summary-class-name
         toc-edge-title-class-name)

(define toc-class-name (class-name "toc"))
(define toc-node-class-name (class-name "toc-node"))
(define toc-edge-class-name (class-name "toc-edge"))
(define toc-edge-details-class-name (class-name "toc-edge-details"))
(define toc-edge-summary-class-name (class-name "toc-edge-summary"))
(define toc-edge-title-class-name (class-name "toc-edge-title"))

(define ((make-toc [cfg : Config])
         [ss : (Listof Section)]) : XExprs
  (tagged% 'div
           `((class ,toc-class-name))
           ((make-toc:aux cfg) ss)))

(: make-toc:aux : (Config . -> . ((Listof Section) . -> . XExprs)))
(: make-toc:aux-1 : (Config . -> . (Section . -> . XExprs)))

(define ((make-toc:aux cfg) ss)
  (apply tagged% 'ul
         `((class ,toc-node-class-name))
         (map (make-toc:aux-1 cfg) ss)))

(define ((make-toc:aux-1 cfg) s)
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
                                      ((inline->xexprs cfg) title)))
                    ((make-toc:aux cfg) (section-subsections s)))))
