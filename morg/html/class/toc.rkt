#lang typed/racket

(require "../class.rkt")

(provide toc-class-name
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
