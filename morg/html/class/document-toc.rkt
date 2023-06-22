#lang typed/racket

(require "../class.rkt")

(provide document-front-toc-class-name
         document-main-toc-class-name
         document-back-toc-class-name
         document-toc-class-name)

(define document-front-toc-class-name (class-name "document-front-toc"))
(define document-main-toc-class-name (class-name "document-main-toc"))
(define document-back-toc-class-name (class-name "document-back-toc"))
(define document-toc-class-name (class-name "document-toc"))
