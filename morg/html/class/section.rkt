#lang typed/racket

(require "../class.rkt")

(provide section-class-name
         section-title-class-name
         section-body-class-name
         section-toc-class-name)

(define section-class-name (class-name "section"))
(define section-title-class-name (class-name "section-title"))
(define section-body-class-name (class-name "section-body"))
(define section-toc-class-name (class-name "section-toc"))
