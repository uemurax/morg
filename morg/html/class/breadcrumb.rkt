#lang typed/racket

(require "../class.rkt")

(provide breadcrumb-class-name
         breadcrumb-top-class-name
         breadcrumb-node-class-name)

(define breadcrumb-class-name (class-name "breadcrumb"))
(define breadcrumb-top-class-name (class-name "breadcumrb-top"))
(define breadcrumb-node-class-name (class-name "breadcrumb-node"))
