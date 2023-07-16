#lang typed/racket

(require "site.rkt"
         "config.rkt"
         "../data/document.rkt")

(provide ->html)

(define (->html #:config [cfg : Config default-config]
                [doc : Document]) : Site
  (make-site cfg doc))
