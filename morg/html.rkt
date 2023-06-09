#lang typed/racket

(require "html/site.rkt"
         "data/document.rkt")

(provide ->html)

(define (->html #:config [cfg : UserConfig default-config]
                [doc : Document]) : Site
  (make-site cfg doc))
