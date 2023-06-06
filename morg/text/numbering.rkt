#lang typed/racket

(require "../data/node.rkt"
         "../markup/string.rkt"
         "../util/list.rkt")

(provide section-node-format-index
         article-node-format-index)

(define (section-node-format-index [n : SectionNode]) : StringTree
  (define is (map node-index (node-trace n)))
  (define ns (map number-format is))
  (define xs (list-join-1 ns "."))
  (apply string~ xs))

(define (article-node-format-index [n : ArticleNode]) : StringTree
  (define i (article-node-index n))
  (define p (article-node-parent n))
  (string~ (section-node-format-index p)
           "*"
           (number-format i)))

(define (number-format [n : Natural]) : String
  (number->string (+ n 1)))
