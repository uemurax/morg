#lang typed/racket

(require "index.rkt"
         "article.rkt"
         "section.rkt"
         "document.rkt")

(provide (struct-out index-item) IndexItem
         IndexList
         make-index-list
         index-list-sort)

(struct index-item
  ([index : Index]
   [article : Article])
  #:transparent
  #:type-name IndexItem)

(define-type IndexList
  (Listof IndexItem))

(define (make-index-list [doc : Document]) : IndexList
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (apply append
         (map make-index-list:section
              (append front main back))))

(define (make-index-list:section [sec : Section]) : IndexList
  (define articles (section-articles sec))
  (apply append (map make-index-list:article articles)))

(define (make-index-list:article [art : Article]) : IndexList
  (map (lambda ([i : Index])
         (index-item i art))
       (article-indexes art)))

(define (index-item<? [i1 : IndexItem] [i2 : IndexItem]) : Boolean
  ((index-item-index i1) . index<? . (index-item-index i2)))

(define (index-list-sort 
         [il : IndexList]
         #:less-than? [less-than? : (IndexItem IndexItem . -> . Boolean)
                       index-item<?]) : IndexList
  (sort il less-than?))
