#lang typed/racket

(require "index.rkt"
         "article.rkt"
         "section.rkt"
         "document.rkt")

(provide (struct-out index-item) IndexItem
         IndexList
         IndexTable
         index-table-has-key?
         index-table-ref
         empty-index-table
         make-index-table
         index-item<?
         index-list-sort)

(struct index-item
  ([index : Index]
   [article : Article])
  #:transparent
  #:type-name IndexItem)

(define-type IndexList
  (Listof IndexItem))

(struct index-table
  ([contents : (HashTable Symbol IndexList)])
  #:type-name IndexTable)

(define (index-table-has-key? [tbl : IndexTable] [type : Symbol]) : Boolean
  (hash-has-key? (index-table-contents tbl) type))

(define (index-table-ref [tbl : IndexTable] [type : Symbol]) : IndexList
  (hash-ref (index-table-contents tbl) type))

(define empty-index-table
  (index-table (hash)))

(define (make-index-table [doc : Document]) : IndexTable
  (make-index-table:document doc (index-table (hash))))

(define (make-index-table:document [doc : Document] [tbl : IndexTable]) : IndexTable
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (foldl make-index-table:section tbl
         (append front main back)))

(define (make-index-table:section [sec : Section] [tbl : IndexTable]) : IndexTable
  (foldl make-index-table:article tbl (section-articles sec)))

(define (make-index-table:article [art : Article] [tbl : IndexTable]) : IndexTable
  (foldl (make-index-table:index art) tbl (article-indexes art)))

(define ((make-index-table:index [art : Article]) [i : Index] [tbl : IndexTable]) : IndexTable
  (define tbl-1 (index-table-contents tbl))
  (define type (index-type i))
  (define item (index-item i art))
  (define ls (hash-ref tbl-1 type (lambda () '())))
  (index-table (hash-set tbl-1 type (list* item ls))))

(define (index-item<? [i1 : IndexItem] [i2 : IndexItem]) : Boolean
  ((index-item-index i1) . index<? . (index-item-index i2)))

(define (index-list-sort 
         [il : IndexList]
         #:less-than? [less-than? : (IndexItem IndexItem . -> . Boolean)
                       index-item<?]) : IndexList
  (sort il less-than?))
