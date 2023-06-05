#lang typed/racket

(require "article.rkt"
         "section.rkt"
         "id.rkt")

(provide (except-out (struct-out root) root) Root
         (except-out (struct-out section-node) section-node) SectionNode
         (except-out (struct-out article-node) article-node) ArticleNode
         Node
         node-id node-index node-parent
         node-trace
         NodeTable
         node-table-has-key? node-table-ref
         make-node-table)

(struct root
  ([contents : (Listof Section)])
  #:transparent
  #:type-name Root)

(struct section-node
  ([contents : Section]
   [parent : (U SectionNode Root)]
   [index : Natural]
   [siblings-left : (Listof Section)]
   [siblings-right : (Listof Section)])
  #:transparent
  #:type-name SectionNode)

(struct article-node
  ([contents : Article]
   [parent : SectionNode]
   [index : Natural])
  #:transparent
  #:type-name ArticleNode)

(define-type Node
  (U SectionNode ArticleNode))

(define (node-id [n : Node]) : Id
  (cond
   [(section-node? n) (section-id (section-node-contents n))]
   [(article-node? n) (article-id (article-node-contents n))]))

(define (node-parent [n : Node]) : (U SectionNode Root)
  (cond
   [(section-node? n) (section-node-parent n)]
   [(article-node? n) (article-node-parent n)]))

(define (node-index [n : Node]) : Natural
  (cond
   [(section-node? n) (section-node-index n)]
   [(article-node? n) (article-node-index n)]))

(define (node-trace [n : Node]) : (Listof Node)
  (node-trace:aux n (list)))

(define (node-trace:aux [n : (U Node Root)] [acc : (Listof Node)]) : (Listof Node)
  (cond
   [(root? n) acc]
   [else (node-trace:aux (node-parent n) (list* n acc))]))

(struct node-table
  ([contents : (HashTable Id Node)])
  #:type-name NodeTable)

(define (node-table-has-key? [tbl : NodeTable] [i : Id]) : Boolean
  (hash-has-key? (node-table-contents tbl) i))

(define (node-table-ref [tbl : NodeTable] [i : Id]) : Node
  (hash-ref (node-table-contents tbl) i))

(define (node-table-set [tbl : NodeTable] [i : Id] [n : Node]) : NodeTable
  (node-table (hash-set (node-table-contents tbl) i n)))

(define (check-unique [tbl : NodeTable] [id : Id] [n : Node]) : Void
  (when (node-table-has-key? tbl id)
    (error (format "Duplicated id:\none: ~a\nanother: ~a")
           (map node-id (node-trace (node-table-ref tbl id)))
           (map node-id (node-trace n))))
  (void))

(define (node-table-traverse-article [tbl : NodeTable] [a : Article] [s : SectionNode] [i : Natural]) : NodeTable
  (define n (article-node a s i))
  (define id (article-id a))
  (check-unique tbl id n)
  (node-table-set tbl id n))

(define (node-table-traverse-articles [tbl : NodeTable] [as : (Listof Article)] [s : SectionNode]) : NodeTable
  (node-table-traverse-articles:aux tbl as s 0))

(define (node-table-traverse-articles:aux
         [tbl : NodeTable] [as : (Listof Article)]
         [s : SectionNode] [i : Natural]) : NodeTable
  (match as
   [(list* a as)
    (node-table-traverse-articles:aux
     (node-table-traverse-article tbl a s i)
     as s (+ i 1))]
   [_ tbl]))

(define (node-table-traverse-section
         [tbl : NodeTable] [s : Section] [p : (U SectionNode Root)]
         [i : Natural] [left : (Listof Section)] [right : (Listof Section)]) : NodeTable
  (define n (section-node s p i left right))
  (define id (section-id s))
  (check-unique tbl id n)
  (define tbl-1 (node-table-set tbl id n))
  (define tbl-2
    (node-table-traverse-articles tbl-1 (section-articles s) n))
  (node-table-traverse-sections tbl-2 (section-subsections s) n))

(define (node-table-traverse-sections [tbl : NodeTable] [ss : (Listof Section)] [p : (U SectionNode Root)]) : NodeTable
  (node-table-traverse-sections:aux tbl (list) ss p 0))

(define (node-table-traverse-sections:aux
         [tbl : NodeTable] [left : (Listof Section)] [right : (Listof Section)]
         [p : (U SectionNode Root)] [i : Natural]) : NodeTable
  (match right
   [(list* s right)
    (node-table-traverse-sections:aux
     (node-table-traverse-section tbl s p i left right)
     (list* s left) right p (+ i 1))]
   [_ tbl]))

(define (node-table-traverse-root [tbl : NodeTable] [r : Root]) : NodeTable
  (node-table-traverse-sections tbl (root-contents r) r))

(define (make-node-table [ss : (Listof Section)]) : NodeTable
  (node-table-traverse-root (node-table (hash)) (root ss)))
