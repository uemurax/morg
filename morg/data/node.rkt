#lang at-exp typed/racket

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

(module+ test
  (require typed/rackunit))

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
    (error (format "Duplicated id:\none: ~a\nanother: ~a"
                   (map node-id (node-trace (node-table-ref tbl id)))
                   (map node-id (node-trace n)))))
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

(module+ test
  (require "../markup/article.rkt"
           "../markup/section.rkt"
           "../markup/block.rkt"
           "../markup/splice.rkt")

  (define a1
    @article~[#:id "a1" #:header @~{Definition}])
  (define a2
    @article~[#:id "a2" #:header @~{Proposition}])
  (define s1
    @section~[
      #:id "s1" #:title @~{Title 1}
      a1
      @paragraph~{
        Hello, world!
      }
      a2
    ])
  (define s2
    @section~[
      #:id "s2" #:title @~{Title 2}
      @paragraph~{
        By, world!
      }
    ])
  (define a3
    @article~[#:id "a3" #:header @~{Theorem}])
  (define s5
    @section~[
      #:id "s5" #:title @~{Title 5}
    ])
  (define s3
    @section~[
      #:id "s3" #:title @~{Title 3}
      a3
      #:subsections @list[
        s5
      ]
    ])
  (define s4
    @section~[
      #:id "s4" #:title @~{Title 4}
      #:subsections @list[
        s1 s2 s3
      ]
    ])
  (define tbl
    (make-node-table (list s4)))
  (define ns4
    (section-node s4 (root (list s4)) 0 (list) (list)))
  (define ns1
    (section-node s1 ns4 0 (list) (list s2 s3)))
  (define ns2
    (section-node s2 ns4 1 (list s1) (list s3)))
  (define ns3
    (section-node s3 ns4 2 (list s2 s1) (list)))
  (define na1
    (article-node a1 ns1 0))
  (define na2
    (article-node a2 ns1 1))
  (define na3
    (article-node a3 ns3 0))
  (define ns5
    (section-node s5 ns3 0 (list) (list)))

  (check-equal?
   (node-table-contents tbl)
   (hash (id "a1") na1
         (id "a2") na2
         (id "a3") na3
         (id "s1") ns1
         (id "s2") ns2
         (id "s5") ns5
         (id "s3") ns3
         (id "s4") ns4))
  
  (check-equal?
   (node-trace ns5)
   (list ns4 ns3 ns5))
  (check-equal?
   (node-trace na2)
   (list ns4 ns1 na2)))
