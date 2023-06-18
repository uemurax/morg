#lang typed/racket

(require "inline.rkt"
         "splice.rkt"
         "block.rkt"
         "article.rkt"
         "section.rkt"
         "document.rkt"
         "id.rkt")

(provide (struct-out anchor-key) AnchorKey
         AnchorTable
         anchor-table-has-key?
         anchor-table-ref
         empty-anchor-table
         make-anchor-table)

(struct anchor-key
  ([node : Id]
   [anchor : Id])
  #:transparent
  #:type-name AnchorKey)

(define-type (AnchorTable-1 PureInline)
  (HashTable AnchorKey (Anchor PureInline)))

(define-type (Cons PureInline X)
  (X (AnchorTable-1 PureInline) . -> . (AnchorTable-1 PureInline)))

(: make-anchor-table:inline : (Id . -> . (Cons PureInline Inline)))

(define #:forall (PureInline Inline)
        (make-anchor-table:list-item
         [n : Id] [f : (Cons PureInline Inline)])
        : (Cons PureInline (ListItem Inline))
  (lambda (li tbl)
    (define tbl-1 (f (list-item-head li) tbl))
    (f (list-item-contents li) tbl-1)))

(define #:forall (PureInline Inline)
        (make-anchor-table:pure-inline-element
         [n : Id] [f : (Cons PureInline Inline)])
        : (Cons PureInline (PureInlineElement Inline))
  (lambda (pi tbl)
    (cond
     [(unordered-list? pi)
      (foldl (make-anchor-table:list-item n f) tbl
             (unordered-list-contents pi))]
     [(ordered-list? pi)
      (foldl (make-anchor-table:list-item n f) tbl
             (ordered-list-contents pi))]
     [(href? pi)
      (define c (href-contents pi))
      (if c (f c tbl) tbl)]
     [(emph? pi) (f (emph-contents pi) tbl)]
     [(display? pi) (f (display-contents pi) tbl)]
     [(code? pi) (f (code-contents pi) tbl)]
     [(dfn? pi) (f (dfn-contents pi) tbl)]
     [else tbl])))

(define #:forall (PureInline Inline)
        (make-anchor-table:inline-element [n : Id] [f : (Cons PureInline Inline)])
        : (Cons PureInline (InlineElement PureInline Inline))
  (lambda (x tbl)
    (cond
     [(ref? x) tbl]
     [(anchor? x)
      (define key (anchor-key n (anchor-id x)))
      (if (hash-has-key? tbl key)
          (error (format "Duplicated anchor: ~a" key))
          (hash-set tbl key x))]
     [(anchor-ref? x) tbl]
     [else ((make-anchor-table:pure-inline-element n f) x tbl)])))

(define ((make-anchor-table:inline n) il tbl)
  (define x (inline-contents il))
  (define f (make-anchor-table:inline n))
  (cond
   [(splice? x) (foldl f tbl (splice-contents x))]
   [else ((make-anchor-table:inline-element n f) x tbl)]))

(define (make-anchor-table:block [n : Id]) : (Cons PureInline Block)
  (lambda (b tbl)
    (define x (block-contents b))
    (define f (make-anchor-table:block n))
    (cond
     [(splice? x) (foldl f tbl (splice-contents x))]
     [(paragraph? x)
      ((make-anchor-table:inline n) (paragraph-contents x) tbl)]
     [else tbl])))

(define (make-anchor-table:proof [n : Id]) : (Cons PureInline Proof)
  (lambda (pf tbl)
    ((make-anchor-table:block n) (proof-contents pf) tbl)))

(define make-anchor-table:article : (Cons PureInline Article)
  (lambda (a tbl)
    (define n (article-id a))
    (define f (make-anchor-table:block n))
    (define tbl-1 (f (article-contents a) tbl))
    (define pf (article-proof a))
    (if pf ((make-anchor-table:proof n) pf tbl-1) tbl-1)))

(define (make-anchor-table:section-element [n : Id])
        : (Cons PureInline SectionElement)
  (lambda (s tbl)
    (cond
     [(article? s) (make-anchor-table:article s tbl)]
     [(block? s) ((make-anchor-table:block n) s tbl)])))

(define make-anchor-table:section : (Cons PureInline Section)
  (lambda (s tbl)
    (define n (section-id s))
    (define tbl-1
      (foldl (make-anchor-table:section-element n)
             tbl
             (section-contents s)))
    (foldl make-anchor-table:section
           tbl-1
           (section-subsections s))))

(define make-anchor-table:document : (Cons PureInline Document)
  (lambda (d tbl)
    (define n (document-id d))
    (define tbl-1
      ((make-anchor-table:block n) (document-contents d) tbl))
    (foldl make-anchor-table:section
           tbl-1
           (append (document-front d)
                   (document-main d)
                   (document-back d)))))

(struct anchor-table
  ([contents : (AnchorTable-1 PureInline)])
  #:type-name AnchorTable)

(define (anchor-table-has-key? [tbl : AnchorTable] [key : AnchorKey])
  (hash-has-key? (anchor-table-contents tbl) key))

(define (anchor-table-ref [tbl : AnchorTable] [key : AnchorKey])
  (hash-ref (anchor-table-contents tbl) key))

(define empty-anchor-table
  (anchor-table (hash)))

(define (make-anchor-table [d : Document]) : AnchorTable
  (define tbl
    (make-anchor-table:document d (hash)))
  (anchor-table tbl))
