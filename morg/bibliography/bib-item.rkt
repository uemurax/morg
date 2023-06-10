#lang typed/racket

(require "../data/inline.rkt"
         "../data/date.rkt")

(provide BibItem
         EPrintType
         (struct-out eprint) EPrint
         (struct-out book) Book)

(define-type BibItem
  (U Book))

(define-type EPrintType
  (U 'arXiv))

(struct eprint
  ([type : EPrintType]
   [id : String])
  #:transparent
  #:type-name EPrint)

(struct book
  ([author : (Listof Inline)]
   [title : Inline]
   [date : Date]
   [publisher : (Option Inline)]
   [address : (Option Inline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Book)
