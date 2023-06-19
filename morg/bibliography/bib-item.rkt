#lang typed/racket

(require "../data/inline.rkt"
         "../data/date.rkt")

(provide BibItem
         EPrintType
         (struct-out eprint) EPrint
         (struct-out article) Article
         (struct-out thesis) Thesis
         (struct-out book) Book)

(define-type BibItem
  (U Book
     Article))

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

(struct article
  ([author : (Listof Inline)]
   [title : Inline]
   [journal-title : Inline]
   [date : Date]
   [volume : Inline]
   [number : (Option Inline)]
   [pages : (Option Inline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Article)

(struct thesis
  ([author : (Listof Inline)]
   [title : Inline]
   [type : Inline]
   [institution : Inline]
   [date : Date]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Thesis)
