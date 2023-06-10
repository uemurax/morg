#lang typed/racket

(require "../data/inline.rkt")

(provide (struct-out eprint) EPrint
         (struct-out book) Book)

(struct eprint
  ([type : (U 'arXIv)]
   [id : String])
  #:transparent
  #:type-name EPrint)

(struct book
  ([author : (Listof Inline)]
   [title : Inline]
   [date : date]
   [publisher : (Option Inline)]
   [address : (Option Inline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Book)
