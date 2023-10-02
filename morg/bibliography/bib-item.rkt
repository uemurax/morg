#lang typed/racket

(require "../data/inline.rkt"
         "../data/date.rkt")

(provide BibItem
         EPrintType
         (struct-out eprint) EPrint
         (struct-out article) Article
         (struct-out thesis) Thesis
         (struct-out misc) Misc
         (struct-out inproceedings) InProceedings
         (struct-out inbook) InBook
         (struct-out book) Book)

(define-type BibItem
  (U Book
     InBook
     InProceedings
     Thesis
     Misc
     Article))

(define-type EPrintType
  (U 'arXiv))

(struct eprint
  ([type : EPrintType]
   [id : String])
  #:transparent
  #:type-name EPrint)

(struct book
  ([author : (Listof PureInline)]
   [title : PureInline]
   [date : Date]
   [publisher : (Option PureInline)]
   [location : (Option PureInline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Book)

(struct inbook
  ([author : (Listof PureInline)]
   [title : PureInline]
   [booktitle : PureInline]
   [date : Date]
   [editor : (Option (Listof PureInline))]
   [publisher : (Option PureInline)]
   [location : (Option PureInline)]
   [volume : (Option PureInline)]
   [pages : (Option PureInline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name InBook)

(struct article
  ([author : (Listof PureInline)]
   [title : PureInline]
   [journal-title : PureInline]
   [date : Date]
   [volume : PureInline]
   [number : (Option PureInline)]
   [pages : (Option PureInline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Article)

(struct inproceedings
  ([author : (Listof PureInline)]
   [title : PureInline]
   [book-title : PureInline]
   [date : Date]
   [editor : (Option (Listof PureInline))]
   [publisher : (Option PureInline)]
   [location : (Option PureInline)]
   [pages : (Option PureInline)]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name InProceedings)

(struct thesis
  ([author : (Listof PureInline)]
   [title : PureInline]
   [type : PureInline]
   [institution : PureInline]
   [date : Date]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Thesis)

(struct misc
  ([author : (Listof PureInline)]
   [title : PureInline]
   [date : Date]
   [doi : (Option String)]
   [url : (Option String)]
   [eprint : (Option EPrint)])
  #:transparent
  #:type-name Misc)
