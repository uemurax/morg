#lang typed/racket

(require "bib-item.rkt"
         "../markup/inline.rkt"
         "../data/date.rkt"
         "../util/option.rkt")

(provide eprint%
         book%)

(define (eprint% #:type [type : EPrintType 'arXiv]
                 [id : String]) : EPrint
  (eprint type id))

(define (book% #:author [author : (Listof InlineLike)]
               #:title [title : InlineLike]
               #:date [d : Date]
               #:publisher [publisher : (Option InlineLike) #f]
               #:address [address : (Option InlineLike) #f]
               #:doi [doi : (Option String) #f]
               #:url [url : (Option String) #f]
               #:eprint [ep : (Option EPrint) #f]) : Book
  (book (map inline% author)
        (inline% title)
        d
        (option-map inline% publisher)
        (option-map inline% address)
        doi url ep))
