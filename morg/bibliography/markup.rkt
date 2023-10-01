#lang typed/racket

(require "bib-item.rkt"
         "../markup/inline.rkt"
         "../data/date.rkt"
         "../util/option.rkt")

(provide eprint%
         article%
         thesis%
         misc%
         arXiv%
         inbook%
         book%)

(define (eprint% #:type [type : EPrintType 'arXiv]
                 [id : String]) : EPrint
  (eprint type id))

(define (book% #:author [author : (Listof PureInlineLike)]
               #:title [title : PureInlineLike]
               #:date [d : Date]
               #:publisher [publisher : (Option PureInlineLike) #f]
               #:location [location : (Option PureInlineLike) #f]
               #:doi [doi : (Option String) #f]
               #:url [url : (Option String) #f]
               #:eprint [ep : (Option EPrint) #f]) : Book
  (book (map pure-inline% author)
        (pure-inline% title)
        d
        (option-map pure-inline% publisher)
        (option-map pure-inline% location)
        doi url ep))

(define (inbook% #:author [author : (Listof PureInlineLike)]
                 #:title [title : PureInlineLike]
                 #:booktitle [booktitle : PureInlineLike]
                 #:date [d : Date]
                 #:editor [editor : (Option (Listof PureInlineLike)) #f]
                 #:publisher [publisher : (Option PureInlineLike) #f]
                 #:location [location : (Option PureInlineLike) #f]
                 #:volume [volume : (Option PureInlineLike) #f]
                 #:pages [pages : (Option PureInlineLike) #f]
                 #:doi [doi : (Option String) #f]
                 #:url [url : (Option String) #f]
                 #:eprint [ep : (Option EPrint) #f]) : InBook
  (inbook (map pure-inline% author)
          (pure-inline% title)
          (pure-inline% booktitle)
          d
          (option-map (lambda ([a : (Listof PureInlineLike)])
                        (map pure-inline% a))
                      editor)
          (option-map pure-inline% publisher)
          (option-map pure-inline% location)
          (option-map pure-inline% volume)
          (option-map pure-inline% pages)
          doi url ep))

(define (article% #:author [author : (Listof PureInlineLike)]
                  #:title [title : PureInlineLike]
                  #:journal-title [journal-title : PureInlineLike]
                  #:date [d : Date]
                  #:volume [volume : PureInlineLike]
                  #:number [number : (Option PureInlineLike) #f]
                  #:pages [pages : (Option PureInlineLike) #f]
                  #:doi [doi : (Option String) #f]
                  #:url [url : (Option String) #f]
                  #:eprint [ep : (Option EPrint) #f]) : Article
  (article (map pure-inline% author)
           (pure-inline% title)
           (pure-inline% journal-title)
           d
           (pure-inline% volume)
           (option-map pure-inline% number)
           (option-map pure-inline% pages)
           doi url ep))

(define (thesis% #:author [author : (Listof PureInlineLike)]
                 #:title [title : PureInlineLike]
                 #:type [type : PureInlineLike "PhD Thesis"]
                 #:institution [institution : PureInlineLike]
                 #:date [d : Date]
                 #:doi [doi : (Option String) #f]
                 #:url [url : (Option String) #f]
                 #:eprint [ep : (Option EPrint) #f]) : Thesis
  (thesis (map pure-inline% author)
          (pure-inline% title)
          (pure-inline% type)
          (pure-inline% institution)
          d
          doi url ep))

(define (misc% #:author [author : (Listof PureInlineLike)]
               #:title [title : PureInlineLike]
               #:date [d : Date]
               #:doi [doi : (Option String) #f]
               #:url [url : (Option String) #f]
               #:eprint [ep : (Option EPrint) #f]) : Misc
  (misc (map pure-inline% author)
        (pure-inline% title)
        d
        doi url ep))

(define (arXiv% #:author [author : (Listof PureInlineLike)]
                #:title [title : PureInlineLike]
                #:date [d : Date]
                #:id [id : String])
  (misc% #:author author
         #:title title
         #:date d
         #:eprint (eprint% #:type 'arXiv id)))
