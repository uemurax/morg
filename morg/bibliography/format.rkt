#lang at-exp typed/racket

(require "bib-item.rkt"
         "../data/inline.rkt"
         "../data/date.rkt"
         "../markup/inline.rkt"
         "../markup/splice.rkt"
         "../text/date.rkt"
         "../util/list.rkt")

(provide format-bib-item)

(define (format-bib-item [b : BibItem]) : Inline
  (cond
   [(book? b) (format-book b)]
   [(article? b) (format-article b)]
   [(thesis? b) (format-thesis b)]
   [(misc? b) (format-misc b)]
   [else (error "Unimplemented.")]))

(define (format-author [a : (Listof Inline)]) : Inline
  (apply inline% (list-join-1 a " & ")))

(define (format-url [s : String]) : Inline
  @inline%{@href%[s]})

(define (format-doi [s : String]) : Inline
  (format-url (format "https://doi.org/~a" s)))

(define (format-eprint [e : EPrint]) : Inline
  (define type (eprint-type e))
  (define i (eprint-id e))
  (case type
   [(arXiv) (format-url (format "https://arxiv.org/abs/~a" i))]
   [else (error "Unimplemented.")]))

(define (format-online #:doi [doi : (Option String) #f]
                       #:url [url : (Option String) #f]
                       #:eprint [ep : (Option EPrint) #f]) : Inline
  (define doi-1 : InlineLike
    @when%[doi]{ @(format-doi doi)})
  (define url-1 : InlineLike
    @when%[url]{ @(format-url url)})
  (define ep-1 : InlineLike
    @when%[ep]{ @(format-eprint ep)})
  @inline%{@|doi-1|@|url-1|@|ep-1|})

(define (format-book [b : Book]) : Inline
  (define author (format-author (book-author b)))
  (define title (book-title b))
  (define publisher-1 (book-publisher b))
  (define publisher : InlineLike
    @when%[publisher-1]{@|publisher-1|, })
  (define address-1 (book-address b))
  (define address : InlineLike
    @when%[address-1]{@|address-1|, })
  (define date (date->text (book-date b)))
  (define online
    (format-online #:doi (book-doi b)
                   #:url (book-url b)
                   #:eprint (book-eprint b)))
  @inline%{@|author|. @emph%{@|title|}. @|publisher|@|address|@|date|.@|online|})

(define (format-article [a : Article]) : Inline
  (define author (format-author (article-author a)))
  (define title (article-title a))
  (define journal-title (article-journal-title a))
  (define volume (article-volume a))
  (define number-1 (article-number a))
  (define number : InlineLike
    @when%[number-1]{(@|number-1|)})
  (define pages-1 (article-pages a))
  (define pages : InlineLike
    @when%[pages-1]{:@|pages-1|})
  (define date (date->text (article-date a)))
  (define online
    (format-online #:doi (article-doi a)
                   #:url (article-url a)
                   #:eprint (article-eprint a)))
  @inline%{@|author|. @|title|. @emph%{@|journal-title|}, @|volume|@|number|@|pages|, @|date|.@|online|})

(define (format-thesis [t : Thesis]) : Inline
  (define author (format-author (thesis-author t)))
  (define title (thesis-title t))
  (define type (thesis-type t))
  (define inst (thesis-institution t))
  (define date (date->text (thesis-date t)))
  (define online
    (format-online #:doi (thesis-doi t)
                   #:url (thesis-url t)
                   #:eprint (thesis-eprint t)))
  @inline%{@|author|. @emph{@|title|}. @|type|, @|inst|, @|date|.@|online|})

(define (format-misc [m : Misc]) : Inline
  (define author (format-author (misc-author m)))
  (define title (misc-title m))
  (define date (date->text (misc-date m)))
  (define online
    (format-online #:doi (misc-doi m)
                   #:url (misc-url m)
                   #:eprint (misc-eprint m)))
  @inline%{@|author|. @emph{@|title|}. @|date|.@|online|})
