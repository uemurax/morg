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
  (define doi-1 (book-doi b))
  (define doi : InlineLike
    @when%[doi-1]{ @(format-doi doi-1)})
  (define url-1 (book-url b))
  (define url : InlineLike
    @when%[url-1]{ @(format-url url-1)})
  (define eprint-1 (book-eprint b))
  (define eprint : InlineLike
    @when%[eprint-1]{ @(format-eprint eprint-1)})
  @inline%{@|author|. @emph%{@|title|}. @|publisher|@|address|@|date|.@|doi|@|url|@|eprint|})

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
  (define doi-1 (article-doi a))
  (define doi : InlineLike
    @when%[doi-1]{ @(format-doi doi-1)})
  (define url-1 (article-url a))
  (define url : InlineLike
    @when%[url-1]{ @(format-url url-1)})
  (define eprint-1 (article-eprint a))
  (define eprint : InlineLike
    @when%[eprint-1]{ @(format-eprint eprint-1)})
  @inline%{@|author|. @|title|. @emph%{@|journal-title|}, @|volume|@|number|@|pages|, @|date|.@|doi|@|url|@|eprint|})
