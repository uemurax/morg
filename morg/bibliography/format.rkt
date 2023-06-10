#lang at-exp typed/racket

(require "bib-item.rkt"
         "../data/inline.rkt"
         "../markup/inline.rkt"
         "../markup/splice.rkt"
         "../util/list.rkt")

(provide format-bib-item)

(define (format-bib-item [b : BibItem]) : Inline
  (cond
   [(book? b) (format-book b)]
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
  (define year (number->string (date-year (book-date b))))
  (define doi-1 (book-doi b))
  (define doi : InlineLike
    @when%[doi-1]{ @(format-doi doi-1)})
  (define url-1 (book-url b))
  (define url : InlineLike
    @when%[url-1]{ @(format-url url-1)})
  (define eprint-1 (book-eprint b))
  (define eprint : InlineLike
    @when%[eprint-1]{ @(format-eprint eprint-1)})
  @inline%{@|author|. @|title|. @|publisher|@|address|@|year|.@|doi|@|url|@|eprint|})
