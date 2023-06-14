#lang at-exp typed/racket

(require "../data/id.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../text/id.rkt"
         "class.rkt")

(provide id->url
         id->xexprs/a
         id-class-name)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")
  (provide id-class-name
           id-css)

  (define id-class-name (class-name "id"))

  (define id-css
    @string%{
      .@|id-class-name| {
        color: gray;
        text-decoration-line: none;
      }
    }))

(require 'style)

(define (id->url [i : Id]) : String
  (format "~a.html" (id-contents i)))

(define (id->xexprs/a [i : Id]) : XExprs
  (tagged% 'a
            `((class ,id-class-name)
              (href ,(id->url i)))
            (string-tree->string (id->text i))))
