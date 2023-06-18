#lang at-exp typed/racket

(require "../data/id.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../text/id.rkt"
         "class.rkt")

(provide id->url
         anchor-id->css-id
         id->xexprs/a)

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

(define (anchor-id->css-id [node : Id] [anchor : Id])
  (string-tree->string
   @string%{a_@(id-contents node)_@(id-contents anchor)}))
