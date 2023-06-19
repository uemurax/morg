#lang at-exp typed/racket

(require "../data/id.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../text/id.rkt"
         "class/id.rkt")

(provide id->url
         anchor-id->css-id
         id->xexprs/a)

(define (id->url [i : Id]) : String
  (format "~a.html" (id-contents i)))

(define (id->xexprs/a [i : Id]) : XExprs
  (tagged% 'a
            `((class ,id-class-name)
              (href ,(id->url i)))
            (string-tree->string (id->text i))))

(define (anchor-id->css-id [node : Id] [anchor : Id])
  (string-tree->string
   @string%{a井@(id-contents node)井@(id-contents anchor)}))
