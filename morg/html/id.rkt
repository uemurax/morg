#lang typed/racket

(require "../data/id.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../text/id.rkt"
         "class.rkt"
         "config.rkt")

(provide id->url
         id->xexprs/a
         id-class-name)

(define (id->url [i : Id]) : String
  (id-contents i))

(define id-class-name (class-name "id"))

(define ((id->xexprs/a [_cfg : Config])
         [i : Id]) : XExprs
  (tagged% 'a
            `((class ,id-class-name)
              (href ,(id->url i)))
            (string-tree->string (id->text i))))