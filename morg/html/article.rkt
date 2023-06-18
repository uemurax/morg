#lang at-exp typed/racket

(require "../data/article.rkt"
         "../markup/xexpr.rkt"
         "../markup/splice.rkt"
         "../data/id.rkt"
         "class.rkt"
         "state.rkt"
         "inline.rkt"
         "block.rkt"
         "id.rkt")

(provide article->xexprs)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")
  (provide statement-class-name
           statement-header-class-name
           statement-header-header-class-name
           statement-header-title-class-name
           statement-body-class-name
           proof-class-name
           proof-header-class-name
           proof-body-class-name
           article-class-name
           article-css)

  (define statement-class-name (class-name "statement"))
  (define statement-header-class-name (class-name "statement-header"))
  (define statement-header-header-class-name (class-name "statement-header-header"))
  (define statement-header-title-class-name (class-name "statement-header-title"))
  (define statement-body-class-name (class-name "statement-body"))
  (define proof-class-name (class-name "proof"))
  (define proof-header-class-name (class-name "proof-header"))
  (define proof-body-class-name (class-name "proof-body"))
  (define article-class-name (class-name "article"))

  (define article-css
    @string%{
      .@|article-class-name|, .@|statement-class-name|, .@|proof-class-name| {
        margin-block: 1em;
      }
      .@|statement-header-header-class-name|, .@|proof-header-class-name| {
        font-weight: bold;
      }
    }))

(require 'style)

(define ((article->xexprs:statement [st : State]) [a : Article]) : XExprs
  (define f pure-inline->xexprs)
  (define title (article-title a))
  (tagged% 'div
           `((class ,statement-class-name))
           (tagged% 'header
                    `((class ,statement-header-class-name))
                    (id->xexprs/a (article-id a))
                    " "
                    (tagged% 'span
                             `((class ,statement-header-header-class-name))
                             (f (article-header a)))
                    (when% title
                      (tagged% 'span
                               `((class ,statement-header-title-class-name))
                               "("
                               (f title)
                               ")")))
           (tagged% 'div
                    `((class ,statement-body-class-name))
                    ((block->xexprs st) (article-contents a)))))

(define ((proof->xexprs [st : State]) [pf : Proof]) : XExprs
  (tagged% 'details
           `((class ,proof-class-name))
           (tagged% 'summary
                    `((class ,proof-header-class-name))
                    (pure-inline->xexprs (proof-header pf)))
           (tagged% 'div
                    `((class ,proof-body-class-name))
                    ((block->xexprs st) (proof-contents pf)))))

(define ((article->xexprs [st-1 : State]) [a : Article]) : XExprs
  (define id (article-id a))
  (define st
    (struct-copy state st-1
     [id id]))
  (define pf (article-proof a))
  (tagged% 'article
           `((class ,article-class-name))
           ((article->xexprs:statement st) a)
           (when% pf
             ((proof->xexprs st) pf))))
