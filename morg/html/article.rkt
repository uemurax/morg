#lang at-exp typed/racket

(require "../data/article.rkt"
         "../markup/xexpr.rkt"
         "../markup/splice.rkt"
         "../data/id.rkt"
         "class/article.rkt"
         "state.rkt"
         "inline.rkt"
         "block.rkt"
         "id.rkt")

(provide article->xexprs)

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
