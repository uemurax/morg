#lang typed/racket

(require "../data/article.rkt"
         "../markup/xexpr.rkt"
         "../markup/splice.rkt"
         "../data/id.rkt"
         "class.rkt"
         "config.rkt"
         "inline.rkt"
         "block.rkt"
         "id.rkt")

(provide article->xexprs
         statement-class-name
         statement-header-class-name
         statement-header-header-class-name
         statement-header-title-class-name
         statement-body-class-name
         proof-class-name
         proof-header-class-name
         proof-body-class-name)

(define statement-class-name (class-name "statement"))
(define statement-header-class-name (class-name "statement-header"))
(define statement-header-header-class-name (class-name "statement-header-header"))
(define statement-header-title-class-name (class-name "statement-header-title"))
(define statement-body-class-name (class-name "statement-body"))

(define ((article->xexprs:statement [cfg : Config]) [a : Article]) : XExprs
  (define f inline->xexprs)
  (define title (article-title a))
  (tagged% 'div
           `((class ,statement-class-name))
           (tagged% 'header
                    `((class ,statement-header-class-name))
                    (id->xexprs/a (article-id a))
                    (tagged% 'span
                             `((class ,statement-header-header-class-name))
                             (f (article-header a)))
                    (when% title
                      (tagged% 'span
                               `((class ,statement-header-title-class-name))
                               (f title))))
           (tagged% 'div
                    `((class ,statement-body-class-name))
                    ((block->xexprs cfg) (article-contents a)))))

(define proof-class-name (class-name "proof"))
(define proof-header-class-name (class-name "proof-header"))
(define proof-body-class-name (class-name "proof-body"))

(define ((proof->xexprs [cfg : Config]) [pf : Proof]) : XExprs
  (tagged% 'div
           `((class ,proof-class-name))
           (tagged% 'header
                    `((class ,proof-header-class-name))
                    (inline->xexprs (proof-header pf)))
           (tagged% 'div
                    `((class ,proof-body-class-name))
                    ((block->xexprs cfg) (proof-contents pf)))))

(define article-class-name (class-name "article"))

(define ((article->xexprs [cfg : Config]) [a : Article]) : XExprs
  (define pf (article-proof a))
  (tagged% 'article
           `((class ,article-class-name)
             (id ,(id-contents (article-id a))))
           ((article->xexprs:statement cfg) a)
           (when% pf
             ((proof->xexprs cfg) pf))))
