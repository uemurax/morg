#lang at-exp typed/racket

(require "../data/node.rkt"
         "../data/document.rkt"
         "../data/id.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         (submod "inline.rkt" style)
         (submod "id.rkt" style)
         (submod "toc.rkt" style)
         (submod "article.rkt" style)
         (submod "section.rkt" style)
         (submod "block.rkt" style)
         (submod "document.rkt" style))

(provide Assets
         (struct-out site-state) SiteState
         (struct-out config) Config
         site-state-node-ref
         default-config)

(define-type Assets
  (HashTable String StringTreeLike))

(struct site-state
  ([front : NodeTable]
   [main : NodeTable]
   [back : NodeTable]
   [root : Document])
  #:transparent
  #:type-name SiteState)

(struct config
  ([body-template : (SiteState (U Node Document) . -> . (XExprs . -> . XExprs))]
   [head-template : (SiteState (U Node Document) . -> . (XExprs . -> . XExprs))]
   [assets : Assets])
  #:transparent
  #:type-name Config)

(define (site-state-node-ref [st : SiteState] [id : Id])
        : (U Node Document)
  (define front (site-state-front st))
  (define main (site-state-main st))
  (define back (site-state-back st))
  (define doc (site-state-root st))
  (cond
   [(node-table-has-key? front id) (node-table-ref front id)]
   [(node-table-has-key? main id) (node-table-ref main id)]
   [(node-table-has-key? back id) (node-table-ref back id)]
   [else doc]))

(define default-config:css-name "default.css")

(define ((default-config:body-template [_st : SiteState] [_n : (U Node Document)])
         [x : XExprs]) : XExprs
  x)

(define ((default-config:head-template [_st : SiteState] [_n : (U Node Document)])
         [x : XExprs]) : XExprs
  (xexprs%
   x
   (tagged% 'link
            `((rel "stylesheet")
              (href ,default-config:css-name)))))

(define default-config:css
  @string%{
    body {
      font-family: sans-serif;
      font-size: 112.5%;
    }
    main {
      max-width: 800px;
      margin: 40px auto;
    }
    @|id-css|
    @|inline-css|
    @|block-css|
    @|toc-css|
    @|article-css|
    @|section-css|
    @|document-css|
  })

(define default-config:assets
  (hash default-config:css-name default-config:css))

(define default-config
  (config
   default-config:body-template
   default-config:head-template
   default-config:assets))
