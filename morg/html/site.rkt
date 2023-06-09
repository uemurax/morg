#lang at-exp typed/racket

(require (prefix-in xml: typed/xml)
         "../data/document.rkt"
         "../data/node.rkt"
         "../data/id.rkt"
         "../data/section.rkt"
         "../data/article.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../markup/inline.rkt"
         "../text/id.rkt"
         "inline.rkt"
         "id.rkt"
         "document.rkt"
         "xexpr-table.rkt")

(require/typed racket/hash
  [hash-union ((HashTable String String) (HashTable String String) . -> . (HashTable String String))])

(provide (struct-out config) Config
         (struct-out user-config) UserConfig
         Assets
         Site
         make-site
         default-config)

(define-type Assets
  (HashTable String StringTree))

(struct config
  ([user-config : UserConfig]
   [root : Document])
  #:transparent
  #:type-name Config)

(struct user-config
  ([body-template : (Config (U Node Document) . -> . (XExprs . -> . XExprs))]
   [head-template : (Config (U Node Document) . -> . (XExprs . -> . XExprs))]
   [assets : Assets])
  #:transparent
  #:type-name UserConfig)

(define-type Site
  (HashTable String String))

(struct node-tables
  ([front : NodeTable]
   [main : NodeTable]
   [back : NodeTable])
  #:transparent
  #:type-name NodeTables)

(define (node-tables-ref [doc : Document] [tbls : NodeTables] [id : Id])
        : (U Node Document)
  (match tbls
   [(node-tables front main back)
    (cond
     [(node-table-has-key? front id) (node-table-ref front id)]
     [(node-table-has-key? main id) (node-table-ref main id)]
     [(node-table-has-key? back id) (node-table-ref back id)]
     [else doc])]))

(define ((apply-template [cfg : Config] [tbls : NodeTables])
         [i : Id] [x : XExprs]) : (Values String String)
  (define usr-cfg (config-user-config cfg))
  (define n (node-tables-ref (config-root cfg) tbls i))
  (define head
    (((user-config-head-template usr-cfg) cfg n) x))
  (define body
    (((user-config-body-template usr-cfg) cfg n)
     (tagged% 'main '() x)))
  (define html
    (car 
     (tagged% 'html '()
              (tagged% 'head '() head)
              (tagged% 'body '() body))))
  (values (id->url i)
          (parameterize ([xml:current-unescaped-tags xml:html-unescaped-tags]
                         [xml:empty-tag-shorthand xml:html-empty-tags])
            (string-append
             "<!DOCTYPE html>\n"
             (xml:xexpr->string html)))))

(define (make-site [usr-cfg : UserConfig] [doc : Document]) : Site
  (define pages
    (document->xexprs doc))
  (define tbls
    (node-tables
     (make-node-table (document-front doc))
     (make-node-table (document-main doc))
     (make-node-table (document-back doc))))
  (define cfg
    (config usr-cfg doc))
  (define site
    (hash-map/copy pages (apply-template cfg tbls)))
  (define assets
    (hash-map/copy (user-config-assets usr-cfg)
      (lambda ([k : String] [x : StringTree])
        (values k (string-tree->string x)))))
  (hash-union site assets))

(define default-config:css-name "default.css")

(define ((default-config:body-template [_cfg : Config] [_n : (U Node Document)])
         [x : XExprs]) : XExprs
  x)

(define ((default-config:head-template [cfg : Config] [n : (U Node Document)])
         [x : XExprs]) : XExprs
  (define doc (config-root cfg))
  (define doc-title (document-title doc))
  (define title
    (cond
     [(document? n) doc-title]
     [(section-node? n)
      @inline%{@(section-title (section-node-contents n)) -- @|doc-title|}]
     [(article-node? n)
      (define a (article-node-contents n))
      (define t (article-title a))
      (define h
        (if t t (id->text (article-id a))))
      @inline%{@|h| -- @|doc-title|}]))
  (xexprs%
   (tagged% 'title '()
            (inline->xexprs title))
   (tagged% 'link
            `((rel "stylesheet")
              (href ,default-config:css-name)))
   x))

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
  })

(define default-config:assets
  (hash default-config:css-name default-config:css))

(define default-config
  (user-config
   default-config:body-template
   default-config:head-template
   default-config:assets))
