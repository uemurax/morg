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
         "../util/escape.rkt"
         "inline.rkt"
         "id.rkt"
         "document.rkt"
         (submod "inline.rkt" style)
         (submod "id.rkt" style)
         (submod "toc.rkt" style)
         (submod "article.rkt" style)
         (submod "section.rkt" style)
         (submod "block.rkt" style)
         (submod "document.rkt" style)
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
    (((user-config-head-template usr-cfg) cfg n) 
     (head-init cfg n)))
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

(define (js-escape [x : String])
  (escape (hash "\\" "\\\\") x))

(define (head-init [cfg : Config] [n : (U Node Document)]) : XExprs
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
   (tagged% 'meta '((charset "UTF-8")))
   (tagged% 'title '()
            (inline->xexprs title))
   (tagged% 'link
            '((rel "stylesheet")
              (href "https://cdn.jsdelivr.net/npm/katex@0.16.7/dist/katex.min.css")
              (integrity "sha384-3UiQGuEI4TTMaFmGIZumfRPtfKQ3trwQE2JgosJxCnGmQpL/lJdjpcHkaaFwHlcI")
              (crossorigin "anonymous")))
   (tagged% 'script
            '((defer "true")
              (src "https://cdn.jsdelivr.net/npm/katex@0.16.7/dist/katex.min.js")
              (integrity "sha384-G0zcxDFp5LWZtDuRMnBkk3EphCK1lhEf4UEyEM693ka574TZGwo4IWwS6QLzM/2t")
              (crossorigin "anonymous")))
   (tagged% 'script
            '((defer "true")
              (src "https://cdn.jsdelivr.net/npm/katex@0.16.7/dist/contrib/auto-render.min.js")
              (integrity "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05")
              (crossorigin "anonymous")))
   (tagged% 'script '()
    @string%{
      document.addEventListener("DOMContentLoaded", function() {
        const elems = document.getElementsByClassName('@|katex-class-name|');
        for(let i = 0; i < elems.length; i++) {
          renderMathInElement(elems[i], {
            delimiters: [
              {left: '@(js-escape katex-delimiter-left)',
               right: '@(js-escape katex-delimiter-right)',
               display: false}
            ]
          });
        }
      });
    })))

(define ((default-config:head-template [_cfg : Config] [_n : (U Node Document)])
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
  (user-config
   default-config:body-template
   default-config:head-template
   default-config:assets))
