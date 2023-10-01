#lang at-exp typed/racket

(require (prefix-in xml: typed/xml)
         (prefix-in url: typed/net/url)
         "../data/document.rkt"
         "../data/node.rkt"
         "../data/id.rkt"
         "../data/section.rkt"
         "../data/article.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../markup/inline.rkt"
         "../markup/splice.rkt"
         "../text/id.rkt"
         "../util/escape.rkt"
         "../text/inline.rkt"
         "pure-inline.rkt"
         "id.rkt"
         "document.rkt"
         "config.rkt"
         "site-state.rkt"
         "class/inline.rkt")

(require/typed racket/hash
  [hash-union ((HashTable String String) (HashTable String String) . -> . (HashTable String String))])

(provide Site
         make-site)

(define-type Site
  (HashTable String String))

(define og-prefix "og:")

(define (og-meta [property : String]
                 [content : StringTreeLike])
  (tagged% 'meta
           `((property ,(string-append og-prefix property))
             (content ,(string-tree-like->string content)))))

(define ((apply-template [cfg : Config] [st : SiteState])
         [i : Id] [x : XExprs]) : (Values String String)
  (define n (site-state-node-ref st i))
  (define head
    (((config-head-template cfg) st n) 
     (head-init cfg st n)))
  (define body
    (((config-body-template cfg) st n)
     (tagged% 'main '() x)))
  (define html
    (car 
     (tagged% 'html '()
              (tagged% 'head
                       `((prefix
                          ,(format "~a https://ogp.me/ns#"
                                   og-prefix)))
                       head)
              (tagged% 'body '() body))))
  (values (id->url i)
          (parameterize ([xml:current-unescaped-tags xml:html-unescaped-tags]
                         [xml:empty-tag-shorthand xml:html-empty-tags])
            (string-append
             "<!DOCTYPE html>\n"
             (xml:xexpr->string html)))))

(define (make-site [cfg : Config] [doc : Document]) : Site
  (define pages
    ((document->xexprs cfg) doc))
  (define st
    (site-state
     (make-node-table (document-front doc))
     (make-node-table (document-main doc))
     (make-node-table (document-back doc))
     doc))
  (define site
    (hash-map/copy pages (apply-template cfg st)))
  (define assets
    (hash-map/copy (config-assets cfg)
      (lambda ([k : String] [x : StringTreeLike])
        (values k (string-tree-like->string x)))))
  (hash-union site assets))

(define (js-escape [x : String])
  (escape (hash "\\" "\\\\") x))

(define (head-init [cfg : Config] [st : SiteState] [n : (U Node Document)]) : XExprs
  (define doc (site-state-root st))
  (define doc-title (document-title doc))
  (define base-url (config-base-url cfg))
  (define url
    (and base-url
         (cond
           [(document? n) base-url]
           [else
            (url:combine-url/relative
             base-url
             (id->url (node-id n)))])))
  (define page-title
    (cond
      [(document? n) doc-title]
      [(section-node? n)
       (section-title (section-node-contents n))]
      [(article-node? n)
       (define a (article-node-contents n))
       (define t (article-title a))
       (if t t (pure-inline% (id->text (article-id a))))]))
  (define title
    (cond
      [(document? n) doc-title]
      [else
       @pure-inline%{@|page-title| -- @|doc-title|}]))
  (xexprs%
   (tagged% 'meta '((charset "UTF-8")))
   (tagged% 'meta '((name "viewport")
                    (content "width=device-width,initial-scale=1")))
   (tagged% 'title '()
            (pure-inline->xexprs title))
   (tagged% 'link
            '((rel "stylesheet")
              (href "https://cdn.jsdelivr.net/npm/katex@0.16.7/dist/katex.min.css")
              (integrity "sha384-3UiQGuEI4TTMaFmGIZumfRPtfKQ3trwQE2JgosJxCnGmQpL/lJdjpcHkaaFwHlcI")
              (crossorigin "anonymous")))
   (og-meta "title" (pure-inline->text page-title))
   (og-meta "site_name" (pure-inline->text doc-title))
   (og-meta "type" (if (document? n) "website" "article"))
   (when% url
          (og-meta "url" (url:url->string url)))
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
