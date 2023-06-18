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
         "config.rkt"
         (submod "inline.rkt" style))

(require/typed racket/hash
  [hash-union ((HashTable String String) (HashTable String String) . -> . (HashTable String String))])

(provide Site
         make-site)

(define-type Site
  (HashTable String String))

(define ((apply-template [cfg : Config] [st : SiteState])
         [i : Id] [x : XExprs]) : (Values String String)
  (define n (site-state-node-ref st i))
  (define head
    (((config-head-template cfg) st n) 
     (head-init st n)))
  (define body
    (((config-body-template cfg) st n)
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

(define (make-site [cfg : Config] [doc : Document]) : Site
  (define pages
    (document->xexprs doc))
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

(define (head-init [st : SiteState] [n : (U Node Document)]) : XExprs
  (define doc (site-state-root st))
  (define doc-title (document-title doc))
  (define title
    (cond
     [(document? n) doc-title]
     [(section-node? n)
      @pure-inline%{@(section-title (section-node-contents n)) -- @|doc-title|}]
     [(article-node? n)
      (define a (article-node-contents n))
      (define t (article-title a))
      (define h
        (if t t (id->text (article-id a))))
      @pure-inline%{@|h| -- @|doc-title|}]))
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
