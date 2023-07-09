#lang at-exp typed/racket

(require "../data/node.rkt"
         "../data/document.rkt"
         "../data/id.rkt"
         "../data/inline.rkt"
         "../data/extension.rkt"
         "../markup/xexpr.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "site-state.rkt"
         "d-pad.rkt"
         "breadcrumb.rkt"
         "document-toc.rkt"
         "pure-inline.rkt"
         "id.rkt"
         "class.rkt"
         "class/inline.rkt"
         "class/id.rkt"
         "class/toc.rkt"
         "class/article.rkt"
         "class/section.rkt"
         "class/block.rkt"
         "class/d-pad.rkt"
         "class/breadcrumb.rkt"
         "class/document-toc.rkt"
         "class/document.rkt")

(provide Assets
         (struct-out config) Config
         site-state-node-ref
         config-add-css
         default-config)

(define-type Assets
  (HashTable String StringTreeLike))

(struct config
  ([body-template : (SiteState (U Node Document) . -> . (XExprs . -> . XExprs))]
   [head-template : (SiteState (U Node Document) . -> . (XExprs . -> . XExprs))]
   [render-extension : (ExtHash ((Listof XExprs) . -> . XExprs))]
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

(define site-title-class-name (class-name "default-site-title"))
(define main-container-class-name (class-name "default-main-container"))
(define main-container-alt-class-name (class-name "default-main-container-alt"))

(define id-name class-name)
(define main-container-id (id-name "default-main-container"))
(define body-container-id (id-name "default-body-container"))
(define header-id (id-name "default-header"))
(define side-nav-id (id-name "default-side-nav"))
(define side-button-id (id-name "default-side-button"))

(define (js-name [s : String])
  (format "morg_generated_~a" s))
(define toggle-side-nav (js-name "default_toggle_side_nav"))

(define ((default-config:body-template:main-container
          [st : SiteState] [n : (U Node Document)])
         [x : XExprs]) : XExprs
  (define node?
    ((make-predicate Node) n))
  (tagged% 'div
           (if node? `((id ,main-container-id)) '())
           (tagged% 'div
                    `((class ,(if node?
                                  main-container-class-name
                                  main-container-alt-class-name)))
                    (when% node?
                           (tagged% 'nav '()
                           (make-breadcrumb st n)))
                    x
                    (when% node?
                      (tagged% 'nav '()
                               (make-d-pad st n))))))

(define ((default-config:body-template [st : SiteState] [n : (U Node Document)])
         [x : XExprs]) : XExprs
  (define main
    ((default-config:body-template:main-container st n) x))
  (cond
   [(document? n) main]
   [else
    (define doc (site-state-root st))
    (define tr
      (map node-id (node-trace n)))
    (tagged% 'div
             `((id ,body-container-id))
             (tagged% 'div
                      `((id ,header-id))
                      (tagged% 'button
                               `((id ,side-button-id)
                                 (onclick ,(string-tree->string
                                            @string%{@|toggle-side-nav|();})))
                               "☰")
                      (tagged% 'script '()
                               @string%{
                                 function @|toggle-side-nav|() {
                                   console.log('called.');
                                   let e = document.getElementById('@|side-nav-id|');
                                   console.log(e);
                                   if (e.style.display == "none") {
                                     e.style.display = "block";
                                   }
                                   else {
                                     e.style.display = "none";
                                   }
                                 }
                               })
                      (tagged% 'a
                               `((class ,site-title-class-name)
                                 (href ,(id->url (document-id doc))))
                               (pure-inline->xexprs (document-title doc))))
             (tagged% 'nav
                      `((id ,side-nav-id)
                        (style "display: none;"))
                      (make-document-toc (site-state-root st) tr))
             main)]))

(define ((default-config:head-template [_st : SiteState] [_n : (U Node Document)])
         [x : XExprs]) : XExprs
  (xexprs%
   x
   (tagged% 'link
            `((rel "stylesheet")
              (href ,default-config:css-name)))))

(define header-size "2em")
(define side-nav-width "min(16em, 20%)")
(define main-padding 0.5)

(define default-config:css
  @string%{
    body {
      font-family: sans-serif;
      font-size: 112.5%;
      margin: 0;
      background-color: White;
    }
    .@|main-container-class-name|, .@|main-container-alt-class-name| {
      max-width: 40em;
      margin-inline: auto;
    }
    .@|id-class-name| {
      color: gray;
      font-family: monospace, monospace;
    }
    .@|code-class-name| {
      font-family: monospace, monospace;
      background-color: WhiteSmoke;
    }
    .@|unordered-list-class-name|, .@|ordered-list-class-name| {
      padding-inline-start: 1em;
    }
    .@|list-item-head-class-name| {
      margin-inline-end: 1em;
    }
    .@|display-class-name| {
      margin-block: 1em;
    }
    .@|toc-node-class-name| {
      list-style-type: none;
      padding-inline-start: 1em;
    }
    .@|toc-edge-summary-selected-class-name| {
      background-color: LightGoldenRodYellow;
      font-weight: bold;
    }
    .@|article-class-name|, .@|statement-class-name|, .@|proof-class-name| {
      margin-block: 1em;
    }
    .@|statement-header-header-class-name|, .@|proof-header-class-name| {
      font-weight: bold;
    }
    .@|document-author-list-class-name| {
      list-style-type: none;
      padding-inline-start: 0;
    }
    .@|d-pad-class-name| {
      list-style-type: none;
      display: flex;
      justify-content: center;
    }
    .@|d-pad-previous-class-name|, .@|d-pad-up-class-name|, .@|d-pad-next-class-name| {
      margin-inline: 1em;
      display: inline;
    }
    .@|breadcrumb-class-name| {
      list-style-type: none;
      padding: 0;
    }
    .@|breadcrumb-top-class-name|, .@|breadcrumb-node-class-name| {
      display: inline;
    }
    .@|breadcrumb-node-class-name|::before {
      content: " > ";
    }
    .@|document-toc-class-name| {
      list-style-type: none;
      padding: 0;
    }
    .@|site-title-class-name| {
      font-weight: bold;
      text-decoration-line: none;
      color: gray;
      margin-inline: 1em;
    }
    #@|body-container-id| {
      background-color: inherit;
    }
    #@|header-id| {
      z-index: 8;
      background-color: inherit;
      position: sticky;
      top: 0;
      left: 0;
      height: @|header-size|;
      display: flex;
      align-items: center;
      justify-content: start;
    }
    #@|side-nav-id| {
      background-color: inherit;
      z-index: 4;
      position: sticky;
      top: @|header-size|;
      left: 0;
    }
    #@|side-nav-id| .@|document-toc-class-name| {
      margin-block: 0;
    }
    #@|main-container-id| {
      padding: @(number->string main-padding)em;
    }
    @"@"media screen and (min-width: 60em) {
      #@|side-nav-id| {
        display: block !important;
        position: fixed;
        width: @|side-nav-width|;
        height: calc(100% - @|header-size|);
        overflow-y: auto;
      }
      #@|main-container-id| {
        position: relative;
        left: @|side-nav-width|;
        width: calc(100% - @|side-nav-width| - @(number->string (* 2 main-padding))em);
      }
      .@|main-container-class-name| {
        margin-inline-start: 1em;
      }
      #@|side-button-id| {
        display: none;
      }
    }
  })

(define default-config:assets
  (hash default-config:css-name default-config:css))

(define default-config
  (config
   default-config:body-template
   default-config:head-template
   (empty-ext-hash)
   default-config:assets))

(define (config-add-css [cfg : Config] 
                        [name : String]
                        [css : StringTreeLike]) : Config
  (struct-copy config cfg
   [assets
    (let ([a (config-assets cfg)])
      (if (hash-has-key? a name)
          (error (format "CSS name already exists: ~a" name))
          (hash-set a name css)))]
   [head-template
    (let ([t (config-head-template cfg)])
      (lambda ([st : SiteState] [n : (U Node Document)])
        (lambda ([x : XExprs])
          (xexprs%
           ((t st n) x)
           (tagged% 'link
                    `((rel "stylesheet")
                      (href ,name)))))))]))
