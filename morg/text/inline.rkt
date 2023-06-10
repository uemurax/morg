#lang at-exp typed/racket

(require "../data/inline.rkt"
         "../data/splice.rkt"
         "../data/node.rkt"
         "../data/article.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "splice.rkt"
         "numbering.rkt"
         "id.rkt"
         "tex.rkt"
         "config.rkt")

(provide inline->text)

(: inline->text : (Config . -> . (Inline . -> . StringTree)))

(define ((inline->text cfg) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->text cfg) x)]
   [(splice? x) ((splice->text (inline->text cfg)) x)]
   [(ref? x) ((ref->text cfg) x)]
   [(math? x) ((math->text cfg) x)]
   [(unordered-list? x) ((unordered-list->text cfg) x)]
   [else (error "Unimplemented.")]))

(define ((text->text [_cfg : Config]) [t : Text]) : StringTree
  (string% (text-contents t)))

(define ((ref->text [cfg : Config]) [r : Ref]) : StringTree
  (define tbl (config-node-table cfg))
  (define id (ref-id r))
  (define in? (node-table-has-key? tbl id))
  (define s
    (cond
     [in?
      (define user (config-user-config cfg))
      (define mk (user-config-make-section-ref user))
      (define nd (node-table-ref tbl id))
      (define s1
        (cond
         [(section-node? nd)
          (mk (length (node-trace nd))
              (section-node-format-index nd))]
         [(article-node? nd)
          @string%{@((inline->text cfg) (article-header (article-node-contents nd))) @(article-node-format-index nd)}]))
      @string%{@|s1| }]
     [else @string%{}]))
  @string%{@|s|@(id->text id)})

(define ((math->text [_cfg : Config]) [m : Math]) : StringTree
  @string%{\(@(math-tex->text (math-contents m))\)})

(define ((list-item->text [cfg : Config])
         [i : ListItem]) : StringTree
  @string%{* @((inline->text cfg) (list-item-contents i))})

(define ((unordered-list->text [cfg : Config])
         [ul : UnorderedList]) : StringTree
  @string%{
    [@(apply % (map (list-item->text cfg) (unordered-list-contents ul)))]
  })
