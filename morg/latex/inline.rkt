#lang at-exp typed/racket

(require "../data/inline.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/node.rkt"
         "../data/article.rkt"
         "../data/splice.rkt"
         "../markup/tex.rkt"
         "../markup/splice.rkt"
         "../text/numbering.rkt"
         "id.rkt"
         "splice.rkt"
         "config.rkt")

(provide inline->latex)

(: inline->latex : (Config . -> . (Inline . -> . tex:TextTeX)))

(define ((text->latex [_cfg : Config])
          [x : Text]) : tex:TextTeX
  @text-tex%{@(text-contents x)})

(define ((ref->latex [cfg : Config])
         [x : Ref]) : tex:TextTeX
  (define i (ref-id x))
  (define tbl (config-node-table cfg))
  (define in? (node-table-has-key? tbl i))
  (define text : TextTeXLike
    (cond
     [in?
      (define nd (node-table-ref tbl i))
      (cond
       [(section-node? nd)
        (define mk (user-config-make-section-ref (config-user-config cfg)))
        @(mk (length (node-trace nd))
             (section-node-format-index nd))]
       [(article-node? nd)
        (define a (article-node-contents nd))
        @%{@((inline->latex cfg) (article-header a)) @(article-node-format-index nd)}])]
     [else (id->latex i)]))
  @text-tex%{@(id->hyperlink i text)})

(define ((math->latex [_cfg : Config])
         [x : Math]) : tex:TextTeX
  (tex:text-tex (tex:math (math-contents x))))

(define ((list-item->latex [cfg : Config])
         [i : ListItem]) : tex:TextTeX
  @text-tex%{@macro%["item"]@((inline->latex cfg) (list-item-contents i))})

(define ((unordered-list->latex [cfg : Config])
         [ul : UnorderedList]) : tex:TextTeX
  @text-tex%{
    @environment%["itemize"]{
      @(apply % (map (list-item->latex cfg) (unordered-list-contents ul)))
    }
  })

(define ((href->latex [cfg : Config])
         [h : HRef]) : tex:TextTeX
  (define url (href-url h))
  (define contents (href-contents h))
  (define x @text-tex%{@macro%["url" @argument%{@|url|}]})
  (if contents
      @text-tex%{@((inline->latex cfg) contents)@macro%["footnote" @argument%{@|x|}]}
      x))

(define ((inline->latex cfg) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->latex cfg) x)]
   [(math? x) ((math->latex cfg) x)]
   [(ref? x) ((ref->latex cfg) x)]
   [(unordered-list? x) ((unordered-list->latex cfg) x)]
   [(href? x) ((href->latex cfg) x)]
   [(splice? x) ((splice->latex (inline->latex cfg)) x)]
   [else (error "Unimplemented.")]))
