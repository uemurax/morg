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
         "state.rkt"
         "config.rkt")

(provide inline->latex)

(: inline->latex : (State . -> . (Inline . -> . tex:TextTeX)))

(define ((text->latex [_st : State])
          [x : Text]) : tex:TextTeX
  @text-tex%{@(text-contents x)})

(define ((ref->latex [st : State])
         [x : Ref]) : tex:TextTeX
  (define i (ref-id x))
  (define tbl (state-node-table st))
  (define in? (node-table-has-key? tbl i))
  (define text : TextTeXLike
    (cond
     [in?
      (define nd (node-table-ref tbl i))
      (cond
       [(section-node? nd)
        (define mk (config-make-section-ref% (state-config st)))
        @(mk (length (node-trace nd))
             (section-node-format-index nd))]
       [(article-node? nd)
        (define a (article-node-contents nd))
        @%{@((inline->latex st) (article-header a)) @(article-node-format-index nd)}])]
     [else (id->latex i)]))
  @text-tex%{@(id->hyperlink i text)})

(define ((math->latex [_st : State])
         [x : Math]) : tex:TextTeX
  (tex:text-tex (tex:math (math-contents x))))

(define ((list-item->latex [st : State])
         [i : ListItem]) : tex:TextTeX
  (define itm
    @macro%["item" @optional-argument%{@(list-item-head i)}])
  @text-tex%{@|itm|@((inline->latex st) (list-item-contents i))})

(define ((unordered-list->latex [st : State])
         [ul : UnorderedList]) : tex:TextTeX
  @text-tex%{
    @environment%["itemize"]{
      @(apply % (map (list-item->latex st) (unordered-list-contents ul)))
    }
  })

(define ((ordered-list->latex [st : State])
         [ol : OrderedList]) : tex:TextTeX
  @text-tex%{
    @environment%["enumerate"]{
      @(apply % (map (list-item->latex st) (ordered-list-contents ol)))
    }
  })

(define ((href->latex [st : State])
         [h : HRef]) : tex:TextTeX
  (define url (href-url h))
  (define contents (href-contents h))
  (define x @text-tex%{@macro%["url" @argument%{@|url|}]})
  (if contents
      @text-tex%{@((inline->latex st) contents)@macro%["footnote" @argument%{@|x|}]}
      x))

(define ((emph->latex [st : State])
         [e : Emph]) : tex:TextTeX
  @text-tex%{@macro%["emph" @argument%{@((inline->latex st) (emph-contents e))}]})

(define ((display->latex [st : State])
         [d : Display]) : tex:TextTeX
  @text-tex%{@environment%["center"]{
    @((inline->latex st) (display-contents d))
  }})

(define ((code->latex [st : State])
         [c : Code]) : tex:TextTeX
  @text-tex%{@macro%["texttt" @argument%{@((inline->latex st) (code-contents c))}]})

(define ((dfn->latex [st : State])
         [d : Dfn]) : tex:TextTeX
  @text-tex%{@macro%["emph" @argument%{@((inline->latex st) (dfn-contents d))}]})

(define ((inline->latex st) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->latex st) x)]
   [(math? x) ((math->latex st) x)]
   [(ref? x) ((ref->latex st) x)]
   [(unordered-list? x) ((unordered-list->latex st) x)]
   [(ordered-list? x) ( (ordered-list->latex st) x)]
   [(href? x) ((href->latex st) x)]
   [(emph? x) ((emph->latex st) x)]
   [(dfn? x) ((dfn->latex st) x)]
   [(display? x) ((display->latex st) x)]
   [(code? x) ((code->latex st) x)]
   [(splice? x) ((splice->latex (inline->latex st)) x)]
   [else (error "Unimplemented.")]))
