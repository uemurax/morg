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

(provide inline->latex
         pure-inline->latex)

(: inline->latex : (State . -> . (Inline . -> . tex:TextTeX)))
(: pure-inline->latex : (PureInline . -> . tex:TextTeX))

(define (text->latex [x : Text]) : tex:TextTeX
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
        @%{@(pure-inline->latex (article-header a)) @(article-node-format-index nd)}])]
     [else (id->latex i)]))
  @text-tex%{@(id->hyperlink i text)})

(define (math->latex [x : Math]) : tex:TextTeX
  (tex:text-tex (tex:math (math-contents x))))

(define #:forall (Inline)
        ((list-item->latex [f : (Inline . -> . tex:TextTeX)])
         [i : (ListItem Inline)]) : tex:TextTeX
  (define itm
    @macro%["item" @optional-argument%{@(f (list-item-head i))}])
  @text-tex%{@|itm|@(f (list-item-contents i))})

(define #:forall (Inline)
        ((unordered-list->latex [f : (Inline . -> . tex:TextTeX)])
         [ul : (UnorderedList Inline)]) : tex:TextTeX
  @text-tex%{
    @environment%["itemize"]{
      @(apply % (map (list-item->latex f) (unordered-list-contents ul)))
    }
  })

(define #:forall (Inline)
        ((ordered-list->latex [f : (Inline . -> . tex:TextTeX)])
         [ol : (OrderedList Inline)]) : tex:TextTeX
  @text-tex%{
    @environment%["enumerate"]{
      @(apply % (map (list-item->latex f) (ordered-list-contents ol)))
    }
  })

(define #:forall (Inline)
        ((href->latex [f : (Inline . -> . tex:TextTeX)])
         [h : (HRef Inline)]) : tex:TextTeX
  (define url (href-url h))
  (define contents (href-contents h))
  (define x @text-tex%{@macro%["url" @argument%{@|url|}]})
  (if contents
      @text-tex%{@(f contents)@macro%["footnote" @argument%{@|x|}]}
      x))

(define #:forall (Inline)
        ((emph->latex [f : (Inline . -> . tex:TextTeX)])
         [e : (Emph Inline)]) : tex:TextTeX
  @text-tex%{@macro%["emph" @argument%{@(f (emph-contents e))}]})

(define #:forall (Inline)
        ((display->latex [f : (Inline . -> . tex:TextTeX)])
         [d : (Display Inline)]) : tex:TextTeX
  @text-tex%{@environment%["center"]{
    @(f (display-contents d))
  }})

(define #:forall (Inline)
        ((code->latex [f : (Inline . -> . tex:TextTeX)])
         [c : (Code Inline)]) : tex:TextTeX
  @text-tex%{@macro%["texttt" @argument%{@(f (code-contents c))}]})

(define #:forall (Inline)
        ((dfn->latex [f : (Inline . -> . tex:TextTeX)])
         [d : (Dfn Inline)]) : tex:TextTeX
  @text-tex%{@macro%["emph" @argument%{@(f (dfn-contents d))}]})

(define #:forall (Inline)
        ((pure-inline-element->latex [f : (Inline . -> . tex:TextTeX)])
         [x : (PureInlineElement Inline)]) : tex:TextTeX
  (cond
   [(text? x) (text->latex x)]
   [(math? x) (math->latex x)]
   [(unordered-list? x) ((unordered-list->latex f) x)]
   [(ordered-list? x) ((ordered-list->latex f) x)]
   [(href? x) ((href->latex f) x)]
   [(emph? x) ((emph->latex f) x)]
   [(display? x) ((display->latex f) x)]
   [(code? x) ((code->latex f) x)]
   [(dfn? x) ((dfn->latex f) x)]))

(define #:forall (Inline)
        ((inline-element->latex [st : State]
                                [f : (Inline . -> . tex:TextTeX)])
         [i : (InlineElement Inline)]) : tex:TextTeX
  (cond
   [(ref? i) ((ref->latex st) i)]
   [(anchor? i) (error "Unimplemented.")]
   [(anchor-ref? i) (error "Unimplemented.")]
   [else ((pure-inline-element->latex f) i)]))

(define (pure-inline->latex pi)
  (define x (pure-inline-contents pi))
  (define f pure-inline->latex)
  (cond
   [(splice? x) ((splice->latex f) x)]
   [else ((pure-inline-element->latex f) x)]))

(define ((inline->latex st) i)
  (define x (inline-contents i))
  (define f (inline->latex st))
  (cond
   [(splice? x) ((splice->latex f) x)]
   [else ((inline-element->latex st f) x)]))
