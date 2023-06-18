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
         "config.rkt"
         "state.rkt")

(provide inline->text
         pure-inline->text)

(: inline->text : (State . -> . (Inline . -> . StringTree)))
(: pure-inline->text : (PureInline . -> . StringTree))

(define (text->text [t : Text]) : StringTree
  (string% (text-contents t)))

(define ((ref->text [st : State]) [r : Ref]) : StringTree
  (define tbl (state-node-table st))
  (define id (ref-id r))
  (define in? (node-table-has-key? tbl id))
  (define s
    (cond
     [in?
      (define user (state-config st))
      (define mk (config-make-section-ref% user))
      (define nd (node-table-ref tbl id))
      (define s1
        (cond
         [(section-node? nd)
          (mk (length (node-trace nd))
              (section-node-format-index nd))]
         [(article-node? nd)
          @string%{@(pure-inline->text (article-header (article-node-contents nd))) @(article-node-format-index nd)}]))
      @string%{@|s1| }]
     [else @string%{}]))
  @string%{@|s|@(id->text id)})

(define (math->text [m : Math]) : StringTree
  @string%{\(@(math-tex->text (math-contents m))\)})

(define #:forall (Inline)
        ((list-item->text [f : (Inline . -> . StringTree)])
         [i : (ListItem Inline)]) : StringTree
  @string%{ @(f (list-item-head i)) @(f (list-item-contents i))})

(define #:forall (Inline)
        ((unordered-list->text [f : (Inline . -> . StringTree)])
         [ul : (UnorderedList Inline)]) : StringTree
  @string%{
    {@(apply % (map (list-item->text f) (unordered-list-contents ul)))}
  })

(define #:forall (Inline)
        ((ordered-list->text [f : (Inline . -> . StringTree)])
         [ol : (OrderedList Inline)]) : StringTree
  @string%{
    {@(apply % (map (list-item->text f) (ordered-list-contents ol)))}
  })

(define #:forall (Inline)
        ((href->text [f : (Inline . -> . StringTree)])
         [h : (HRef Inline)]) : StringTree
  (define url (href-url h))
  (define contents (href-contents h))
  (if contents
      @string%{[@(f contents)](@|url|)}
      @string%{<@|url|>}))

(define #:forall (Inline)
        ((emph->text [f : (Inline . -> . StringTree)])
         [e : (Emph Inline)]) : StringTree
  @string%{*@(f (emph-contents e))*})

(define #:forall (Inline)
        ((display->text [f : (Inline . -> . StringTree)])
         [d : (Display Inline)]) : StringTree
  @string%{
    
    
    @(f (display-contents d))
    
    
  })

(define #:forall (Inline)
        ((code->text [f : (Inline . -> . StringTree)])
         [c : (Code Inline)]) : StringTree
  @string%{`@(f (code-contents c))`})

(define #:forall (Inline)
        ((dfn->text [f : (Inline . -> . StringTree)])
         [d : (Dfn Inline)]) : StringTree
  @string%{*@(f (dfn-contents d))*})

(define #:forall (Inline)
        ((pure-inline-element->text [f : (Inline . -> . StringTree)])
         [pi : (PureInlineElement Inline)]) : StringTree
  (cond
   [(text? pi) (text->text pi)]
   [(math? pi) (math->text pi)]
   [(unordered-list? pi) ((unordered-list->text f) pi)]
   [(ordered-list? pi) ((ordered-list->text f) pi)]
   [(href? pi) ((href->text f) pi)]
   [(emph? pi) ((emph->text f) pi)]
   [(display? pi) ((display->text f) pi)]
   [(code? pi) ((code->text f) pi)]
   [(dfn? pi) ((dfn->text f) pi)]))

(define #:forall (Inline)
        ((inline-element->text [st : State]
                               [f : (Inline . -> . StringTree)])
         [i : (InlineElement Inline)]) : StringTree
  (cond
   [(ref? i) ((ref->text st) i)]
   [(anchor? i) (error "Unimplemented.")]
   [(anchor-ref? i) (error "Unimplemented.")]
   [else ((pure-inline-element->text f) i)]))

(define ((inline->text st) i)
  (define x (inline-contents i))
  (define f (inline->text st))
  (cond
   [(splice? x) (string% (splice-map f x))]
   [else ((inline-element->text st f) x)]))

(define (pure-inline->text pi)
  (define x (pure-inline-contents pi))
  (define f pure-inline->text)
  (cond
   [(splice? x) (string% (splice-map f x))]
   [else ((pure-inline-element->text f) x)]))
