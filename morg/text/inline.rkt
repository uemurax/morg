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

(provide inline->text)

(: inline->text : (State . -> . (Inline . -> . StringTree)))

(define ((inline->text st) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->text st) x)]
   [(splice? x) ((splice->text (inline->text st)) x)]
   [(ref? x) ((ref->text st) x)]
   [(math? x) ((math->text st) x)]
   [(unordered-list? x) ((unordered-list->text st) x)]
   [(href? x) ((href->text st) x)]
   [(emph? x) ((emph->text st) x)]
   [(display? x) ((display->text st) x)]
   [(code? x) ((code->text st) x)]
   [else (error "Unimplemented.")]))

(define ((text->text [_st : State]) [t : Text]) : StringTree
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
          @string%{@((inline->text st) (article-header (article-node-contents nd))) @(article-node-format-index nd)}]))
      @string%{@|s1| }]
     [else @string%{}]))
  @string%{@|s|@(id->text id)})

(define ((math->text [_st : State]) [m : Math]) : StringTree
  @string%{\(@(math-tex->text (math-contents m))\)})

(define ((list-item->text [st : State])
         [i : ListItem]) : StringTree
  @string%{ @(list-item-head i) @((inline->text st) (list-item-contents i))})

(define ((unordered-list->text [st : State])
         [ul : UnorderedList]) : StringTree
  @string%{
    {@(apply % (map (list-item->text st) (unordered-list-contents ul)))}
  })

(define ((href->text [st : State])
         [h : HRef]) : StringTree
  (define url (href-url h))
  (define contents (href-contents h))
  (if contents
      @string%{[@((inline->text st) contents)](@|url|)}
      @string%{<@|url|>}))

(define ((emph->text [st : State])
         [e : Emph]) : StringTree
  @string%{*@((inline->text st) (emph-contents e))*})

(define ((display->text [st : State])
         [d : Display]) : StringTree
  @string%{
    
    
    @((inline->text st) (display-contents d))
    
    
  })

(define ((code->text [st : State])
         [c : Code]) : StringTree
  @string%{`@((inline->text st) (code-contents c))`})
