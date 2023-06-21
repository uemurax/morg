#lang at-exp typed/racket

(require "../data/inline.rkt"
         "../markup/xexpr.rkt"
         "../text/id.rkt"
         "../data/splice.rkt"
         "../data/anchor-table.rkt"
         "../data/extension.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "class/inline.rkt"
         "state.rkt"
         "pure-inline.rkt"
         "config.rkt"
         "id.rkt"
         "splice.rkt")

(provide inline->xexprs)

(: inline->xexprs : (State . -> . (Inline . -> . XExprs)))

(define (ref->xexprs [x : Ref]) : XExprs
  (define i (ref-id x))
  (id->xexprs/a i))

(define #:forall (PureInline)
        ((anchor->xexprs [st : State]
                         [g : (PureInline . -> . XExprs)])
          [a : (Anchor PureInline)]) : XExprs
  (tagged% 'a
           `((class ,anchor-class-name)
             (id ,(anchor-id->css-id (state-id st) (anchor-id a))))
           (g (anchor-contents a))))

(define ((anchor-ref->xexprs [st : State])
         [ar : AnchorRef]) : XExprs
  (define id-n (anchor-ref-node ar))
  (define id-a (anchor-ref-anchor ar))
  (define id-this (state-id st))
  (define not-this? (not (equal? id-n id-this)))
  (define url
    (string-tree->string
     @string%{@when%[not-this?]{@(id->url id-n)}#@(anchor-id->css-id id-n id-a)}))
  (define tbl (state-anchor-table st))
  (define key (anchor-key id-n id-a))
  (define l
    (cond
     [(anchor-table-has-key? tbl key)
      (pure-inline->xexprs (anchor-contents (anchor-table-ref tbl key)))]
     [else (anchor-id->text id-n id-a)]))
  (tagged% 'a
           `((class ,anchor-ref-class-name)
             (href ,url))
           l))

(define #:forall (Inline)
        ((extension->xexprs [st : State]
                            [f : (Inline . -> . XExprs)])
         [s : (Extension (Listof Inline))]) : XExprs
  (define cfg (state-config st))
  (define rnd (config-render-extension cfg))
  (define g
    (ext-hash-ref rnd s (lambda ()
                          (lambda ([xs : (Listof XExprs)])
                            (apply xexprs% xs)))))
  (tagged% 'span
           `((class ,inline-ext-class-name))
           (g (map f (extension-contents s)))))

(define #:forall (PureInline Inline)
        ((inline-element->xexprs [st : State]
                                 [g : (PureInline . -> . XExprs)]
                                 [f : (Inline . -> . XExprs)])
         [i : (InlineElement PureInline Inline)]) : XExprs
  (cond
   [(ref? i) (ref->xexprs i)]
   [(anchor? i) ((anchor->xexprs st g) i)]
   [(anchor-ref? i) ((anchor-ref->xexprs st) i)]
   [(extension? i) ((extension->xexprs st f) i)]
   [else ((pure-inline-element->xexprs f) i)]))

(define ((inline->xexprs st) i)
  (define x (inline-contents i))
  (define f (inline->xexprs st))
  (define g pure-inline->xexprs)
  (cond
   [(splice? x) ((splice->xexprs f) x)]
   [else ((inline-element->xexprs st g f) x)]))
