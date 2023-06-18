#lang at-exp typed/racket

(require "../data/inline.rkt"
         "../markup/xexpr.rkt"
         "../text/tex.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "class.rkt"
         "id.rkt"
         "splice.rkt")

(provide inline->xexprs
         pure-inline->xexprs
         katex-delimiter-left
         katex-delimiter-right)

(module style typed/racket
  (require "class.rkt"
           "../markup/string.rkt")
  (provide katex-class-name
           ref-class-name
           list-item-class-name
           list-item-head-class-name
           unordered-list-class-name
           ordered-list-class-name
           href-class-name
           emph-class-name
           display-class-name
           code-class-name
           dfn-class-name
           inline-css)

  (define katex-class-name (class-name "katex"))
  (define ref-class-name (class-name "ref"))
  (define list-item-class-name (class-name "list-item"))
  (define list-item-head-class-name (class-name "list-item-head"))
  (define unordered-list-class-name (class-name "unordered-list"))
  (define ordered-list-class-name (class-name "ordered-list"))
  (define href-class-name (class-name "href"))
  (define emph-class-name (class-name "emph"))
  (define display-class-name (class-name "display"))
  (define code-class-name (class-name "code"))
  (define dfn-class-name (class-name "dfn"))
  
  (define inline-css
    @string%{
      .@|unordered-list-class-name|, .@|ordered-list-class-name| {
        padding-inline-start: 1em;
      }
      .@|list-item-head-class-name| {
        margin-inline-end: 1em;
      }
      .@|dfn-class-name| {
        font-style: normal;
        font-weight: bold;
      }
    }))

(require 'style)

(define (text->xexprs [x : Text]) : XExprs
  (xexprs% (text-contents x)))

(define katex-delimiter-left "\\(")
(define katex-delimiter-right "\\)")

(define (math->xexprs [x : Math]) : XExprs
  (tagged% 'span
           `((class ,katex-class-name))
           (string-tree-like->string*
            katex-delimiter-left
            (math-tex->text (math-contents x))
            katex-delimiter-right)))

(define (ref->xexprs [x : Ref]) : XExprs
  (define i (ref-id x))
  (id->xexprs/a i))

(define #:forall (Inline)
        ((list-item->xexprs [f : (Inline . -> . XExprs)])
          [i : (ListItem Inline)]) : XExprs
  (tagged% 'li
           `((class ,list-item-class-name))
           (tagged% 'span
                    `((class ,list-item-head-class-name))
                    (f (list-item-head i)))
           (f (list-item-contents i))))

(define #:forall (Inline)
        ((unordered-list->xexprs [f : (Inline . -> . XExprs)])
         [ul : (UnorderedList Inline)]) : XExprs
  (tagged% 'ul
           `((class ,unordered-list-class-name)
             (style "list-style-type: none;"))
           (apply % (map (list-item->xexprs f) (unordered-list-contents ul)))))

(define #:forall (Inline)
        ((ordered-list->xexprs [f : (Inline . -> . XExprs)])
         [ol : (OrderedList Inline)]) : XExprs
  (tagged% 'ol
           `((class ,ordered-list-class-name)
             (style "list-style-type: none;"))
           (apply % (map (list-item->xexprs f) (ordered-list-contents ol)))))

(define #:forall (Inline)
        ((href->xexprs [f : (Inline . -> . XExprs)])
         [h : (HRef Inline)]) : XExprs
  (define url (href-url h))
  (define contents (href-contents h))
  (tagged% 'a
           `((class ,href-class-name)
             (href ,url))
           (if contents
               (f contents)
               url)))

(define #:forall (Inline)
        ((emph->xexprs [f : (Inline . -> . XExprs)])
         [e : (Emph Inline)]) : XExprs
  (tagged% 'em
           `((class ,emph-class-name))
           (f (emph-contents e))))

(define #:forall (Inline)
        ((display->xexprs [f : (Inline . -> . XExprs)])
         [d : (Display Inline)]) : XExprs
  (tagged% 'center
           `((class ,display-class-name))
           (f (display-contents d))))

(define #:forall (Inline)
        ((code->xexprs [f : (Inline . -> . XExprs)])
         [c : (Code Inline)]) : XExprs
  (tagged% 'code
           `((class ,code-class-name))
           (f (code-contents c))))

(define #:forall (Inline)
        ((dfn->xexprs [f : (Inline . -> . XExprs)])
         [d : (Dfn Inline)]) : XExprs
  (tagged% 'dfn
           `((class ,dfn-class-name))
           (f (dfn-contents d))))

(define #:forall (Inline)
        ((pure-inline-element->xexprs [f : (Inline . -> . XExprs)])
         [pi : (PureInlineElement Inline)]) : XExprs
  (cond
   [(text? pi) (text->xexprs pi)]
   [(math? pi) (math->xexprs pi)]
   [(unordered-list? pi) ((unordered-list->xexprs f) pi)]
   [(ordered-list? pi) ((ordered-list->xexprs f) pi)]
   [(href? pi) ((href->xexprs f) pi)]
   [(emph? pi) ((emph->xexprs f) pi)]
   [(display? pi) ((display->xexprs f) pi)]
   [(code? pi) ((code->xexprs f) pi)]
   [(dfn? pi) ((dfn->xexprs f) pi)]))

(define #:forall (Inline)
        ((inline-element->xexprs [f : (Inline . -> . XExprs)])
         [i : (InlineElement Inline)]) : XExprs
  (cond
   [(ref? i) (ref->xexprs i)]
   [(anchor? i) (error "Unimplemented.")]
   [(anchor-ref? i) (error "Unimplemented.")]
   [else ((pure-inline-element->xexprs f) i)]))

(define (pure-inline->xexprs [pi : PureInline]) : XExprs
  (define x (pure-inline-contents pi))
  (define f pure-inline->xexprs)
  (cond
   [(splice? x) ((splice->xexprs f) x)]
   [else ((pure-inline-element->xexprs f) x)]))

(define (inline->xexprs [i : Inline]) : XExprs
  (define x (inline-contents i))
  (define f inline->xexprs)
  (cond
   [(splice? x) ((splice->xexprs inline->xexprs) x)]
   [else ((inline-element->xexprs f) x)]))
