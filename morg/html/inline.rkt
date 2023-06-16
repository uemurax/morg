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

(define (list-item->xexprs [i : ListItem]) : XExprs
  (tagged% 'li
           `((class ,list-item-class-name))
           (tagged% 'span
                    `((class ,list-item-head-class-name))
                    (list-item-head i))
           (inline->xexprs (list-item-contents i))))

(define (unordered-list->xexprs [ul : UnorderedList]) : XExprs
  (tagged% 'ul
           `((class ,unordered-list-class-name)
             (style "list-style-type: none;"))
           (apply % (map list-item->xexprs (unordered-list-contents ul)))))

(define (ordered-list->xexprs [ol : OrderedList]) : XExprs
  (tagged% 'ol
           `((class ,ordered-list-class-name)
             (style "list-style-type: none;"))
           (apply % (map list-item->xexprs (ordered-list-contents ol)))))

(define (href->xexprs [h : HRef]) : XExprs
  (define url (href-url h))
  (define contents (href-contents h))
  (tagged% 'a
           `((class ,href-class-name)
             (href ,url))
           (if contents
               (inline->xexprs contents)
               url)))

(define (emph->xexprs [e : Emph]) : XExprs
  (tagged% 'em
           `((class ,emph-class-name))
           (inline->xexprs (emph-contents e))))

(define (display->xexprs [d : Display]) : XExprs
  (tagged% 'center
           `((class ,display-class-name))
           (inline->xexprs (display-contents d))))

(define (code->xexprs [c : Code]) : XExprs
  (tagged% 'code
           `((class ,code-class-name))
           (inline->xexprs (code-contents c))))

(define (dfn->xexprs [d : Dfn]) : XExprs
  (tagged% 'dfn
           `((class ,dfn-class-name))
           (inline->xexprs (dfn-contents d))))

(define (inline->xexprs [i : Inline]) : XExprs
  (define x (inline-contents i))
  (cond
   [(text? x) (text->xexprs x)]
   [(math? x) (math->xexprs x)]
   [(ref? x) (ref->xexprs x)]
   [(unordered-list? x) (unordered-list->xexprs x)]
   [(ordered-list? x) (ordered-list->xexprs x)]
   [(href? x) (href->xexprs x)]
   [(emph? x) (emph->xexprs x)]
   [(display? x) (display->xexprs x)]
   [(code? x) (code->xexprs x)]
   [(dfn? x) (dfn->xexprs x)]
   [(splice? x) ((splice->xexprs inline->xexprs) x)]
   [else (error "Unimplemented.")]))
