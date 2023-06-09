#lang typed/racket

(require "../data/inline.rkt"
         "../markup/xexpr.rkt"
         "../text/tex.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "class.rkt"
         "id.rkt"
         "splice.rkt")

(provide inline->xexprs
         katex-class-name
         katex-delimiter-left
         katex-delimiter-right
         ref-class-name)

(define (text->xexprs [x : Text]) : XExprs
  (xexprs% (text-contents x)))

(define katex-class-name (class-name "katex"))

(define katex-delimiter-left "\\(")
(define katex-delimiter-right "\\)")

(define (math->xexprs [x : Math]) : XExprs
  (tagged% 'span
           `((class ,katex-class-name))
           (string-tree-like->string*
            katex-delimiter-left
            (math-tex->text (math-contents x))
            katex-delimiter-right)))

(define ref-class-name (class-name "ref"))

(define (ref->xexprs [x : Ref]) : XExprs
  (define i (ref-id x))
  (id->xexprs/a i))

(define (inline->xexprs [i : Inline]) : XExprs
  (define x (inline-contents i))
  (cond
   [(text? x) (text->xexprs x)]
   [(math? x) (math->xexprs x)]
   [(ref? x) (ref->xexprs x)]
   [(splice? x) ((splice->xexprs inline->xexprs) x)]
   [else (error "Unimplemented.")]))
