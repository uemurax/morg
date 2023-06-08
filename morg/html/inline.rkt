#lang typed/racket

(require "../data/inline.rkt"
         "../markup/xexpr.rkt"
         "../text/tex.rkt"
         "../text/id.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "class.rkt"
         "id.rkt"
         "splice.rkt"
         "config.rkt")

(provide inline->xexprs
         katex-class-name
         katex-delimiter-left
         katex-delimiter-right
         ref-class-name)

(define ((text->xexprs [_cfg : Config]) [x : Text]) : XExprs
  (xexprs% (text-contents x)))

(define katex-class-name (class-name "katex"))

(define katex-delimiter-left "\\(")
(define katex-delimiter-right "\\)")

(define ((math->xexprs [_cfg : Config]) [x : Math]) : XExprs
  (tagged% 'span
           `((class ,katex-class-name))
           (string-append
            katex-delimiter-left
            (string-tree->string (math-tex->text (math-contents x)))
            katex-delimiter-right)))

(define ref-class-name (class-name "ref"))

(define ((ref->xexprs [cfg : Config]) [x : Ref]) : XExprs
  (define i (ref-id x))
  ((id->xexprs/a cfg) i))

(: inline->xexprs : (Config . -> . (Inline . -> . XExprs)))

(define ((inline->xexprs cfg) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->xexprs cfg) x)]
   [(math? x) ((math->xexprs cfg) x)]
   [(ref? x) ((ref->xexprs cfg) x)]
   [(splice? x) ((splice->xexprs (inline->xexprs cfg)) x)]
   [else (error "Unimplemented.")]))
