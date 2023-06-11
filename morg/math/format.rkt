#lang at-exp typed/racket

(require "tex-plus.rkt"
         "../data/tex.rkt"
         "../data/splice.rkt"
         "../markup/tex.rkt")

(provide (struct-out user-config) UserConfig
         (struct-out config) Config
         default-config
         format-math-tex+)

(struct user-config
  ([levels : (Listof Symbol)])
  #:transparent
  #:type-name UserConfig)

(struct config
  ([user-config : UserConfig]
   [level : Level])
  #:transparent
  #:type-name Config)

(define default-config
  (user-config
   '(+ * generic-bin generic-rel comma)))

(: format-math-tex+ : (Config . -> . (MathTeX+ . -> . MathTeX)))

(define ((format-paren [cfg : Config])
         [p : (Paren MathTeX+)]) : (Group MathTeX)
  (define usr-cfg (config-user-config cfg))
  (define ss (user-config-levels usr-cfg))
  (define cur-lv (config-level cfg))
  (define lv (paren-level p))
  (define cfg-1
    (struct-copy config cfg
     [level lv]))
  (define f (format-math-tex+ cfg-1))
  (define contents (group (f (paren-contents p))))
  (define comp (lv . (level-compare ss) . cur-lv))
  (case comp
   [(<) contents]
   [else
    (define l (f (paren-left p)))
    (define r (f (paren-right p)))
    (group (math-tex% l contents r))]))

(define ((format-atom+ [cfg : Config])
         [a : (Atom+ MathTeX+)]) : (Atom MathTeX)
  (define x (atom+-contents a))
  (define f (format-math-tex+ cfg))
  (cond
   [(atom? x) ((atom-map f) x)]
   [(paren? x) (atom ((format-paren cfg) x))]))

(define ((format-sub-sup [cfg : Config])
         [s : (SubSup (Atom+ MathTeX+) MathTeX+)]) : (SubSup (Atom MathTeX) MathTeX)
  (define cfg-1
    (struct-copy config cfg
     [level (level #f 0)]))
  ((sub-sup-map (format-atom+ cfg-1) (format-math-tex+ cfg)) s))

(define ((format-math-tex+ cfg) m)
  (define x (math-tex+-contents m))
  (cond
   [(atom+? x) (math-tex ((format-atom+ cfg) x))]
   [(splice? x) (math-tex (splice-map (format-math-tex+ cfg) x))]
   [(sub-sup? x) (math-tex ((format-sub-sup cfg) x))]))
