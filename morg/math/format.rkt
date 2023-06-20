#lang at-exp typed/racket

(require "tex-plus.rkt"
         "../data/tex.rkt"
         "../data/splice.rkt"
         "config.rkt"
         "../markup/tex.rkt")

(provide (struct-out state) State
         format-math-tex+)

(struct state
  ([config : Config]
   [level : Level])
  #:transparent
  #:type-name State)

(: format-math-tex+ : (State . -> . (MathTeX+ . -> . MathTeX)))

(define ((format-paren [st : State])
         [p : (Paren MathTeX+)]) : MathTeXLike
  (define cfg (state-config st))
  (define ss (config-levels cfg))
  (define cur-lv (state-level st))
  (define lv (paren-level p))
  (define st-1
    (struct-copy state st
     [level lv]))
  (define f (format-math-tex+ st-1))
  (define contents (f (paren-contents p)))
  (define comp (lv . (level-compare ss) . cur-lv))
  (case comp
   [(<) contents]
   [else
    (define l (f (paren-left p)))
    (define r (f (paren-right p)))
    (math-tex% l contents r)]))

(define ((format-atom [st : State])
         [a : (Atom MathTeX+)]) : (Atom MathTeX)
  ((atom-map (format-math-tex+ st)) a))

(define ((format-sub-sup [st : State])
         [s : (SubSup (Atom MathTeX+) MathTeX+)]) : (SubSup (Atom MathTeX) MathTeX)
  (define st-1
    (struct-copy state st
     [level (level #f 0)]))
  (define st-2
    (struct-copy state st
     [level (level #t 0)]))
  ((sub-sup-map (format-atom st-1) (format-math-tex+ st-2)) s))

(define ((format-math-tex+ st) m)
  (define x (math-tex+-contents m))
  (cond
   [(atom? x) (math-tex ((format-atom st) x))]
   [(splice? x) (math-tex (splice-map (format-math-tex+ st) x))]
   [(paren? x) (math-tex% ((format-paren st) x))]
   [(sub-sup? x) (math-tex ((format-sub-sup st) x))]))
