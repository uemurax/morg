#lang at-exp typed/racket

(require "../data/tex.rkt"
         "../markup/string.rkt"
         "../util/escape.rkt")

(provide text-tex->text
         math-tex->text)

(define escape-text-tex
  (hash "#" "\\#"
        "$" "\\$"
        "%" "\\%"
        "&" "\\&"
        "_" "\\_"
        "{" "\\{"
        "}" "\\}"
        "~" "\\textasciitilde "
        "^" "\\textasciicircum "
        "\\" "\\textbackslash "))

(define escape-math-tex
  (hash "#" "\\#"
        "$" "\\$"
        "%" "\\%"
        "&" "\\&"
        "_" "\\_"
        "{" "\\{"
        "}" "\\}"
        "~" "\\~{}"
        "^" "\\^{}"
        "\\" "\\backslash "))

(define (text-tex->text:text [x : Text]) : StringTree
  @string~{@(escape escape-text-tex (text-contents x))})

(define (math-tex->text:text [x : Text]) : StringTree
  @string~{@(escape escape-math-tex (text-contents x))})

(define (macro->text [x : Macro]) : StringTree
  (define y (macro-contents x))
  (cond
   [(or (regexp-match-exact? #px"[[:alpha:]]+" y)
        (eq? (string-length y) 1))
    @string~{\@|y| }]
   [else
    @string~{\csname @|y|\endcsname }]))

(define #:forall (X)
        ((group->text [f : (X . -> . StringTree)])
         [x : (Group X)]) : StringTree
  @string~{{@(apply string~ (map f (group-contents x)))}})

(define (math->text [x : Math]) : StringTree
  @string~{\(@(math-tex->text (math-contents x))\)})

(define (sub-sup->text [x : SubSup]) : StringTree
  (define f math-tex->text)
  (define base (sub-sup-base x))
  (define sub (sub-sup-sub x))
  (define sup (sub-sup-sup x))
  (cond
   [(and sub sup)
    @string~{@(f base)_{@(f sub)}^{@(f sup)}}]
   [sub
    @string~{@(f base)_{@(f sub)}}]
   [sup
    @string~{@(f base)^{@(f sup)}}]
   [else (error "This must not happen.")]))

(define (text-tex->text [x : TextTeX]) : StringTree
  (define y (text-tex-contents x))
  (cond
   [(text? y) (text-tex->text:text y)]
   [(macro? y) (macro->text y)]
   [(group? y) ((group->text text-tex->text) y)]
   [(math? (math->text y))]
   [else (error "Unimplemented.")]))

(define (math-tex->text [x : MathTeX]) : StringTree
  (define y (math-tex-contents x))
  (cond
   [(text? y) (math-tex->text:text y)]
   [(macro? y) (macro->text y)]
   [(group? y) ((group->text math-tex->text) y)]
   [(sub-sup? y) (sub-sup->text y)]
   [else (error "Unimplemented.")]))
