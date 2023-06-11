#lang at-exp typed/racket

(require "../data/tex.rkt"
         "../markup/string.rkt"
         "../markup/splice.rkt"
         "../data/splice.rkt"
         "splice.rkt"
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
  @string%{@(escape escape-text-tex (text-contents x))})

(define (math-tex->text:text [x : Text]) : StringTree
  @string%{@(escape escape-math-tex (text-contents x))})

(define #:forall (X)
        ((argument->text [f : (X . -> . StringTree)])
         [x : (Argument X)]) : StringTree
  (define body (argument-contents x))
  (define parens (argument-parentheses x))
  (define left (car parens))
  (define right (cdr parens))
  @string%{@|left|@(f body)@|right|})

(define #:forall (X)
        ((macro->text [f : (X . -> . StringTree)])
         [x : (Macro X)]) : StringTree
  (define y (macro-head x))
  (define args (macro-arguments x))
  (define head
    (cond
     [(or (regexp-match-exact? #px"[[:alpha:]]+" y)
          (eq? (string-length y) 1))
      @string%{\@|y|}]
     [else
      @string%{\csname @|y|\endcsname}]))
  @string%{@|head| @(apply % (map ((inst argument->text X) f) args))})

(define #:forall (X)
        ((group->text [f : (X . -> . StringTree)])
         [x : (Group X)]) : StringTree
  @string%{{@(f (group-contents x))}})

(define #:forall (X)
        ((atom->text [f : (X . -> . StringTree)]
                     [t : (Text . -> . StringTree)])
         [x : (Atom X)]) : StringTree
  (define y (atom-contents x))
  (cond
   [(text? y) (t y)]
   [(macro? y) ((macro->text f) y)]
   [(group? y) ((group->text f) y)]))

(define (math->text [x : Math]) : StringTree
  @string%{\(@(math-tex->text (math-contents x))\)})

(define #:forall (X)
        ((sub-sup->text [f : (X . -> . StringTree)])
         [x : (SubSup X)]) : StringTree
  (define base (sub-sup-base x))
  (define sub (sub-sup-sub x))
  (define sup (sub-sup-sup x))
  (define b ((atom->text f math-tex->text:text) base))
  (cond
   [(and sub sup)
    @string%{@|b|_{@(f sub)}^{@(f sup)}}]
   [sub
    @string%{@|b|_{@(f sub)}}]
   [sup
    @string%{@|b|^{@(f sup)}}]
   [else (error "This must not happen.")]))

(define (text-tex->text [x : TextTeX]) : StringTree
  (define y (text-tex-contents x))
  (cond
   [(atom? y)
    ((atom->text text-tex->text text-tex->text:text) y)]
   [(splice? y) ((splice->text text-tex->text) y)]
   [(math? y) (math->text y)]
   [else (error "Unimplemented.")]))

(define (math-tex->text [x : MathTeX]) : StringTree
  (define y (math-tex-contents x))
  (cond
   [(atom? y)
    ((atom->text math-tex->text math-tex->text:text) y)]
   [(splice? y) ((splice->text math-tex->text) y)]
   [(sub-sup? y) ((sub-sup->text math-tex->text) y)]
   [else (error "Unimplemented.")]))
