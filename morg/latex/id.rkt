#lang at-exp typed/racket

(require "../data/id.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../markup/tex.rkt"
         "../markup/splice.rkt")

(provide id->latex
         id->latex/margin
         id->hypertarget
         id->hyperlink)

(define (id->latex [i : Id]) : tex:TextTeX
  (define x (id-contents i))
  @text-tex%{@macro%["textcolor" @argument%{gray} @argument%{@macro%["normalsize"]@macro%["texttt" @argument%{[@|x|]}]}]})

(define (id->latex/margin [i : Id]) : tex:TextTeX
  @text-tex%{@macro%["marginnote" @argument%{@(id->latex i)}]})

(define (id->label [i : Id]) : tex:TextTeX
  @text-tex%{morg-generated-@(id-contents i)})

(define (id->hypertarget [i : Id]) : tex:TextTeX
  @text-tex%{@macro%["hypertarget" @argument%{@(id->label i)} @argument%[""]]})

(define (id->hyperlink [i : Id] [x : TextTeXLike]) : tex:TextTeX
  @text-tex%{@macro%["hyperlink" @argument%{@(id->label i)} @argument%{@|x|}]})
