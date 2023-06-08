#lang at-exp typed/racket

(require "../data/id.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../markup/tex.rkt"
         "../markup/splice.rkt")

(provide id->latex
         id->latex/margin)

(define (id->latex [i : Id]) : tex:TextTeX
  (define x (id-contents i))
  @text-tex%{@macro%["textcolor" @argument%{[@|x|]}]})

(define (id->latex/margin [i : Id]) : tex:TextTeX
  @text-tex%{@macro%["marginpar" @argument%{@(id->latex i)}]})
